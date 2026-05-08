package main

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"log"
	"mime"
	"mime/multipart"
	"mime/quotedprintable"
	"net"
	"net/mail"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"
	"time"
)

const (
	hashVersion  = "v1"
	hashHexChars = 48 // 192 bits, keeps local-part safely below SMTP 64-char limit
)

var replyCutoffPatterns = []*regexp.Regexp{
	regexp.MustCompile(`(?i)^on .+wrote:$`),
	regexp.MustCompile(`(?i)^from:\s`),
	regexp.MustCompile(`(?i)^sent:\s`),
	regexp.MustCompile(`(?i)^subject:\s`),
	regexp.MustCompile(`(?i)^to:\s`),
	regexp.MustCompile(`(?i)^cc:\s`),
	regexp.MustCompile(`(?i)^-----original message-----$`),
}

type config struct {
	listenAddr    string
	sessionRoot   string
	piBinary      string
	hashLength    int
	routingPrefix string
	promptTimeout time.Duration
	quitTimeout   time.Duration
}

type sessionHeader struct {
	Type string `json:"type"`
	ID   string `json:"id"`
	Cwd  string `json:"cwd"`
}

type sessionMatch struct {
	ID    string
	Cwd   string
	Path  string
	MTime time.Time
}

type rpcEnvelope struct {
	Type    string `json:"type"`
	Command string `json:"command,omitempty"`
	Success *bool  `json:"success,omitempty"`
	Error   string `json:"error,omitempty"`
}

type sessionLocks struct {
	mu    sync.Mutex
	locks map[string]*sync.Mutex
}

func newSessionLocks() *sessionLocks {
	return &sessionLocks{locks: map[string]*sync.Mutex{}}
}

func (l *sessionLocks) lock(key string) func() {
	l.mu.Lock()
	m := l.locks[key]
	if m == nil {
		m = &sync.Mutex{}
		l.locks[key] = m
	}
	l.mu.Unlock()

	m.Lock()
	return m.Unlock
}

type receiver struct {
	cfg   config
	locks *sessionLocks
}

func main() {
	cfg := config{}
	flag.StringVar(&cfg.listenAddr, "listen", "0.0.0.0:2525", "SMTP listen address")
	flag.StringVar(&cfg.sessionRoot, "session-root", "/home/agent/.pi/agent/sessions", "pi session root directory")
	flag.StringVar(&cfg.piBinary, "pi-bin", "pi", "path to pi binary")
	flag.IntVar(&cfg.hashLength, "hash-length", hashHexChars, "number of hex chars from SHA256 hash")
	flag.StringVar(&cfg.routingPrefix, "routing-prefix", "patches+", "recipient local-part prefix used for routing")
	flag.DurationVar(&cfg.promptTimeout, "prompt-timeout", 10*time.Minute, "max time allowed for one pi RPC prompt")
	flag.DurationVar(&cfg.quitTimeout, "quit-timeout", 10*time.Second, "time to wait for pi RPC process to exit after stdin close")
	flag.Parse()

	if cfg.hashLength <= 0 || cfg.hashLength > 64 {
		log.Fatalf("invalid --hash-length=%d (must be between 1 and 64)", cfg.hashLength)
	}

	ln, err := net.Listen("tcp", cfg.listenAddr)
	if err != nil {
		log.Fatalf("failed to listen on %s: %v", cfg.listenAddr, err)
	}
	defer ln.Close()

	r := &receiver{cfg: cfg, locks: newSessionLocks()}
	log.Printf("pi mail receiver listening on %s", cfg.listenAddr)

	for {
		conn, err := ln.Accept()
		if err != nil {
			log.Printf("accept error: %v", err)
			continue
		}
		go r.handleConn(conn)
	}
}

func (r *receiver) handleConn(conn net.Conn) {
	defer conn.Close()

	reader := bufio.NewReader(conn)
	writer := bufio.NewWriter(conn)
	flushLine := func(format string, args ...any) error {
		if _, err := fmt.Fprintf(writer, format+"\r\n", args...); err != nil {
			return err
		}
		return writer.Flush()
	}

	if err := flushLine("220 pi-mail-receiver ESMTP"); err != nil {
		return
	}

	var mailFrom string
	var rcptTo []string

	for {
		if err := conn.SetReadDeadline(time.Now().Add(2 * time.Minute)); err != nil {
			return
		}
		line, err := reader.ReadString('\n')
		if err != nil {
			if !errors.Is(err, io.EOF) {
				log.Printf("smtp read error from %s: %v", conn.RemoteAddr(), err)
			}
			return
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			continue
		}

		cmd, arg := splitCommand(line)
		switch cmd {
		case "EHLO", "HELO":
			if err := flushLine("250 pi-mail-receiver"); err != nil {
				return
			}
		case "MAIL":
			from, perr := parsePathArg(arg, "FROM:")
			if perr != nil {
				_ = flushLine("501 5.5.2 %s", perr.Error())
				continue
			}
			mailFrom = from
			rcptTo = rcptTo[:0]
			if err := flushLine("250 2.1.0 OK"); err != nil {
				return
			}
		case "RCPT":
			rcpt, perr := parsePathArg(arg, "TO:")
			if perr != nil {
				_ = flushLine("501 5.5.2 %s", perr.Error())
				continue
			}
			if !recipientHasRoutingHash(rcpt, r.cfg.routingPrefix) {
				_ = flushLine("550 5.1.1 unsupported recipient")
				continue
			}
			rcptTo = append(rcptTo, rcpt)
			if err := flushLine("250 2.1.5 OK"); err != nil {
				return
			}
		case "DATA":
			if len(rcptTo) == 0 {
				_ = flushLine("503 5.5.1 need RCPT before DATA")
				continue
			}
			if err := flushLine("354 End data with <CR><LF>.<CR><LF>"); err != nil {
				return
			}

			payload, derr := readDataBlock(reader)
			if derr != nil {
				log.Printf("smtp DATA read failed from %s: %v", conn.RemoteAddr(), derr)
				_ = flushLine("451 4.3.0 failed to read DATA")
				mailFrom = ""
				rcptTo = rcptTo[:0]
				continue
			}

			if err := r.processIncoming(mailFrom, rcptTo, payload); err != nil {
				log.Printf("message rejected: from=%q rcpt=%v err=%v", mailFrom, rcptTo, err)
				_ = flushLine("451 4.3.0 %s", sanitizeSMTPError(err.Error()))
			} else {
				if err := flushLine("250 2.0.0 accepted"); err != nil {
					return
				}
			}

			mailFrom = ""
			rcptTo = rcptTo[:0]
		case "RSET":
			mailFrom = ""
			rcptTo = rcptTo[:0]
			if err := flushLine("250 2.0.0 reset"); err != nil {
				return
			}
		case "NOOP":
			if err := flushLine("250 2.0.0 OK"); err != nil {
				return
			}
		case "QUIT":
			_ = flushLine("221 2.0.0 bye")
			return
		default:
			if err := flushLine("502 5.5.2 unsupported command"); err != nil {
				return
			}
		}
	}
}

func (r *receiver) processIncoming(mailFrom string, rcptTo []string, payload []byte) error {
	routingHash, err := extractRoutingHash(rcptTo, r.cfg.routingPrefix)
	if err != nil {
		return err
	}

	session, err := r.findSessionByHash(routingHash)
	if err != nil {
		return err
	}

	prompt, err := extractPrompt(payload)
	if err != nil {
		return err
	}

	unlock := r.locks.lock(session.Path)
	defer unlock()

	log.Printf("dispatching reply from=%q session=%s cwd=%s prompt-bytes=%d", mailFrom, session.ID, session.Cwd, len(prompt))
	if err := r.sendPromptToPi(session, prompt); err != nil {
		return err
	}

	return nil
}

func (r *receiver) findSessionByHash(target string) (*sessionMatch, error) {
	target = strings.ToLower(strings.TrimSpace(target))
	if target == "" {
		return nil, errors.New("empty routing hash")
	}

	matches := make([]sessionMatch, 0, 1)
	walkErr := filepath.WalkDir(r.cfg.sessionRoot, func(path string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return nil
		}
		if d.IsDir() || !strings.HasSuffix(strings.ToLower(d.Name()), ".jsonl") {
			return nil
		}

		header, err := readSessionHeader(path)
		if err != nil {
			return nil
		}
		if header.Type != "session" || header.ID == "" || header.Cwd == "" {
			return nil
		}

		canonCwd := canonicalPath(header.Cwd)
		h := computeSessionHash(header.ID, canonCwd, r.cfg.hashLength)
		if h != target {
			return nil
		}

		info, err := d.Info()
		if err != nil {
			return nil
		}
		matches = append(matches, sessionMatch{
			ID:    header.ID,
			Cwd:   canonCwd,
			Path:  path,
			MTime: info.ModTime(),
		})
		return nil
	})
	if walkErr != nil {
		return nil, fmt.Errorf("failed walking session root: %w", walkErr)
	}
	if len(matches) == 0 {
		return nil, fmt.Errorf("no session matched hash %q", target)
	}

	sort.Slice(matches, func(i, j int) bool { return matches[i].MTime.After(matches[j].MTime) })
	match := matches[0]
	return &match, nil
}

func (r *receiver) sendPromptToPi(session *sessionMatch, prompt string) error {
	ctx, cancel := context.WithTimeout(context.Background(), r.cfg.promptTimeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, r.cfg.piBinary, "--mode", "rpc", "--session", session.Path)
	cmd.Dir = session.Cwd

	stdin, err := cmd.StdinPipe()
	if err != nil {
		return fmt.Errorf("failed to open pi stdin: %w", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("failed to open pi stdout: %w", err)
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr

	if err := cmd.Start(); err != nil {
		return fmt.Errorf("failed to start pi RPC: %w", err)
	}

	enc := json.NewEncoder(stdin)
	if err := enc.Encode(map[string]any{
		"id":      "reply-mail",
		"type":    "prompt",
		"message": prompt,
	}); err != nil {
		_ = stdin.Close()
		_ = killAndWait(cmd)
		return fmt.Errorf("failed to send RPC prompt: %w", err)
	}

	scanner := bufio.NewScanner(stdout)
	scanner.Buffer(make([]byte, 64*1024), 10*1024*1024)

	promptAccepted := false
	for scanner.Scan() {
		line := scanner.Bytes()
		var evt rpcEnvelope
		if err := json.Unmarshal(line, &evt); err != nil {
			continue
		}

		switch evt.Type {
		case "response":
			if evt.Command != "prompt" {
				continue
			}
			if evt.Success != nil && *evt.Success {
				promptAccepted = true
				continue
			}
			_ = stdin.Close()
			_ = killAndWait(cmd)
			errMsg := strings.TrimSpace(evt.Error)
			if errMsg == "" {
				errMsg = "prompt rejected"
			}
			return errors.New(errMsg)
		case "agent_end":
			if !promptAccepted {
				continue
			}
			_ = stdin.Close()
			if err := waitWithTimeout(cmd, r.cfg.quitTimeout); err != nil {
				return fmt.Errorf("pi did not exit cleanly: %w (stderr: %s)", err, strings.TrimSpace(stderr.String()))
			}
			return nil
		}
	}

	if err := scanner.Err(); err != nil {
		_ = stdin.Close()
		_ = killAndWait(cmd)
		return fmt.Errorf("failed reading pi RPC stream: %w", err)
	}

	_ = stdin.Close()
	if ctx.Err() != nil {
		_ = killAndWait(cmd)
		return fmt.Errorf("pi RPC timed out: %w", ctx.Err())
	}
	if err := waitWithTimeout(cmd, r.cfg.quitTimeout); err != nil {
		return fmt.Errorf("pi exited unexpectedly: %w (stderr: %s)", err, strings.TrimSpace(stderr.String()))
	}
	if !promptAccepted {
		return errors.New("pi RPC ended before prompt acknowledgement")
	}
	return errors.New("pi RPC ended before agent completion")
}

func waitWithTimeout(cmd *exec.Cmd, timeout time.Duration) error {
	done := make(chan error, 1)
	go func() {
		done <- cmd.Wait()
	}()

	select {
	case err := <-done:
		return err
	case <-time.After(timeout):
		_ = cmd.Process.Kill()
		return <-done
	}
}

func killAndWait(cmd *exec.Cmd) error {
	if cmd.Process != nil {
		_ = cmd.Process.Kill()
	}
	return cmd.Wait()
}

func extractRoutingHash(recipients []string, prefix string) (string, error) {
	var found string
	for _, rcpt := range recipients {
		local := localPart(normalizeAddress(rcpt))
		if local == "" {
			continue
		}
		if !strings.HasPrefix(strings.ToLower(local), strings.ToLower(prefix)) {
			continue
		}
		hash := strings.ToLower(strings.TrimSpace(local[len(prefix):]))
		if hash == "" {
			continue
		}
		if found == "" {
			found = hash
			continue
		}
		if found != hash {
			return "", fmt.Errorf("multiple routing hashes in recipients: %q vs %q", found, hash)
		}
	}
	if found == "" {
		return "", errors.New("no routing hash found in recipients")
	}
	return found, nil
}

func extractPrompt(raw []byte) (string, error) {
	msg, err := mail.ReadMessage(bytes.NewReader(raw))
	if err != nil {
		return "", fmt.Errorf("invalid mail payload: %w", err)
	}

	body, err := io.ReadAll(msg.Body)
	if err != nil {
		return "", fmt.Errorf("failed reading mail body: %w", err)
	}

	text, err := extractTextPlain(msg.Header, body)
	if err != nil {
		return "", err
	}

	text = normalizeNewlines(text)
	cleaned := stripQuotedReply(text)
	if cleaned == "" {
		cleaned = strings.TrimSpace(text)
	}
	if cleaned == "" {
		return "", errors.New("mail body is empty after quote stripping")
	}
	return cleaned, nil
}

func extractTextPlain(header mail.Header, body []byte) (string, error) {
	contentType := strings.TrimSpace(header.Get("Content-Type"))
	if contentType == "" {
		return decodeTransfer(body, header.Get("Content-Transfer-Encoding"))
	}

	mediaType, params, err := mime.ParseMediaType(contentType)
	if err != nil {
		return decodeTransfer(body, header.Get("Content-Transfer-Encoding"))
	}
	mediaType = strings.ToLower(mediaType)

	if strings.HasPrefix(mediaType, "multipart/") {
		boundary := params["boundary"]
		if boundary == "" {
			return "", errors.New("multipart message without boundary")
		}
		mr := multipart.NewReader(bytes.NewReader(body), boundary)
		for {
			part, err := mr.NextPart()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return "", fmt.Errorf("failed reading multipart payload: %w", err)
			}

			partBytes, err := io.ReadAll(part)
			if err != nil {
				return "", fmt.Errorf("failed reading MIME part: %w", err)
			}
			partMediaType := strings.ToLower(part.Header.Get("Content-Type"))
			if partMediaType == "" {
				partMediaType = "text/plain"
			}
			if mt, _, err := mime.ParseMediaType(partMediaType); err == nil {
				partMediaType = strings.ToLower(mt)
			}
			if partMediaType != "text/plain" {
				continue
			}
			return decodeTransfer(partBytes, part.Header.Get("Content-Transfer-Encoding"))
		}
		return "", errors.New("multipart message did not contain text/plain part")
	}

	if mediaType != "text/plain" {
		return "", fmt.Errorf("unsupported content type: %s", mediaType)
	}
	return decodeTransfer(body, header.Get("Content-Transfer-Encoding"))
}

func decodeTransfer(data []byte, encoding string) (string, error) {
	switch strings.ToLower(strings.TrimSpace(encoding)) {
	case "", "7bit", "8bit", "binary":
		return string(data), nil
	case "quoted-printable":
		decoded, err := io.ReadAll(quotedprintable.NewReader(bytes.NewReader(data)))
		if err != nil {
			return "", fmt.Errorf("failed quoted-printable decode: %w", err)
		}
		return string(decoded), nil
	case "base64":
		decoded, err := io.ReadAll(base64.NewDecoder(base64.StdEncoding, bytes.NewReader(data)))
		if err != nil {
			return "", fmt.Errorf("failed base64 decode: %w", err)
		}
		return string(decoded), nil
	default:
		// Unknown transfer encoding: keep best-effort raw bytes.
		return string(data), nil
	}
}

func stripQuotedReply(text string) string {
	lines := strings.Split(text, "\n")
	kept := make([]string, 0, len(lines))

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if isReplyCutoff(trimmed) {
			break
		}
		if strings.HasPrefix(trimmed, ">") {
			continue
		}
		kept = append(kept, line)
	}

	for len(kept) > 0 && strings.TrimSpace(kept[len(kept)-1]) == "" {
		kept = kept[:len(kept)-1]
	}

	return strings.TrimSpace(strings.Join(kept, "\n"))
}

func isReplyCutoff(line string) bool {
	if line == "" {
		return false
	}
	for _, re := range replyCutoffPatterns {
		if re.MatchString(line) {
			return true
		}
	}
	return false
}

func computeSessionHash(sessionID, cwd string, length int) string {
	payload := hashVersion + "\x00" + sessionID + "\x00" + cwd
	sum := sha256.Sum256([]byte(payload))
	hexHash := hex.EncodeToString(sum[:])
	if length > 0 && length < len(hexHash) {
		return hexHash[:length]
	}
	return hexHash
}

func readSessionHeader(path string) (*sessionHeader, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	r := bufio.NewReader(f)
	line, err := r.ReadBytes('\n')
	if err != nil && !errors.Is(err, io.EOF) {
		return nil, err
	}
	line = bytes.TrimSpace(line)
	if len(line) == 0 {
		return nil, errors.New("empty session header")
	}

	var h sessionHeader
	if err := json.Unmarshal(line, &h); err != nil {
		return nil, err
	}
	return &h, nil
}

func canonicalPath(path string) string {
	clean := filepath.Clean(path)
	if abs, err := filepath.Abs(clean); err == nil {
		clean = abs
	}
	if resolved, err := filepath.EvalSymlinks(clean); err == nil {
		clean = resolved
	}
	return clean
}

func recipientHasRoutingHash(rawRecipient, prefix string) bool {
	local := localPart(normalizeAddress(rawRecipient))
	if local == "" {
		return false
	}
	if !strings.HasPrefix(strings.ToLower(local), strings.ToLower(prefix)) {
		return false
	}
	return strings.TrimSpace(local[len(prefix):]) != ""
}

func normalizeAddress(raw string) string {
	addr := strings.TrimSpace(raw)
	if idx := strings.IndexAny(addr, " \t"); idx >= 0 {
		addr = addr[:idx]
	}
	addr = strings.TrimSpace(strings.Trim(addr, "<>"))
	return strings.ToLower(addr)
}

func localPart(address string) string {
	if address == "" {
		return ""
	}
	parts := strings.SplitN(address, "@", 2)
	return parts[0]
}

func parsePathArg(arg, prefix string) (string, error) {
	if !strings.HasPrefix(strings.ToUpper(arg), prefix) {
		return "", fmt.Errorf("expected %s<address>", strings.ToLower(prefix))
	}
	raw := strings.TrimSpace(arg[len(prefix):])
	if raw == "" {
		return "", errors.New("missing address")
	}

	if strings.HasPrefix(raw, "<") {
		end := strings.Index(raw, ">")
		if end < 0 {
			return "", errors.New("unterminated angle-bracket address")
		}
		return raw[:end+1], nil
	}

	fields := strings.Fields(raw)
	if len(fields) == 0 {
		return "", errors.New("missing address")
	}
	return fields[0], nil
}

func splitCommand(line string) (string, string) {
	line = strings.TrimSpace(line)
	if line == "" {
		return "", ""
	}

	idx := strings.IndexByte(line, ' ')
	if idx < 0 {
		return strings.ToUpper(line), ""
	}
	return strings.ToUpper(line[:idx]), strings.TrimSpace(line[idx+1:])
}

func readDataBlock(r *bufio.Reader) ([]byte, error) {
	var buf bytes.Buffer
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			return nil, err
		}
		if line == ".\r\n" || line == ".\n" {
			break
		}
		if strings.HasPrefix(line, "..") {
			line = line[1:]
		}
		buf.WriteString(line)
	}
	return buf.Bytes(), nil
}

func normalizeNewlines(s string) string {
	s = strings.ReplaceAll(s, "\r\n", "\n")
	s = strings.ReplaceAll(s, "\r", "\n")
	return s
}

func sanitizeSMTPError(message string) string {
	message = strings.TrimSpace(message)
	message = strings.ReplaceAll(message, "\r", " ")
	message = strings.ReplaceAll(message, "\n", " ")
	if message == "" {
		return "internal error"
	}
	return message
}
