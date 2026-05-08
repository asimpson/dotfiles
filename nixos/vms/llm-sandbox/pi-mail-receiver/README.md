# pi-mail-receiver

Small SMTP listener used inside the llm-sandbox VM.

It receives reply mail from the host, extracts `patches+<hash>@localhost` from
the SMTP recipient, resolves the matching pi session by scanning
`~/.pi/agent/sessions`, and then sends the reply body as a prompt via `pi --mode rpc`.

## Routing hash

`sendPatch` computes:

```text
hash = SHA256("v1\0<session-id>\0<canonical-cwd>")
```

and uses the first 48 hex chars in:

```text
Reply-To: patches+<hash>@localhost
```

The receiver uses the same function and matches by recomputation (no state DB).

## Runtime config

Configured via systemd in `configuration.nix`:

- listen: `0.0.0.0:2525`
- session root: `/home/agent/.pi/agent/sessions`
- pi binary: `/home/agent/.npm-global/bin/pi`
