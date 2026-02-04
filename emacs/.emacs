(add-to-list 'exec-path "/Users/adam/.nix-profile/bin")
(setq custom-file "~/.dotfiles/emacs/emacs-custom.el")
(load custom-file 'noerror)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-deferred-compilation t)
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(global-set-key (kbd "C-SPC") nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

(set-face-attribute 'default nil :font "Hack-12")
(set-frame-font "Hack-12" nil t)
(scroll-bar-mode -1)

(use-package yaml-mode
  :mode (("\.yml?\'" . yaml-mode)))

(set-default 'truncate-lines t)

(setq make-backup-files nil)

(setq auto-save-default nil)

(setq inhibit-splash-screen t)

(use-package base16-theme
  :if (display-graphic-p)
  :config (load-theme 'base16-dracula t))

(use-package diminish
  :defer 1
  :config (progn
            (diminish 'buffer-face-mode)
            (diminish 'eldoc-mode)
            (diminish 'which-key-mode)
            (eval-after-load "eglot" '(diminish 'eglot--managed-mode "eglot"))
            (eval-after-load "autorevert" '(diminish 'auto-revert-mode))))

(defun project-find-file-other-window()
  "Use project-find-file but open in another split."
  (interactive)
  (let (buf (current-buffer))
    (switch-to-buffer-other-window (project-find-file))
    (switch-to-buffer buf)))

(defmacro simpson-make-neutral (map)
  "Create evil-style window movement for a given MAP."
  `(progn
     (define-key ,map (kbd "C-j") 'evil-window-down)
     (define-key ,map (kbd "C-k") 'evil-window-up)
     (define-key ,map (kbd "C-h") 'evil-window-left)
     (define-key ,map (kbd "C-l") 'evil-window-right)))

(defmacro simpson-make-neutral--keys (map)
  "Create evil-style  movement for a given MAP."
  `(progn
     (define-key ,map "j" 'next-line)
     (define-key ,map "k" 'previous-line)))

(use-package evil
  :diminish "vim"
  :init (setq evil-want-keybinding nil)
  :config (progn
            (evil-mode t)
            (setq-default evil-shift-width 2)
            (setq evil-vsplit-window-right t)
            (setq evil-split-window-below t)
            (evil-set-undo-system 'undo-redo)
            (add-to-list 'evil-emacs-state-modes 'dired-mode)
            (add-to-list 'evil-emacs-state-modes 'deadgrep-mode)
            (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
            (add-to-list 'evil-emacs-state-modes 'flymake-diagnostics-buffer-mode)
            (add-to-list 'evil-emacs-state-modes 'flymake-project-diagnostics-mode)
            (fset 'evil-visual-update-x-selection 'ignore)
            (simpson-make-neutral evil-normal-state-map)
            (define-key evil-window-map "=" 'balance-windows-area)
            (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
            (define-key evil-normal-state-map (kbd "C-b") 'project-switch-project)
            (define-key evil-normal-state-map (kbd "SPC SPC") 'simpson-other-project-window)
            (define-key evil-normal-state-map (kbd "C-c C-p") 'project-find-file)
            (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
            (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)
            (add-hook 'post-command-hook 'simpson-evil-mode)))

(defun eglot-go-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook (lambda ()
                                (call-interactively 'eglot-code-action-organize-imports))
            nil t))

(use-package go-mode
  :hook ((go-mode . eglot-ensure)
         (go-mode . eglot-go-save-hooks)))

(use-package eglot
  :config
  (setq eglot-autoshutdown t)  ; shutdown server when last buffer closed
  (evil-define-key 'normal eglot-mode-map (kbd "K") 'eldoc-doc-buffer)
  (setq eglot-events-buffer-size 0))  ; disable events logging for performance

(use-package evil-leader
  :after evil
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (evil-leader/set-key "c" 'display-fill-column-indicator-mode)
            (evil-leader/set-key "v" 'simpson-vert-split)
            (evil-leader/set-key "f" 'deadgrep)
            (evil-leader/set-key "b" 'simpson-other-buffer-window)
            (evil-leader/set-key "r" 'xref-find-references)
            (evil-leader/set-key "d" 'xref-find-definitions)
            (evil-leader/set-key "p" 'flymake-show-buffer-diagnostics)
            (evil-leader/set-key "P" 'flymake-show-project-diagnostics)
            (evil-leader/set-key "x" 'simpson-horizontal-split)))

(defun simpson-vert-split()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun simpson-horizontal-split()
  (interactive)
  (split-window-vertically)
  (evil-window-down 1))

(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)

(defun simpson-copy-file-buffer-name()
  "Copy the file path for the current buffer to the clipboard."
  (interactive)
  (let* ((path (buffer-file-name))
         (relative (file-relative-name path (locate-dominating-file path ".git"))))
    (if (y-or-n-p "Code formatting? ")
        (kill-new (concat "`" relative "`"))
      (kill-new relative))))

(defun simpson-other-project-window()
  (interactive)
  (let ((f (completing-read
            "Project file:"
            (project--files-in-directory
             (project-root (project-current t)) vc-directory-exclusion-list))))
    (split-window-right)
    (evil-window-right 1)
    (find-file f)
    (balance-windows)))

(defun simpson-other-buffer-window()
  (interactive)
  (let ((buf (read-buffer "Switch to: ")))
    (if (>= (count-windows) 2)
        (progn
          (split-window-right)
          (evil-window-right 1)
          (switch-to-buffer buf)
          (balance-windows))
      (switch-to-buffer-other-window buf))))

(use-package key-chord
  :config (progn
            (key-chord-mode 1)
            (setq key-chord-two-keys-delay 0.1)
            (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
            (key-chord-define evil-normal-state-map "//" 'comment-region)
            (key-chord-define evil-normal-state-map "??" 'uncomment-region)))

(use-package which-key
  :diminish ""
  :config (which-key-mode))

(use-package direnv
  :after dash
  :config (progn
            (direnv-mode)
            (setq direnv-always-show-summary nil)
            (advice-add 'eglot :before #'direnv-update-environment)))

(use-package company
  :hook (eglot-managed-mode . company-mode)
  :diminish ""
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package deadgrep
  :after (s f)
  :commands deadgrep)

(add-hook 'prog-mode-hook (lambda()
                            (display-line-numbers-mode)
                            (setq display-line-numbers 'relative)))

(set-face-background 'line-number nil)

(pixel-scroll-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(defmacro json-parse! (buffer)
  "Parse and return JSON from BUFFER.  Ideally for the 'url-retrieve' family of funcs."
  `(with-current-buffer ,buffer (json-read-from-string (buffer-substring-no-properties url-http-end-of-headers (point-max)))))

(defun btc()
  "Get BTC price from coinbase API via synchronous url retrieve."
  (interactive)
  (let ((data (json-parse! (url-retrieve-synchronously "https://api.coinbase.com/v2/prices/BTC-USD/spot" t))))
    (message "BTC: $%s" (alist-get 'amount (car data)))))

(eval-after-load "flymake" '(diminish 'flymake-mode "fly"))
(eval-after-load "flymake" '(simpson-make-neutral--keys flymake-diagnostics-buffer-mode-map))
(eval-after-load "flymake" '(simpson-make-neutral flymake-diagnostics-buffer-mode-map))
(eval-after-load "flymake" '(simpson-make-neutral--keys flymake-project-diagnostics-mode-map))
(eval-after-load "flymake" '(simpson-make-neutral flymake-project-diagnostics-mode-map))
(eval-after-load "xref" '(simpson-make-neutral xref--xref-buffer-mode-map))
(eval-after-load "xref" '(simpson-make-neutral--keys xref--xref-buffer-mode-map))

(use-package diminish
  :defer 1
  :config (progn
            (diminish 'buffer-face-mode)
            (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
            ;; Add these:
            (diminish 'eldoc-mode)
            (diminish 'which-key-mode)))

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package dape
  :config (add-to-list 'dape-configs
                       '(dlv port 55878 :request "attach" :mode "remote")))

(use-package diff-hl
  :demand t
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (evil-define-key 'normal 'global (kbd "] c") 'diff-hl-next-hunk)
  (evil-define-key 'normal 'global (kbd "[ c") 'diff-hl-previous-hunk))

(defun my/dlv-attach-by-name ()
  "Attach dlv to a process selected via completion."
  (interactive)
  (let* ((procs (split-string (shell-command-to-string "ps -eo pid,args") "\n" t))
         (selection (completing-read "Process: " (cdr procs) nil t)) ; skip header
         (pid (string-to-number (car (split-string (string-trim selection))))))
    (my/dlv-attach pid)))

(defun my/dlv-attach (pid)
  "Attach dlv to PID and start dape."
  (interactive "nPID: ")
  (let ((default-directory (project-root (project-current t))))
    (start-process "dlv" "dlv" "dlv" "attach" (number-to-string pid)
                   "--headless" "--listen" "127.0.0.1:55878"))
  (sleep-for 1)
  (call-interactively 'dape))

(defun simpson-pretty-lambda()
  "Make the word lambda the greek character in elisp files."
  (setq prettify-symbols-alist '(("lambda" . 955))))

(use-package aggressive-indent
  :init (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  :mode("\.lisp?\'" . aggressive-indent-mode))

(use-package elisp-mode
  :ensure nil
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'simpson-pretty-lambda)
          (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
          (add-hook 'emacs-lisp-mode-hook 'company-mode)
          (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "λ")))))

(fido-vertical-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-SPC k !") 'simpson-restore-text)
(global-set-key (kbd "C-SPC t") 'tab-bar-switch-to-tab)
(global-set-key (kbd "C-SPC c") 'tab-bar-new-tab)
(global-set-key (kbd "C-SPC d") 'tab-bar-close-tab)
(global-set-key (kbd "C-SPC r") 'tab-bar-rename-tab)

(defun simpson-restore-text()
  "Reset text size to default."
  (interactive)
  (text-scale-set 0))

(use-package magit
  :bind ("C-SPC g" . magit-status)
  :config (progn
            (setq auto-revert-buffer-list-filter nil)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (put 'magit-clean 'disabled nil)
            (setq magit-log-section-commit-count 0)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)
            (set-face-background 'magit-diff-hunk-heading-highlight "DarkMagenta")
            (set-face-background 'magit-diff-hunk-heading "DarkCyan")))

(setq-default mode-line-format nil)

(use-package shrink-path
  :config (progn
            (setq-default mode-line-format (list
                                            " "
                                            '(:eval (when (buffer-file-name)
                                                      (propertize (shrink-path-file (buffer-file-name) t) 'help-echo (buffer-file-name))))
                                            '(:eval (when (buffer-modified-p)
                                                      (propertize "*" 'face '(:foreground "#cd5c5c"))))
                                            " "
                                            '(:eval mode-line-position)
                                            " "
                                            mode-line-modes
                                            mode-line-misc-info
                                            '(:eval (simpson-current-tab))))))

(defun simpson-current-tab ()
  "Return current tab"
  (concat "[" (cdr (assq 'name (cdr (tab-bar--current-tab)))) "]"))

(use-package gotest)

(use-package jest-test-mode
  :diminish "jest"
  :hook ((typescript-ts-mode . jest-test-mode)
         (tsx-ts-mode . jest-test-mode)))

(use-package apheleia
  :diminish "fmt"
  :hook ((typescript-ts-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . (lambda () (setq mode-name "ts")))
         (tsx-ts-mode . (lambda () (setq mode-name "tsx")))))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package editorconfig
  :diminish ""
  :config (editorconfig-mode 1))

(use-package scss-mode
  :mode "\\.scss\\'")

(defun simpson-evil-mode ()
  "Change mode line color based on evil state."
  (cond
   ((evil-insert-state-p) (set-face-attribute 'mode-line nil :background "#ea51b2" :foreground "white" :box '(:line-width 5 :color "#ea51b2")))
   ((evil-normal-state-p) (modeline-theme))))

(set-face-background 'fringe nil)

(defun modeline-theme()
  (when (string= (car custom-enabled-themes) "base16-dracula")
    (set-face-attribute 'header-line nil
                        :background (plist-get base16-dracula-theme-colors :base00)
                        :box `(:color ,(plist-get base16-dracula-theme-colors :base00)))
    (set-face-attribute 'mode-line nil
                        :background (plist-get base16-dracula-theme-colors :base0D)
                        :foreground (plist-get base16-dracula-theme-colors :base01)
                        :box `(:line-width 5 :color ,(plist-get base16-dracula-theme-colors :base0D) :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background (plist-get base16-dracula-theme-colors :base01)
                        :foreground (plist-get base16-dracula-theme-colors :base06)
                        :box `(:line-width 5 :color ,(plist-get base16-dracula-theme-colors :base01) :style nil))))
(modeline-theme)
