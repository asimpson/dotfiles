;;; .emacs --- My Emacs customizations -*- lexical-binding: t; -*-
(setq native-comp-deferred-compilation t)
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(setq custom-file "~/.dotfiles/emacs/emacs-custom.el")
(add-to-list 'load-path "~/.dotfiles/emacs/")
(defvar simpson-evil t
  "Variable to enable or disable evil specific configurations.")
(setq idle-update-delay 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
 (require 'use-package))

(setq native-comp-async-report-warnings-errors 'silent)

(setq-default
 use-package-always-defer t
 use-package-always-ensure t)

(tool-bar-mode -1)
(server-start)
(menu-bar-mode -1)
(show-paren-mode)

(if (equal system-type 'darwin)
    (scroll-bar-mode "right")
    (scroll-bar-mode -1))

(setq visible-bell nil)

(and (version<= "28.2" emacs-version) (not (equal system-type 'darwin))
      (progn
        (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
        (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)))

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(global-set-key (kbd "C-SPC") nil)

(defvar simpson-dropbox-path ""
  "Variable that points to the proper Dropbox path.")

(cond
  ((file-exists-p "~/notes/") (setq simpson-dropbox-path "~/notes/"))
  ((file-exists-p "~/Dropbox (Personal)/") (setq simpson-dropbox-path "~/Dropbox (Personal)/"))
  ((file-exists-p "~/Dropbox/") (setq simpson-dropbox-path "~/Dropbox/"))
  ((file-exists-p "/c/Users/Adam/Dropbox/") (setq simpson-dropbox-path "/c/Users/Adam/Dropbox/")) )

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

(defmacro simpson-load-file (file)
  "Check if FILE exists and if it does load it."
  `(when (file-exists-p ,file)
     (load-library ,file)))

(use-package base16-theme
             :if (display-graphic-p)
             :init (load-theme 'base16-dracula t))

(use-package exec-path-from-shell
             :defer 2
             :config (progn
                       (when (memq window-system '(mac ns x))
                         (exec-path-from-shell-initialize))))

(use-package dired-narrow
  :defer 1
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-subtree
  :defer 1
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle))
  :config(progn
           (set-face-foreground 'dired-subtree-depth-1-face "white")
           (set-face-foreground 'dired-subtree-depth-2-face "white")
           (set-face-foreground 'dired-subtree-depth-3-face "white")
           (set-face-foreground 'dired-subtree-depth-4-face "white")
           (set-face-foreground 'dired-subtree-depth-5-face "white")
           (set-face-foreground 'dired-subtree-depth-6-face "white")))

(use-package vimish-fold
  :defer 1
  :config (vimish-fold-global-mode 1))

(use-package flycheck
             :diminish "lint"
             :defer 1
             :bind ("C-SPC '" . flycheck-mode)
             :config (progn
                       (setq flycheck-global-modes '(sh-mode rjsx-mode emacs-lisp-mode rust-mode ruby-mode common-lisp-mode))
                       (global-flycheck-mode)
                       ;;https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
                       (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))))

(defun project-find-file-other-window()
  "Use project-find-file but open in another split."
  (interactive)
  (let (buf (current-buffer))
    (switch-to-buffer-other-window (project-find-file))
    (switch-to-buffer buf)))

(use-package evil
             :if simpson-evil
             :diminish "vim"
             :defer 1
             :init (setq evil-want-keybinding nil)
             :config (progn
                       (evil-mode t)
                       (setq-default evil-shift-width 2)
                       (setq evil-vsplit-window-right t)
                       (setq evil-split-window-below t)
                       (evil-set-undo-system 'undo-redo)
                       (add-to-list 'evil-emacs-state-modes 'dired-mode)
                       (add-to-list 'evil-emacs-state-modes 'simple-mpc-mode)
                       (add-to-list 'evil-emacs-state-modes 'epa-key-list-mode)
                       (add-to-list 'evil-emacs-state-modes 'ivy-occur-mode)
                       (add-to-list 'evil-emacs-state-modes 'image-mode)
                       (add-to-list 'evil-emacs-state-modes 'comint-mode)
                       (add-to-list 'evil-emacs-state-modes 'eww-mode)
                       (add-to-list 'evil-emacs-state-modes 'sql-interactive-mode)
                       (add-to-list 'evil-emacs-state-modes 'deadgrep-mode)
                       (add-to-list 'evil-emacs-state-modes 'epresent-mode)
                       (add-to-list 'evil-emacs-state-modes 'slime-repl-mode)
                       (evil-set-initial-state 'slime-repl-mode 'emacs)
                       (add-to-list 'evil-emacs-state-modes 'sldb-mode)
                       ;;http://spacemacs.org/doc/FAQ#orgheadline31
                       (fset 'evil-visual-update-x-selection 'ignore)
                       (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
                       (simpson-make-neutral evil-normal-state-map)
                       (define-key evil-normal-state-map (kbd "gx") 'browse-url)
                       (define-key evil-normal-state-map (kbd "SPC SPC") 'project-find-file-other-window)
                       (define-key evil-normal-state-map (kbd "C-c C-p") 'project-find-file)
                       (define-key evil-normal-state-map (kbd "C-b") 'project-switch-project)
                       (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
                       (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)
                       (define-key evil-normal-state-map (kbd "C-b") 'project-switch-project)
                       (add-hook 'post-command-hook 'simpson-evil-mode)))

(eval-after-load 'image-mode (lambda() (simpson-make-neutral image-mode-map)))

(use-package evil-leader
             :if simpson-evil
             :after evil
             :defer 1
             :config (progn
                       (global-evil-leader-mode)
                       (evil-leader/set-leader ",")
                       (evil-leader/set-key "c" 'display-fill-column-indicator-mode)
                       (evil-leader/set-key "t" 'hydra-mocha/body)
                       (evil-leader/set-key "v" 'simpson-vert-split)
                       (evil-leader/set-key "f" 'counsel-rg)
                       (evil-leader/set-key "x" 'simpson-horizontal-split)))

(defun simpson-vert-split()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun simpson-horizontal-split()
  (interactive)
  (split-window-vertically)
  (evil-window-down 1))

(use-package key-chord
             :defer 3
             :config (progn
                       (key-chord-mode 1)
                       (setq key-chord-two-keys-delay 0.1)
                       (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                       (key-chord-define evil-normal-state-map "//" 'comment-region)
                       (key-chord-define evil-normal-state-map "??" 'uncomment-region)
                       (key-chord-define evil-normal-state-map "cc" 'simpson-magit-comment)))

(defun simpson-magit-comment()
  "A small wrapper around 'comment-line' keychord for magit.
      Mashing cc in a magit-status window triggers my custom keybind
      to (comment-line) this function checks what mode is current and then either
      comments or commit"
  (interactive)
  (if (string= major-mode "magit-status-mode")
      (magit-commit)
      (comment-line 1)))

(use-package evil-matchit
             :if simpson-evil
             :after evil
             :defer 1
             :config (progn
                       (global-evil-matchit-mode 1)
                       (plist-put evilmi-plugins 'handlebars-mode '((evilmi-simple-get-tag evilmi-simple-jump)))))

(use-package flyspell-correct-ivy
             :diminish "spell"
             :after ivy
             :bind ("C-SPC C" . flyspell-correct-wrapper))

(use-package magit
             :defer 1
             :bind ("C-SPC g" . magit-status)
             :config (progn
                       (setq auto-revert-buffer-list-filter nil)
                       (define-key magit-popup-mode-map (kbd "q") 'magit-mode-bury-buffer)
                       (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
                       (put 'magit-clean 'disabled nil)
                       (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)
                       (setq magit-commit-show-diff nil)
                       (setq magit-log-section-commit-count 0)
                       (set-face-background 'magit-diff-hunk-heading-highlight "DarkMagenta")
                       (set-face-background 'magit-diff-hunk-heading "DarkCyan")))

(use-package forge
             :after magit
             :config (setq magit-pull-or-fetch t))

(use-package evil-magit
             :defer 1
             :disabled
             :after (:all magit evil)
             :if simpson-evil)

(use-package evil-collection
             :defer 1
             :after (:all magit evil)
             :config (evil-collection-init 'magit)
             :if simpson-evil)

(use-package diff-hl
             :defer 1
             :bind (("C-SPC r" . diff-hl-revert-hunk))
             :config (progn
                       (global-diff-hl-mode)
                       (cond
                         ((string= (car custom-enabled-themes) "base16-dracula") (progn
                                                                                   (set-face-background 'diff-hl-change (plist-get base16-dracula-colors :base0C))
                                                                                   (set-face-background 'diff-hl-insert (plist-get base16-dracula-colors :base0B))
                                                                                   (set-face-background 'diff-hl-delete (plist-get base16-dracula-colors :base09)))))))

(use-package org
             :defer 2
             :pin org
             :mode (("\\.txt\\'" . org-mode))
             :config (progn
                       (require 'ox-md)
                       (setq org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
                       (setq org-agenda-files `(,(concat simpson-dropbox-path "org")))
                       (setq org-log-done t)
                       (setq org-deadline-warning-days 3)
                       (setq org-refile-use-outline-path 'file)
                       (setq org-outline-path-complete-in-steps nil)
                       (setq org-export-with-toc nil)
                       (setq org-todo-keywords
                             '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
                       (setq org-agenda-restore-windows-after-quit t)
                       (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
                       (add-hook 'org-mode-hook 'visual-line-mode)
                       (add-hook 'org-mode-hook (lambda () (setq mode-name "org")))
                       (setq org-pretty-entities t)
                       (setq org-export-with-section-numbers nil)))

(defun simpson-prompt-for-feedwrangler-url()
  (if (y-or-n-p "Get feedwrangler url? ")
      (concat ":URL:" " " ivy-feedwrangler--current-link)
      (let (url)
        (setq url (read-string "What url?: "))
        (concat ":URL:" " " url))))

(defun simpson-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))

(defun simpson-org-blog-capture ()
  "Capture a blog post with my blog template."
  (interactive)
  (org-capture nil "b"))

(defun simpson-org-refresh()
  "Refreshes org buffers that change via dropbox to pull in tasks that have been added outside Emacs."
  (interactive)
  (let ((buffers (seq-filter (lambda (buf)
                               (not (buffer-modified-p buf)))
                             (mapcar 'get-buffer
                                     (mapcar 'f-filename
                                             (f-glob "*.txt" (car org-agenda-files)))))))
    (when buffers
      (seq-each (lambda(buf)
                  (set-buffer buf)
                  (revert-buffer t t)) buffers))))

(use-package markdown-mode
             :mode (("\\.md\\'" . markdown-mode))
             :config (progn
                       (add-hook 'markdown-mode-hook 'visual-line-mode)
                       (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
                       (add-hook 'markdown-mode-hook (lambda () (setq mode-name "md")))
                       (setq markdown-command "/usr/bin/pandoc")
                       (setq markdown-live-preview-delete-export t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

(if (equal system-type 'darwin)
    (progn (set-face-attribute 'default nil :font "Hack-12")
           (set-frame-font "Hack-12" nil t))
    (progn (set-face-attribute 'default nil :font "Hack-9")
           (set-frame-font "Hack-9" nil t)))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(use-package yaml-mode
             :mode (("\\.yml?\\'" . yaml-mode)))

(set-default 'truncate-lines t)

(setq make-backup-files nil)

(setq auto-save-default nil)

(setq inhibit-splash-screen t)

(use-package f)

(use-package visual-fill-column
             :defer 1
             :config (progn
                       (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
                       (setq-default visual-fill-column-width 160)))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(setq header-line-format nil)
(set-face-attribute 'header-line nil :background "white" :box nil)

(when (string= (car custom-enabled-themes) "base16-dracula")
  (set-face-foreground 'vertical-border (plist-get base16-dracula-colors :base02))
  (set-face-background 'fringe (plist-get base16-dracula-colors :base00)))

(setq epg-gpg-program "/usr/local/bin/gpg")

;;http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline15
(fset 'yes-or-no-p 'y-or-n-p)

(use-package shrink-path
  :defer 1)

(setq-default mode-line-format nil)

(eval-after-load 'shrink-path (lambda()
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
                                                                '(:eval (when buffer-file-name
                                                                          (when (equal (car (split-string buffer-file-name ":")) "/ssh")
                                                                            (propertize "TRAMP" 'face '(:foreground "#cd5c5c")))))))))

(defvar simpson-mail-count nil)
(defvar simpson-software-update nil)

(set-face-attribute 'mode-line nil :height 1.0 :box nil)

(defun modeline-theme()
  (when (string= (car custom-enabled-themes) "base16-dracula")
    (set-face-attribute 'header-line nil
                        :background (plist-get base16-dracula-colors :base00)
                        :box `(:color ,(plist-get base16-dracula-colors :base00)))
    (set-face-attribute 'mode-line nil
                        :background (plist-get base16-dracula-colors :base04)
                        :foreground (plist-get base16-dracula-colors :base01)
                        :box `(:line-width 5 :color ,(plist-get base16-dracula-colors :base04) :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background (plist-get base16-dracula-colors :base01)
                        :foreground (plist-get base16-dracula-colors :base06)
                        :box `(:line-width 5 :color ,(plist-get base16-dracula-colors :base01) :style nil))))
(modeline-theme)

(set-face-background 'fringe nil)

(defun simpson-evil-mode ()
  "Change mode line color based on evil state."
  (cond
   ((evil-insert-state-p) (set-face-attribute 'mode-line nil :background "#ea51b2" :foreground "white" :box '(:line-width 5 :color "#ea51b2")))
   ((evil-normal-state-p) (modeline-theme))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq confirm-kill-emacs 'yes-or-no-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
             :diminish ""
             :defer 1
             :config (which-key-mode))

(setq fill-column 80)

(use-package avy
             :defer 1
             :bind (
                    ("C-SPC j" . avy-goto-word-1)
                    ("C-SPC J" . avy-goto-char))
             :config (progn
                       (when (string= (car custom-enabled-themes) "base16-ocean")
                         (set-face-background 'avy-lead-face (plist-get base16-ocean-colors :base08))
                         (set-face-background 'avy-lead-face-0 (plist-get base16-ocean-colors :base0C))
                         (set-face-background 'avy-lead-face-1 (plist-get base16-ocean-colors :base05))
                         (set-face-background 'avy-lead-face-2 (plist-get base16-ocean-colors :base0E)))))

(use-package yasnippet
             :diminish yas-minor-mode
             :bind ("C-SPC e" . yas-expand)
             :defer 1
             :init (progn
                     (add-hook 'js2-mode-hook #'yas-minor-mode)
                     (add-hook 'rjsx-mode-hook #'yas-minor-mode)
                     (add-hook 'ruby-mode-hook #'yas-minor-mode)
                     (add-hook 'org-mode-hook #'yas-minor-mode)
                     (add-hook 'go-mode-hook #'yas-minor-mode))
             :config (progn
                       (yas-reload-all)
                       (define-key yas-minor-mode-map (kbd "<tab>") nil)
                       (define-key yas-minor-mode-map (kbd "TAB") nil)))

(use-package emmet-mode
  :diminish "zen"
  :bind (("C-c e" . emmet-expand-line)
         ("C-c y" . emmet-next-edit-point))
  :defer 1
  :init (progn
          ;; Auto-start on any markup modes
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'sgml-mode-hook 'jsEmmet)
          ;; enable Emmet's css abbreviation
          (add-hook 'css-mode-hook  'emmet-mode)
          ;;JSX gets className not class
          (add-hook 'js2-mode-hook 'jsxEmmet)
          (add-hook 'handlebars-mode-hook 'jsEmmet))
  :config (setq emmet-move-cursor-between-quotes t))

(use-package sauron-ams-org
  :defer 2
  :load-path "~/Projects/sauron-ams-org"
  :ensure nil)

(use-package sauron
             :after sauron-ams-org
             :config (progn
                       (setq sauron-modules '(sauron-erc sauron-ams-org))
                       (simpson-load-file "~/.dotfiles/emacs/irc-watch.gpg")
                       (when simpson-evil (add-to-list 'evil-emacs-state-modes 'sauron-mode))
                       (setq sauron-watch-nicks nil)
                       (when (boundp 'simpson-watch-patterns)
                         (setq sauron-watch-patterns simpson-watch-patterns))
                       (setq sauron-hide-mode-line t)
                       (setq sauron-separate-frame nil)
                       (setq sauron-column-alist '((timestamp . 8)
                                                   (origin . 7)
                                                   (message)))
                       (setq sauron-timestamp-format "%H:%M:%S")
                       (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
                       (advice-add 'shell-command-sentinel :before #'simpson-shell-command-sentiel)
                       (sauron-start-hidden)))

(defun jsxEmmet()
  (setq emmet-expand-jsx-className? t))

(defun jsEmmet()
  (setq emmet-expand-jsx-className? nil))

(setq tramp-default-method "ssh")

(use-package dired
             :ensure nil
             :demand t
             :config (progn
                       (setq dired-dwim-target t)
                       (setq dired-recursive-deletes t)
                       (setq delete-by-moving-to-trash t)
                       (setq dired-use-ls-dired nil)
                       (define-key dired-mode-map "e" 'epa-dired-do-encrypt)
                       (define-key dired-mode-map (kbd "C-o") 'simpson-dired-open-at-point)
                       (define-key dired-mode-map (kbd "C-c !") 'simpson-dired-script-at-point)
                       (simpson-make-neutral dired-mode-map)
                       (simpson-make-neutral--keys dired-mode-map)
                       (define-key dired-mode-map "E" 'epa-dired-do-decrypt)
                       (define-key dired-mode-map "A" 'upload-to-s3)))

(defun simpson-dired-script-at-point()
  "Call 'async-shell-command' on file at point.
      If file is package.json run npm install."
  (interactive)
  (let ((file (dired-file-name-at-point)))
    (if (string-equal "package.json" (file-name-nondirectory file))
        (async-shell-command "npm i")
      (async-shell-command file))))

(defun simpson-dired-open-at-point()
  "Call open process on filename at point."
  (interactive)
  (call-process "open" nil nil nil (file-truename (dired-file-name-at-point))))

(use-package editorconfig
  :diminish ""
  :defer 1
  :config (editorconfig-mode 1))

(use-package prettier
             :diminish "pretty"
             :defer 1)

(use-package flyspell
             :diminish "spell"
             :defer 1
             :config (progn
                       (setq ispell-dictionary "en_US")
                       (add-hook 'erc-mode-hook (lambda () (flyspell-mode 1)))
                       (add-hook 'message-mode-hook (lambda () (flyspell-mode 1)))
                       (setq flyspell-issue-message-flag nil)))

(when (file-exists-p "~/.dotfiles/emacs/authinfo.gpg")
  (setq auth-sources '("~/.dotfiles/emacs/authinfo.gpg")))

(use-package ivy
             :diminish ""
             :defer 1
             :config (progn
                       (ivy-mode)
                       (setq ivy-use-virtual-buffers t)
                       (setq ivy-height 20)
                       (setq ivy-count-format "")
                       (setq ivy-use-selectable-prompt t)
                       (global-set-key (kbd "C-SPC A") 'ivy-resume)
                       (define-key global-map (kbd "C-=") 'ivy-switch-buffer)
                       (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
                       (push '(counsel-M-x . "") ivy-initial-inputs-alist)
                       (simpson-make-neutral ivy-occur-mode-map)
                       (define-key dired-mode-map "r" '(lambda() (interactive) (counsel-rg nil (file-truename dired-directory))))
                       (ivy-add-actions 'counsel-ag '(("O" simpson-other-window "open in new window")))
                       (ivy-add-actions 'counsel-rg '(("O" simpson-other-window "open in new window")))
                       (ivy-set-actions 'links-for-region '(("e" (lambda(item) (browse-url item)) "Browse")))))

(defun links-for-region()
  "Return prompt of URLs for selected word(s).  Put selected URL on 'kill-ring'."
  (interactive)
  (let* ((prompt (use-region-p))
         (query (if prompt
                    (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string "Search for: ")))
         (buf (url-retrieve-synchronously (concat "https://duckduckgo.com/html/?q=" query) t))
         (dom (with-current-buffer buf
                (libxml-parse-html-region url-http-end-of-headers (point-max))))
         (url-list (mapcar (lambda(x)
                             (car (seq-filter
                                   (lambda(x) (string-match "http" x))
                                   (split-string (url-unhex-string (dom-attr x 'href)) "="))))
                           (dom-by-class dom "result__a"))))
    (ivy-read "Which URL? " url-list
              :action (lambda(link) (kill-new link))
              :caller 'links-for-region)

    (ivy-set-actions
     'links-for-region
     '(("d" (lambda (url)
              (let* ((desc (get-buffer-create "*desc*"))
                     (buf (url-retrieve-synchronously url))
                     (dom (with-current-buffer buf
                            (libxml-parse-html-region url-http-end-of-headers (point-max)))))
                (with-current-buffer desc
                  (erase-buffer)
                  (insert (cdr (assoc 'content (seq-filter
                                                'identity
                                                (mapcar
                                                 (lambda (x) (assoc 'content (car (cdr x))))
                                                 (dom-by-tag dom 'meta))))))
                  (pop-to-buffer desc)))) "View description")))
    (cond
      ((string-equal "markdown-mode" major-mode) (markdown-insert-link))
      ((string-equal "org-mode" major-mode) (org-insert-link)))))

(defun simpson-other-window(x)
  (let* ((string (split-string x ":"))
         (num (string-to-number (car (cdr string))))
         (file (car string)))
    (find-file-other-window (concat (locate-dominating-file file ".git") file))
    (with-current-buffer (get-buffer (file-name-nondirectory file))
      (goto-line num))))

(use-package counsel
             :defer 1
             :bind ("C-SPC f" . counsel-find-file)
             :diminish "con"
             :config (progn
                       (counsel-mode)
                       (global-set-key (kbd "M-x") 'counsel-M-x)
                       (define-key dired-mode-map "f" 'counsel-find-file)
                       (ivy-add-actions 'counsel-find-file '(("D" (lambda(file) (delete-file file t)) "delete")))
                       (ivy-add-actions 'counsel-find-file '(("h" (lambda(file) (dired (file-name-directory file))) "Dired")))
                       (global-set-key (kbd "<f1> f") 'counsel-describe-function)
                       (global-set-key (kbd "<f1> v") 'counsel-describe-variable)))

(defun simpson-browse()
  "Fuzzy finding interface to eyebrowse workspaces.
      If the workspace is not tagged with a name, the number is used instead.
      An asterisk (*) deontes current workspace."
  (interactive)
  (let* ((panes (map 'list (lambda (win)
                             (list (if (string-empty-p (car (last win)))
                                       (number-to-string (car win))
                                       (if (equal (car win) (eyebrowse--get 'current-slot))
                                           (concat (car (last win)) "*")
                                           (car (last win)))) :number (car win)))
                     (eyebrowse--get 'window-configs)))
         (pane (completing-read "Jump to session: " panes)))
    (eyebrowse-switch-to-window-config (plist-get (cdr (assoc pane panes)) :number))))

(use-package eyebrowse
             :defer 1
             :init (setq eyebrowse-keymap-prefix (kbd "C-SPC s"))
             :config (progn
                       (eyebrowse-mode t)
                       (set-face-foreground 'eyebrowse-mode-line-active "green4")
                       (setq eyebrowse-mode-line-style nil)
                       (global-set-key (kbd "C-SPC s t") 'simpson-browse)
                       (setq eyebrowse-new-workspace t)))

(defun simpson-pretty-lambda()
  "Make the word lambda the greek character in elisp files."
  (setq prettify-symbols-alist '(("lambda" . 955))))

(use-package elisp-mode
             :ensure nil
             :init (progn
                     (add-hook 'emacs-lisp-mode-hook 'simpson-pretty-lambda)
                     (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
                     (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
                     (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
                     (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "λ")))))

;;minor modes are set with diminish
;;major modes are changed in the mode hook using the variable mode-name
(use-package diminish
             :defer 1
             :config (progn
                       (diminish 'smerge-mode "#$!&")
                       (diminish 'buffer-face-mode)
                       (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
                       (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
                       (add-hook 'shell-mode-hook (lambda () (setq mode-name "shell")))
                       (add-hook 'image-mode-hook (lambda () (setq mode-name "image")))
                       (add-hook 'makefile-bsdmake-mode-hook (lambda () (setq mode-name "make")))))

(use-package swiper
             :defer 1
             :bind ("C-SPC /" . swiper-isearch))

(use-package command-log-mode)

(use-package elisp-format)

(use-package gist)

(use-package php-mode
             :mode ("\\.php?\\'" . php-mode)
             :config (add-hook 'php-mode-hook (lambda () (setq mode-name "php"))))

(use-package json-mode
             :mode ("\\.json?\\'" . json-mode))

(use-package company
             :defer 1
             :diminish ""
             :config (progn
                       (global-company-mode)
                       (setq lsp-completion-provider :capf)))

(defun simpson-sauron-toggle(&optional x)
  "A function to keep the sauron window visible and sized correctly.
      Optional argument (X) to satisfy the various ways the evil-window-move-
      functions are called."
  (interactive)
  (when (window-live-p (get-buffer-window "*Sauron*"))
    (sr-hide)
    (sr-show)))

(advice-add 'balance-windows :after #'simpson-sauron-toggle)
(advice-add 'evil-window-move-far-right :after #'simpson-sauron-toggle)
(advice-add 'evil-window-move-far-left :after #'simpson-sauron-toggle)
(advice-add 'evil-quit :after #'balance-windows)

(use-package ivy-lobsters
             :ensure nil
             :after ivy
             :defer 1
             :config (setq ivy-lobsters-keep-focus t)
             :if (file-exists-p "~/Projects/ivy-lobsters/")
             :load-path "~/Projects/ivy-lobsters/")

(use-package ivy-reddit
             :ensure nil
             :after ivy
             :defer 1
             :commands ivy-reddit
             :if (file-exists-p "~/Projects/ivy-reddit/")
             :load-path "~/Projects/ivy-reddit/")

(use-package pinboard-popular
             :ensure nil
             :commands pinboard-popular
             :load-path "~/Projects/pinboard-popular/")

(use-package ivy-feedwrangler
             :ensure nil
             :after ivy
             :commands ivy-feedwrangler
             :if (file-exists-p "~/Projects/ivy-feedwrangler/")
             :load-path "~/Projects/ivy-feedwrangler/")

(use-package ox-jira
             :after org
             :defer 1
             :config (add-to-list 'org-export-backends 'jira))

(use-package vlf)

(use-package mocha)

(use-package lua-mode
  :mode("\\.lua?\\'" . lua-mode))

(use-package racket-mode
  :mode("\\.rkt?\\'" . racket-mode))

(use-package hydra
             :defer 1
             :config (progn
                       (global-set-key (kbd "C-SPC M") 'hydra-mocha/body)
                       (global-set-key (kbd "C-SPC G") 'hydra-magit/body)
                       (global-set-key (kbd "C-SPC z") 'ivy-window-configuration--hydra/body)
                       (global-set-key (kbd "C-SPC x") 'hydra-js2/body)
                       (global-set-key (kbd "C-SPC ?") 'hydra-help/body)
                       (global-set-key (kbd "C-SPC v") 'hydra-vimish/body)
                       (global-set-key (kbd "C-SPC E") 'hydra-eww/body)
                       (global-set-key (kbd "C-SPC h") 'hydra-smerge/body)))

(defhydra hydra-mocha ()
  "
      Mocha:
      _a_ test at point
      _f_ test file
      _r_ reload local dir
      _p_ test project
      _c_ custom project scope
      "
  ("a" mocha-test-at-point "file at point")
  ("p" mocha-test-project "project" :exit t)
  ("r" hack-dir-local-variables-non-file-buffer "reload local vars")
  ("c" simpson-mocha-scope "custom project scope")
  ("f" mocha-test-file "whole file"))

(defun simpson-mocha-scope()
  (interactive)
  (let ((dir (read-directory-name "Which dir?: ")) (string (read-string "What glob? :")))
    (setq mocha-project-test-directory (concat dir string))))

(setq compilation-always-kill t)

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.
      ;; To create a file, visit it with C-x C-f and enter text in its buffer.")
      (lisp-interaction-mode))

(defhydra hydra-magit (:exit t)
  "
    Handy magit commands:
    _f_ find file given revision
    _F_ find file in other window given revision
    _p_ magit-file-popup
    _l_ see the current file's log
    _b_ blame the current file
  "
  ("f" magit-find-file "find file given rev")
  ("F" magit-find-file-other-window "find file in other window given rev")
  ("p" magit-file-popup "Magit file popup")
  ("l" magit-log-buffer-file "view log for file")
  ("b" magit-blame "view blame for file"))

(use-package aggressive-indent
             :mode("\\.lisp?\\'" . aggressive-indent-mode))

(defhydra hydra-vimish (:exit t)
  "
    Vimish Folding
    _v_ fold
    _V_ unfold
    _x_ delete all folds
  "
  ("V" vimish-fold-delete "unfold")
  ("v" vimish-fold "fold")
  ("x" vimish-fold-delete-all "delete all"))

(use-package ivy-window-configuration
             :if (file-exists-p "~/Projects/ivy-window-configuration/")
             :defer 1
             :load-path "~/Projects/ivy-window-configuration/")

(defhydra hydra-help (:exit t)
  "
    Describe things
    _v_ describe variables
    _f_ describe function
    _s_ describe symbol
    _m_ describe mode
    _f_ describe keybind
    _a_ helpful at point
  "
  ("v" counsel-describe-variable "describe variable")
  ("f" counsel-describe-function "describe function")
  ("s" describe-symbol "describe symbol")
  ("k" describe-key "describe symbol")
  ("a" helpful-at-point "helpful at point")
  ("m" describe-mode "describe mode"))

(use-package slime
             :mode(("\\.lisp?\\'" . slime-mode)
                   ("\\.lisp?\\'" . common-lisp-mode))
             :diminish ""
             :after slime-company
             :config(progn
                      (add-hook 'slime-mode-hook (lambda () (setq mode-name "goo")))
                      (setq slime-contribs '(slime-fancy slime-asdf slime-scratch slime-sprof slime-mdot-fu
                                             slime-compiler-notes-tree slime-hyperdoc
                                             slime-indentation slime-repl))
                      (add-to-list 'slime-contribs 'slime-company)
                      (slime-setup)
                      (setq slime-auto-select-connection 'always)
                      (setq slime-kill-without-query-p t)
                      (setq slime-description-autofocus t)
                      (setq slime-fuzzy-explanation "")
                      (setq slime-asdf-collect-notes t)
                      (setq slime-inhibit-pipelining nil)
                      (setq slime-load-failed-fasl 'always)
                      (setq slime-when-complete-filename-expand t)
                      (setq slime-repl-history-remove-duplicates t)
                      (setq slime-repl-history-trim-whitespaces t)
                      (setq slime-export-symbol-representation-auto t)
                      (setq lisp-indent-function 'common-lisp-indent-function)
                      (setq lisp-loop-indent-subclauses nil)
                      (setq lisp-loop-indent-forms-like-keywords t)
                      (setq lisp-lambda-list-keyword-parameter-alignment t)
                      (simpson-make-neutral slime-repl-mode-map)
                      (setq inferior-lisp-program "/usr/bin/sbcl")))

(use-package slime-company
             :defer 1)

(use-package company-quickhelp
             :defer 1
             :config (company-quickhelp-mode))

(defun eww-more-readable ()
  "Make eww more pleasant to use.
Run it after eww buffer is loaded.
Taken from http://acidwords.com/posts/2017-12-01-distraction-free-eww-surfing.html."
  (interactive)
  (setq eww-header-line-format nil)
  (set-window-margins (get-buffer-window) 10 10)
  (text-scale-set 1)
  (redraw-display)
  (eww-reload 'local))

(use-package rust-mode
             :mode("\\.rs?\\'" . rust-mode)
             :diminish ""
             :config(progn
                      (setq rust-indent-offset 2)
                      (add-hook 'rust-mode-hook (lambda () (setq mode-name "rust")))))

(use-package cargo
             :defer 1
             :config(progn
                      (add-hook 'rust-mode-hook 'cargo-minor-mode)))

(use-package flycheck-rust
  :defer 1
  :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package racer
             :defer 1
             :config (progn
                       (setq racer-cmd "~/.cargo/bin/racer")
                       (setq racer-rust-src-path (concat (car (split-string (shell-command-to-string "rustc --print sysroot") "\n")) "/lib/rustlib/src/rust/src"))
                       (add-hook 'rust-mode-hook 'racer-mode)
                       (add-hook 'racer-mode-hook 'eldoc-mode)
                       (add-hook 'racer-mode-hook 'company-mode)))

(use-package helpful)

(with-eval-after-load ".emacs"
  (with-temp-buffer
      (insert-file-contents "~/.emacs.d/shell-history")
    (setq shell-command-history (split-string (buffer-string) "\n"))))

(use-package dockerfile-mode
             :config (add-hook 'dockerfile-mode-hook (lambda() (setq mode-name "dockerfile")))
             :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package suggest)

;; mu4e-bookmarks, mu4e-contexts, mu4e-user-mail-address are all set in mu4e.gpg
(use-package mu4e
             :ensure nil
             :defer 1
             :load-path "/usr/local/share/emacs/site-lisp/mu4e"
             :config (progn
                       (simpson-load-file "~/.dotfiles/emacs/mu4e.el.gpg")
                       ;;(define-key mu4e-view-mode-map (kbd "M") (if mu4e-view-use-gnus 'ignore 'mu4e-view-link-in-mpv))
                       (set-face-attribute 'mu4e-highlight-face nil :background "DarkRed" :foreground nil)
                       (setq mu4e-maildir "~/Mail")
                       (setq mu4e-main-buffer-hide-personal-addresses t)
                       (setq mu4e-view-show-images t)
                       (setq send-mail-function 'smtpmail-send-it)
                       (setq message-send-mail-function 'smtpmail-send-it)
                       (setq message-kill-buffer-on-exit t)
                       (setq mu4e-context-policy 'pick-first)
                       (setq mu4e-compose-format-flowed t)
                       ;;https://vxlabs.com/2019/08/25/format-flowed-with-long-lines/
                       (setq mu4e-view-show-addresses 't)
                       (setq mu4e-get-mail-command "mbsync -a")
                       (setq mu4e-update-interval 300)
                       (setq user-full-name "Adam Simpson")
                       (setq mu4e-confirm-quit nil)
                       (setq mu4e~view-html-text 'text)
                       (setq mu4e-change-filenames-when-moving t)
                       (setq mu4e-headers-include-related nil)
                       (add-to-list 'mu4e-view-actions
                                    '("ViewInBrowser" . mu4e-action-view-in-browser) t)
                       (define-key mu4e-headers-mode-map (kbd "C-c C-u") 'mu4e-update-index)
                       (simpson-make-neutral mu4e-headers-mode-map)
                       (simpson-make-neutral--keys mu4e-headers-mode-map)
                       (simpson-make-neutral--keys mu4e-view-mode-map)
                       (run-with-timer 1 300 'mu4e-update-mail-and-index t)
                       (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)))

(defmacro json-parse! (buffer)
  "Parse and return JSON from BUFFER.  Ideally for the 'url-retrieve' family of funcs."
  `(with-current-buffer ,buffer (json-read-from-string (buffer-substring-no-properties url-http-end-of-headers (point-max)))))

(defun btc()
  "Get BTC price from coinbase API via synchronous url retrieve."
  (interactive)
  (let ((data (json-parse! (url-retrieve-synchronously "https://api.coinbase.com/v2/prices/BTC-USD/spot" t))))
    (message "BTC: $%s" (alist-get 'amount (car data)))))

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))

(eval-after-load 'eww (lambda()
                        (simpson-make-neutral eww-mode-map)
                        (simpson-make-neutral--keys eww-mode-map)
                        (add-hook 'eww-mode-hook (lambda() (text-scale-set 1)))))

(defhydra hydra-smerge()
  "
    Shortcut to smerge next and previous:
    _n_ smerge-next
    _p_ smerge-prev
  "
  ("n" smerge-next "next conflict")
  ("p" smerge-prev "previous conflict"))

(use-package sql
             :ensure nil
             :config (progn
                       (simpson-make-neutral sql-interactive-mode-map)))

(use-package restclient)

(eval-after-load 'compilation-mode (lambda()
                                     (setq compilation-error-regexp-alist-alist
                                           (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                                                   1 ;; file
                                                   2 ;; line
                                                   3 ;; column
                                                   )
                                                 compilation-error-regexp-alist-alist))

                                     (setq compilation-error-regexp-alist
                                           (cons 'node compilation-error-regexp-alist))))

(use-package inf-ruby)

(use-package typescript-mode
             :bind ("C-SPC F" . simpson-find-import)
             :mode (("\\.ts?\\'" . typescript-mode)
                    ("\\.tsx?\\'" . typescript-mode)
                    ("\\.jsx?\\'" . typescript-mode)))

(use-package rainbow-mode
             :diminish "")

(use-package persistent-scratch
             :defer 1
             :config (progn
                       (persistent-scratch-setup-default)
                       (persistent-scratch-autosave-mode)))

(setq warning-suppress-types '(undo discard-info))

(use-package package-lint)

(use-package flymake-json
             :init (add-hook 'json-mode-hook 'flymake-json-load))

(defun simpson-cancel-timer()
  "Browse timer list by function name and cancel the selected timer."
  (interactive)
  (let ((selection (completing-read "Which timer: "
                                    (mapcar (lambda(timer)
                                              (propertize (symbol-name (nth 5 (append timer nil))) 'timer timer)) timer-list))))
    (cancel-timer (get-text-property 0 'timer selection))))


(add-hook 'prog-mode-hook (lambda()
                            (display-line-numbers-mode)
                            (setq display-line-numbers 'relative)))

(add-hook 'vue-mode-hook (lambda()
                           (display-line-numbers-mode)
                           (setq display-line-numbers 'relative)))

(set-face-background 'line-number nil)

(pixel-scroll-mode)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

(unless (or (equal system-type 'darwin) (equal (getenv "LINUX_TYPE") "WSL2"))
  (add-to-list 'default-frame-alist
               '(undecorated . t)))

(use-package org-preview-html
             :after org)

(use-package alert
             :config (progn
                       (setq alert-log-messages nil)
                       (setq alert-default-style 'libnotify)))

(use-package deadgrep
  :commands deadgrep
  :config (progn
            (simpson-make-neutral--keys deadgrep-mode-map)
            (simpson-make-neutral deadgrep-mode-map)))

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-SPC k !") 'simpson-restore-text)

(defun simpson-restore-text()
  "Reset text size to default."
  (interactive)
  (text-scale-set 0))

(defun simpson-project-clone(url)
  "Clone a git repo given URL and then change to that project."
  (interactive "sGit url: ")
  (let (
        (projectsDir "~/Projects/")
        (name (car (split-string (car (last (split-string url "/"))) "\\."))))
    (shell-command (concat "git clone " url " " projectsDir name))
    (eyebrowse-create-window-config)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) name)
    (dired (concat projectsDir name))))

(global-set-key (kbd "C-SPC k b") 'simpson-project-clone)
(global-set-key (kbd "C-SPC k r") 'vc-revert)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k g") 'ffap)


(defun simpson-copy-file-buffer-name()
  "Copy the file path for the current buffer to the clipboard."
  (interactive)
  (let* ((path (buffer-file-name))
         (relative (file-relative-name path (locate-dominating-file path ".git"))))
    (if (y-or-n-p "Code formatting? ")
        (kill-new (concat "`" relative "`"))
        (kill-new relative))))

(global-set-key (kbd "C-SPC D") 'dired-jump)

(global-set-key (kbd "C-SPC k d") 'simpson-new-note)

(defun simpson-insert-date()
  "Shell out to date to return a formatted date string at point."
  (interactive)
  (shell-command "date +%Y-%m-%d-%I:%M" t))

(defun simpson-new-note(name)
  "Create new file for nvAlt with NAME."
  (interactive "sName of file: ")
  (let* ((use-buf (y-or-n-p "Use this buffer? "))
         (date (replace-regexp-in-string "\n$" "" (shell-command-to-string "date +%m-%d-%y"))) file)
    (if (file-directory-p (concat simpson-dropbox-path "Notational Data"))
        (setq file (concat simpson-dropbox-path "Notational Data/" date "-" name ".txt"))
        (setq file (concat simpson-dropbox-path date "-" name ".txt")))
    (if use-buf
        (write-file file)
        (write-region "" "" file))))

(global-set-key (kbd "C-SPC d") (lambda() (interactive)
                                  (dired simpson-dropbox-path "-laGht")))

;;narrow region
(global-set-key (kbd "C-SPC n") 'narrow-to-region)
;;widen
(global-set-key (kbd "C-SPC N") 'widen)

(setq minor-mode-perm-list (copy-alist minor-mode-alist))

(setq minor-mode-alist (list))

;;revert buffer to last-commit
(define-key global-map (kbd "C-SPC R") 'vc-revert)

(global-set-key "\M-h" 'help-command)

;;write buffer to fil0
(define-key global-map (kbd "C-SPC w") 'write-file)
(define-key global-map (kbd "s-s") 'save-buffer)

(define-key global-map (kbd "s-t") nil)
(define-key global-map (kbd "s-p") nil)

(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)

(defun simpson-delete-file-for-buffer()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      ;;could add the option to delete files that are not tracked by VC
      ;;http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
      (if (vc-backend filename)
          (vc-delete-file filename)))))

(define-key global-map (kbd "C-SPC X") 'simpson-delete-file-for-buffer)

(defun simpson-znc()
  "Connect to znc irc bouncer.  znc-server is defined in irc-accounts.gpg."
  (interactive)
  (let ((choice (completing-read "Which IRC Network? " '("freenode" "mozilla"))))
    (erc-tls :server (if (y-or-n-p "Home? ")
                         (plist-get znc-server :home)
                         (plist-get znc-server :remote))
             :port 6697
             :nick "asimpson"
             :password (concat "adam@erc/" choice ":"(cadr (auth-source-user-and-password "irc.znc.net"))))))

(defun simpson-get-file-name()
  "Return the file name for a buffer."
  (interactive)
  (kill-new (car (reverse (split-string (buffer-file-name) "/")))))

(defun simpson-org-scratch()
  "Make a new buffer with 'org-mode'."
  (interactive)
  (evil-window-new nil nil)
  (org-mode))

(define-key global-map (kbd "C-SPC y") 'simpson-org-scratch)

(define-key global-map (kbd "M-y") (lambda() (interactive) (counsel-yank-pop)))

(put 'narrow-to-region 'disabled nil)

(use-package pinboard-unread
             :load-path "~/Projects/pinboard-unread"
             :commands pinboard-unread
             :ensure nil)

(defun simpson-search-eslint()
  "Search the web for the eslint error at point."
  (interactive)
  (let* ((err (flycheck-overlay-errors-at (point)))
         (id (flycheck-error-id (car err)))
         (url (concat "https://duckduckgo.com/?q=" id " !")))
    (browse-url url)))

(use-package nrs
  :load-path "~/Projects/nrs"
  :after ivy
  :commands nrs
  :ensure nil)

;;this exists because mu4e will trigger browse-url with a file that doesn't exist thanks to git checkouts.
(advice-add 'browse-url :before #'simpson-set-default-dir)

(defun simpson-set-default-dir(url &rest args)
  (setq default-directory (expand-file-name "~/")))

(setq async-shell-command-buffer 'new-buffer)
(defun simpson-kill-shells()
  (interactive)
  (seq-each (lambda(buf) (when (string-match "Async Shell" (buffer-name buf))
                      (switch-to-buffer-other-window buf)
                      (kill-buffer-and-window))) (buffer-list))
  (balance-windows))

(use-package simpson-ruby-version
  :ensure nil
  :load-path "~/Projects/simpson-ruby-version"
  :commands simpson-ruby-version
  :config (progn
            (setq simpson-ruby-default-gem-path "/usr/local/bin/gem")
            (setq simpson-ruby-rubies-path "/usr/local/rubies/")
            (setq simpson-ruby-gem-path "~/.gem/ruby/")))

(use-package rspec-mode
  :defer 1
  :mode ("*spec.rb" . rspec-mode)
  :config (simpson-make-neutral compilation-mode-map))

(use-package web-mode
  :defer 1
  :mode ("\\.html\\.erb\\'" . web-mode)
  :config (setq web-mode-markup-indent-offset 2))

(display-battery-mode)
(setq battery-mode-line-format "[%b%p%%/%t]")
(setq battery-mode-line-limit 90)

(defun simpson-bump-migration()
  "Rename a conflicting migration to the current version + 1 or the user supplied number."
  (interactive)
  (let* ((name (file-name-base (dired-file-name-at-point)))
         (path (file-truename (dired-file-name-at-point)))
         (int (car (split-string name "_")))
         (next (+ (string-to-number int) 1))
         (user-next (number-to-string (read-number "Bump to: " next)))
         (new (concat (f-dirname path) "/0" user-next "_" (mapconcat 'identity (cdr (split-string name "_")) "_") ".rb")))
    (rename-file path new)))

(defun simpson-plantuml-preview()
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively 'plantuml-preview)))

(use-package plantuml-mode
             :mode ("\\.plantuml?\\'" . plantuml-mode)
             :config (progn
                       (setq plantuml-jar-path "/usr/local/plantuml.jar")
                       (define-key plantuml-mode-map (kbd "C-c C-c") 'simpson-plantuml-preview)))

(defun simpson-see-process-for-port()
  "Prompt for port and view running process listening on that port."
  (interactive)
  (let ((port (read-string "Which port? ")))
    (async-shell-command (concat "lsof -wni tcp:" port))))

(use-package epresent)

(defhydra hydra-diff-jump()
  "
    Shortcut to diff next and previous:
    _n_ diff-next
    _p_ diff-prev
  "
  ("n" diff-hl-next-hunk "next hunk")
  ("p" diff-hl-previous-hunk "previous hunk"))

(define-key global-map (kbd "C-x n") 'hydra-diff-jump/body)

(use-package twig-mode
             :mode ("\\.twig\\'" . twig-mode))

(defun s3-parser(&optional target)
  "Run aws S3 ls command with an option bucket TARGET. Output is cut down to just names of objects."
  (let ((cmd (if target
                 (concat "aws s3 ls " target)
                 "aws s3 ls")))
    (mapcar (lambda(item) (last (split-string item " "))) (split-string (shell-command-to-string cmd) "\n"))))

(defun upload-to-s3()
  "Upload a file to S3."
  (interactive)
  (let* ((buffer "*s3-upload-process*")
         (buckets (s3-parser))
         (bucket (ivy-read "Bucket: " buckets))
         (dirs (seq-filter (lambda(dir) (string-match-p "/" (car dir))) (s3-parser bucket)))
         (dir (ivy-read "Directory: " dirs)))
    (start-process "s3-upload" buffer "aws" "s3" "cp" (file-truename (dired-file-name-at-point)) (concat "s3://" bucket "/" dir))
    (pop-to-buffer buffer)))

(defun reload-xcode()
  (interactive)
  (shell-command "osascript -e 'tell application \"Xcode\"
	activate
	tell application \"System Events\" to keystroke \"r\" using command down
end tell'
"))

(global-set-key (kbd "s-r") 'reload-xcode)

(use-package docker)

(use-package docker-compose-mode
             :mode ("\\docker-compose.yml\\'" . docker-compose-mode))

(use-package savehist
             :custom
             (savehist-file "~/.emacs.d/savehist")
             (savehist-save-minibuffer-history t)
             (history-length 10000)
             (savehist-additional-variables
              '(shell-command-history))
             :config
             (savehist-mode +1))

(use-package vue-mode
             :config (set-face-background 'mmm-default-submode-face nil))

(use-package jest)

(use-package go-mode
             :mode ("\\.go\\'" . go-mode))

(use-package terraform-mode
             :mode ("\\.tf\\'" . terraform-mode))

(use-package lsp-mode
             :bind ("C-SPC ," . lsp-workspace-restart)
             :commands (lsp lsp-deferred)
             :hook ((typescript-mode . lsp-deferred)
                    (rust-mode . lsp-deferred)))

(use-package lsp-ui
             :commands lsp-ui-mode)

(setq lsp-eldoc-render-all t)

(cua-mode)

(load "~/quicklisp/clhs-use-local.el" t)

(use-package ng2-mode)

(defun mu4e-view-link-in-mpv (&optional multi)
  "Offer to view URLs in MPV if possible."
  (interactive "P")
  (mu4e~view-handle-urls "URL to watch"
                         multi
                         (lambda (url) (start-process "mpv" nil "mpv" url))))

(setq browse-url-generic-program "firefox")

(setq browse-url-browser-function 'browse-url-generic)

(use-package counsel-jq)

(use-package direnv
             :defer 1
             :config (progn
                       (direnv-mode)
                       (advice-add 'lsp :before #'direnv-update-environment)))

(use-package eglot
             :defer 1
             :config (progn
                       (setq eglot-extend-to-xref t)
                       (add-hook 'go-mode-hook (lambda()
                                                 (add-hook 'before-save-hook 'eglot-format nil t)))
                       (add-hook 'go-mode-hook 'eglot-ensure)))

(use-package simple-mpc)

(defun simpson-find-import()
  "Performa a ripgrep search for import + filename."
  (interactive)
  (counsel-rg (concat "import " (file-name-base (buffer-file-name)))))
;;; .emacs ends here
