;; -*- lexical-binding: t; -*-
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default
  use-package-always-defer t
  use-package-always-ensure t)

;debug use-package 👇
;(setq use-package-verbose t)

(show-paren-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell nil)

(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/lib/node_modules")))
(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "/Users/asimpson/.better-npm/lib/node_modules:/Users/asimpson/.better-npm/bin:/usr/local/bin:/usr/local/sbin")

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(global-set-key (kbd "C-SPC") nil)

(use-package osx-trash
  :if (eq system-type 'darwin)
  :config (progn
            (osx-trash-setup)
            (setq delete-by-moving-to-trash t)))

(use-package base16-theme
  :if (unless (null window-system))
  :init (load-theme 'base16-ocean t))

(use-package exec-path-from-shell
  :defer 2
  :config (progn
            (when (memq window-system '(mac ns))
              (exec-path-from-shell-initialize))))

(use-package dired-narrow
  :defer 1
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-subtree
  :defer 1
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package vimish-fold
  :defer 1
  :config (vimish-fold-global-mode 1))

(use-package flycheck
  :diminish "lint"
  :defer 1
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-SPC '" . flycheck-mode)
  :config (progn
            (setq flycheck-global-modes '(rjsx-mode emacs-lisp-mode json-mode))
            ;;https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
            (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))))

(use-package evil
  :if simpson-evil
  :diminish "vim"
  :defer 1
  :config (progn
            (evil-mode t)
            (setq-default evil-shift-width 2)
            (setq evil-vsplit-window-right t)
            (setq evil-split-window-below t)
            (add-to-list 'evil-emacs-state-modes 'dired-mode)
            (add-to-list 'evil-emacs-state-modes 'epa-key-list-mode)
            (add-to-list 'evil-emacs-state-modes 'ivy-occur-mode)
            ;;http://spacemacs.org/doc/FAQ#orgheadline31
            (fset 'evil-visual-update-x-selection 'ignore)
            (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
            (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
            (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
            (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
            (define-key evil-normal-state-map (kbd "gx") 'browse-url)
            (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
            (when simpson-helm (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file))
            (when simpson-helm (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-projectile-find-file))
            (unless simpson-helm (define-key evil-normal-state-map (kbd "SPC SPC") 'projectile-find-file-other-window))
            (unless simpson-helm (define-key evil-normal-state-map (kbd "\C-p") 'projectile-find-file-other-window))
            (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
            (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)
            (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)))

(add-hook 'evil-after-load-hook 'simpson-set-evil-active)

(defun simpson-set-evil-active()
  (setq simpson-evil-active t))

(use-package evil-leader
  :if simpson-evil
  :after evil
  :defer 1
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (when simpson-helm
              (evil-leader/set-key "f" 'helm-projectile-ag)
              (evil-leader/set-key "F" 'helm-do-ag))
            (unless simpson-helm
              (evil-leader/set-key "f" 'counsel-rg)
              (evil-leader/set-key "s" 'hydra-searching/body)
              (evil-leader/set-key "F" 'simpson-counsel-ag))
            (evil-leader/set-key "c" 'fci-mode)
            (evil-leader/set-key "v" 'evil-window-vnew)
            (evil-leader/set-key "x" 'evil-window-new)))

(defun simpson-counsel-ag()
  (interactive)
  (let ((current-prefix-arg t))
  (counsel-ag)))

(use-package key-chord
  :defer 3
  :config (progn
            (key-chord-mode 1)
            (setq key-chord-two-keys-delay 0.1)
            (when simpson-evil
              (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
              (key-chord-define evil-normal-state-map "//" 'comment-region)
              (key-chord-define evil-normal-state-map "??" 'uncomment-region)
              (key-chord-define evil-normal-state-map "cc" 'simpson-magit-comment))
            (unless simpson-evil (key-chord-define-global "jk" 'god-local-mode))))

(defun simpson-magit-comment()
  "mashing cc in a magit-status window triggers my custom keybind to (comment-line)
   this function checks what mode is current and then either comments or commit"
  (interactive)
  (if (string= major-mode "magit-status-mode")
      (magit-commit)
    (comment-line 1)))

(use-package god-mode
  :if (not simpson-evil)
  :defer 1
  :config (progn
            (defun simpson-god-mode-hook ()
              (if god-local-mode
                  (set-face-attribute 'mode-line nil :background "#d08770" :foreground "#343d46" :box '(:line-width 3 :color "#d08770" :style nil))
                (set-face-attribute 'mode-line nil :background "#dfe1e8" :foreground "#343d46"  :box '(:line-width 3 :color "#dfe1e8" :style ni))))
            (add-hook 'god-mode-enabled-hook 'simpson-god-mode-hook)
            (add-hook 'god-mode-disabled-hook 'simpson-god-mode-hook)
            (when simpson-helm
              (define-key god-local-mode-map (kbd "P") 'helm-projectile-find-file)
              (define-key god-local-mode-map (kbd "F") 'helm-projectile-ag))))

(use-package evil-matchit
  :if simpson-evil
  :after evil
  :config (progn
            (global-evil-matchit-mode 1)
            (plist-put evilmi-plugins 'handlebars-mode '((evilmi-simple-get-tag evilmi-simple-jump)))))

(use-package helm
  :if simpson-helm
  :diminish ""
  :bind (("M-x" . helm-M-x)
         ("C-=" . helm-mini)
         ("C-SPC f" . helm-find-files)
         ("C-SPC k p" . simpson-projects-browser))
  :config (progn
            (helm-mode)
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-background 'helm-ff-dotted-directory (plist-get base16-ocean-colors :base00))
              (set-face-background 'helm-ff-dotted-symlink-directory (plist-get base16-ocean-colors :base00))
              (set-face-foreground 'helm-ff-dotted-directory (plist-get base16-ocean-colors :base03))
              (set-face-foreground 'helm-ff-dotted-symlink-directory (plist-get base16-ocean-colors :base03)))))

(use-package projectile
  :diminish ""
  :bind (("C-SPC b" . projectile-switch-project)
         ("C-c C-p" . projectile-find-file-other-window))
  :config (progn
            (projectile-global-mode)
            (setq projectile-enable-caching nil)
            (setq projectile-switch-project-action 'projectile-find-file)
            (setq projectile-completion-system (if simpson-helm 'helm 'ivy))))

(use-package helm-projectile
  :after projectile
  :if simpson-helm
  :config (progn
            (helm-projectile-on)
            (setq projectile-switch-project-action 'projectile-find-file)
            (setq projectile-completion-system (if simpson-helm 'helm 'ivy))))

(use-package helm-ag
  :if simpson-helm
  :after helm
  :init (setq helm-ag-base-command "ag --nocolor --nogroup"))

(use-package helm-flyspell
  :diminish "spell"
  :if simpson-helm
  :after helm
  :bind ("C-SPC C" . helm-flyspell-correct))

(use-package flyspell-correct-ivy
  :diminish "spell"
  :if (not simpson-helm)
  :after ivy
  :bind ("C-SPC C" . flyspell-correct-previous-word-generic))

(defun simpson-projects-browser()
  (interactive)
  (cd "~/Projects/")
  (if simpson-helm
      (helm-find-files nil)
    (counsel-find-file)))

(use-package magit
  :pin melpa-stable
  :defer 1
  :bind ("C-SPC g" . magit-status)
  :config (progn
            ;;https://github.com/magit/magit/pull/2513
            ;;Users who use Tramp and experience delays, should consider setting
            ;;the option to `magit-auto-revert-repository-buffer-p'.
            (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (put 'magit-clean 'disabled nil)
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-foreground 'magit-blame-date (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-blame-hash (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-blame-heading (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-blame-name (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-blame-summary (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-sequence-onto (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-sequence-done (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'magit-hash (plist-get base16-ocean-colors :base0C))
              (set-face-background 'magit-section-highlight (plist-get base16-ocean-colors :base01)))))

(use-package evil-magit
  :pin melpa-stable
  :after evil
  :if simpson-evil)

(use-package diff-hl
  :defer 1
  :bind (("C-SPC r" . diff-hl-revert-hunk)
         ("C-x p" . diff-hl-previous-hunk)
         ("C-x n" . diff-hl-next-hunk))
  :config (progn
            (global-diff-hl-mode)
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-background 'diff-hl-change (plist-get base16-ocean-colors :base0C))
              (set-face-background 'diff-hl-insert (plist-get base16-ocean-colors :base0B))
              (set-face-background 'diff-hl-delete (plist-get base16-ocean-colors :base09)))))

(use-package org
  :defer 2
  :if (file-exists-p "~/Dropbox (Personal)/org/tasks.txt")
  :bind (("C-SPC c" . simpson-org-task-capture)
         ("C-SPC k B" . simpson-org-blog-capture)
         ("C-SPC t" . org-todo-list)
         ("C-SPC a" . org-agenda)
         ("C-SPC T" . org-tags-view))
  :mode (("\\.txt\\'" . org-mode))
  :config (progn
            (require 'org-notmuch)
            (require 'ox-md)
            ;;look into swapping with txt, org-agenda-file-regexp
            (setq org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
            (setq org-agenda-files '("~/Dropbox (Personal)/org"))
            (setq org-log-done t)
            (setq org-deadline-warning-days 3)
            (setq org-export-with-toc nil)
            (setq org-todo-keywords
                  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
            (setq org-capture-templates
                  '(("a" "My TODO task format." entry
                     (file "~/Dropbox (Personal)/org/tasks.txt")
                     "* TODO %? %^g
    :PROPERTIES:
    :CREATED: %T
    :END:")
                    ("b" "My blog post captures" entry
                     (file "~/Dropbox (Personal)/org/reading.txt")
                     "* %? %^g
    :PROPERTIES:
    %(simpson-prompt-for-feedwrangler-url)
    :CREATED: %T
    :END:")))
            (setq org-agenda-restore-windows-after-quit t)
            (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
            (add-hook 'org-mode-hook 'visual-line-mode)
            (add-hook 'org-mode-hook (lambda () (setq mode-name "org")))
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-foreground 'org-link (plist-get base16-ocean-colors :base0B))
              (set-face-foreground 'org-tag (plist-get base16-ocean-colors :base0A))
              (set-face-foreground 'org-agenda-structure (plist-get base16-ocean-colors :base03)))
            (setq org-html-head "
      <style>
        body {
          width: 800px;
          margin: 0 auto;
          font-family: sans-serif;
        }
        img {
          display: block;
          width: 100%;
          max-width: 100%;
        }
        pre {
          overflow-y: scroll !important;
        }
      </style>
    ")
            (setq exec-path (append exec-path '("/Library/TeX/texbin/latex")))
            (global-set-key (kbd "C-SPC k f") 'org-footnote-new)
            (global-set-key (kbd "C-SPC k l") 'org-toggle-link-display)
            (setq org-export-backends '(ascii html icalendar latex md))
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((sh . t)
               (js . t)))
            (run-at-time 0 (* 60 15) #'simpson-org-refresh)
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-attribute 'org-mode-line-clock nil :foreground (plist-get base16-ocean-colors :base0E) :background nil :box nil :inherit nil))
            (setq org-pretty-entities t)
            (setq org-export-with-section-numbers nil)))

(defun simpson-prompt-for-feedwrangler-url()
  (if (y-or-n-p "Get feedwrangler url?")
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
  "refreshes task buffer to pull in tasks that have been added outside emacs"
  (interactive)
  (when (buffer-live-p "tasks.txt")
    (set-buffer "tasks.txt")
    (revert-buffer t t)))

(use-package multi-term
  :config (progn
            (setq multi-term-program "/bin/zsh")
            (setq multi-term-program-switches "--login")
            (define-key global-map (kbd "C-SPC p") 'term-paste)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config (progn
            ;;add custom fonts for markdown mode
            (add-hook 'markdown-mode-hook 'markdown-fonts)
            ;;toggle on visual line mode for writing
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            ;;toggle on spell-check for writing
            (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
            (add-hook 'markdown-mode-hook (lambda () (setq mode-name "md")))
            (setq markdown-open-command "/usr/local/bin/marked")))

(use-package js2-mode
  :disabled
  :interpreter (("node" . js2-mode))
  :config (progn
            ;; (add-hook 'js2-mode-hook 'relative-line-numbers-mode)
            (setq js2-basic-offset 2)
            (setq js2-highlight-level 3)
            (setq js2-bounce-indent-p t)
            (electric-indent-mode -1)
            (setq js2-mode-show-strict-warnings nil)
            (add-hook 'js2-mode-hook (lambda() (setq show-trailing-whitespace t)))
            (global-set-key (kbd "C-SPC k j") 'js2-mode-hide-warnings-and-errors)
            (defcustom js2-strict-missing-semi-warning nil
              "Non-nil to warn about semicolon auto-insertion after statement.
    Technically this is legal per Ecma-262, but some style guides disallow
    depending on it."
              :type 'boolean
              :group 'js2-mode)))

(use-package rjsx-mode
  :interpreter (("node" . rjsx-mode))
  :mode (("\\.js?\\'" . rjsx-mode)
         ("\\.jsx?\\'" . rjsx-mode))
  :config (progn
            (setq js2-basic-offset 2)
            (setq js2-highlight-level 3)
            (setq js2-bounce-indent-p t)
            (electric-indent-mode -1)
            (setq js2-mode-show-strict-warnings nil)
            (add-hook 'js2-mode-hook (lambda() (setq show-trailing-whitespace t)))
            (add-hook 'rjsx-mode-hook (lambda() (setq mode-name "jsx")))))

;;indents! so brutal, each mode can have their own, e.g. css
;;spaces
(setq-default indent-tabs-mode nil)

;;2 of em
(setq-default tab-width 2)

;;yes, css, even you
(setq-default css-indent-offset 2)

;;fonts
(set-face-attribute 'default nil :font "Hack-10" )
(set-frame-font "Hack-10" nil t)

;;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;each line gets one line
(set-default 'truncate-lines t)

;;backups suck, use Git
;; stop creating backup~ files
(setq make-backup-files nil)

;;backups suck, use Git
(setq auto-save-default nil)

;;splash screen is gross
(setq inhibit-splash-screen t)

;; Use monospaced font faces in current buffer
(defun markdown-fonts ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack" :height 120))
  (buffer-face-mode))

(use-package visual-fill-column
  :defer 1
  :config (progn
            (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
            (setq-default visual-fill-column-width 160)))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;treat new buffers as modified files
;http://stackoverflow.com/a/2592558/2344737
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(defun simpson-header ()
  (let (output)
    (setq output "¯\\_(ツ)_/¯")
    (when (stringp (buffer-file-name))
      (setq output (concat " ▼ ../" (substring (buffer-file-name) 16 nil))))
    (when (string= major-mode "sauron-mode")
      (setq output "(o)"))
    (setq header-line-format output)))

;set the header intially
(simpson-header)

;update the header whenever the buffer-list changes
(add-hook 'buffer-list-update-hook 'simpson-header)

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-foreground 'header-line "#a3adb5")
  (set-face-background 'header-line (plist-get base16-ocean-colors :base02))
  (set-face-attribute 'header-line nil
      :box `(:line-width 1 :color ,(plist-get base16-ocean-colors :base02) :style nil))
  (set-face-foreground 'vertical-border (plist-get base16-ocean-colors :base02))
  (set-face-background 'fringe (plist-get base16-ocean-colors :base00)))

(setenv "GPG_AGENT" "/usr/local/bin/gpg-agent")
;;read gpg-agent environment
;;https://gist.github.com/jupp0r/08ca64b7c14c6093bba2
(defun read-env-line (line)
  "read a env line and post to environment"
  (let ((key-value-pair (split-string line "=" t)))
    ;;car returns first value in list, last return last value
    (setenv (car key-value-pair) (car (last key-value-pair)))))

(defvar gpg-agent-info-file)

(setq gpg-agent-info-file (concat (getenv "HOME") "/.gpg-agent-info"))

(when (file-exists-p gpg-agent-info-file)
  (with-temp-buffer
    ;;create a temp buffer and evalute the BODY of the function
    (progn
      ;;insert the gpg-agent-info file as a string to the temp buffer
      (insert-file-contents gpg-agent-info-file)
      ;;mapc is like map but doesn't accumulate the results
      ;;buffer-string is now our temp-buffer which is filled with our gpg file.
      ;;split at new lines
      (mapc 'read-env-line (split-string (buffer-string) "\n" t)))))

(setq epg-gpg-program "/usr/local/bin/gpg")

;;y over yes
;;http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline15
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default mode-line-format (list
                                ;;mode-line-modified
                                '(:eval (if (buffer-modified-p)
                                            (propertize " !" 'face '(:foreground "#cf6a4c"))
                                          ))
                                " "
                                '(:eval (when (and simpson-evil simpson-evil-active) (propertize evil-mode-line-tag 'face '(:foreground "#bf616a"))))
                                " "
                                '(:eval mode-line-position)
                                mode-line-modes
                                mode-line-misc-info))

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-attribute 'mode-line nil
      :background (plist-get base16-ocean-colors :base06)
      :foreground (plist-get base16-ocean-colors :base01)
      :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base06) :style nil))
  (set-face-attribute 'mode-line-inactive nil
      :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base01) :style nil)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq confirm-kill-emacs 'yes-or-no-p)

(add-hook 'css-mode-hook '(lambda() (setq show-trailing-whitespace t)))

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-background 'trailing-whitespace (plist-get base16-ocean-colors :base0F)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :diminish ""
  :defer 1
  :config (which-key-mode))

(use-package fill-column-indicator
  :config (setq fci-rule-column 80))

(use-package relative-line-numbers
  :diminish ""
  :bind ("C-SPC l" . relative-line-numbers-mode)
  :config (progn
            ;; (add-hook 'css-mode-hook 'relative-line-numbers-mode)
            ;; (add-hook 'web-mode-hook 'relative-line-numbers-mode)
            ;; (add-hook 'emacs-lisp-mode-hook 'relative-line-numbers-mode)
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-foreground 'relative-line-numbers-current-line (plist-get base16-ocean-colors :base09)))))

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

(use-package reveal-in-osx-finder)

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("C-SPC e" . yas-expand)
  :load-path "~/.emacs.d/elpa/yasnippet"
  :defer 1
  :init (progn
          (add-hook 'js2-mode-hook #'yas-minor-mode)
          (add-hook 'rjsx-mode-hook #'yas-minor-mode)
          (add-hook 'org-mode-hook #'yas-minor-mode))
  :config (progn
            (yas-reload-all)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)))

(use-package deft
  :bind ("C-SPC d" . deft)
  :config (progn
            (when simpson-evil (add-to-list 'evil-emacs-state-modes 'deft-mode))
            (setq deft-extensions '("txt" "txt.gpg" "org"))
            (setq deft-directory "/Users/asimpson/Dropbox (Personal)/Notational Data")
            (setq deft-use-filename-as-title t)
            (setq deft-auto-save-interval 60.0)))

(use-package emmet-mode
  :diminish "zen"
  :bind (("C-c e" . emmet-expand-line)
         ("C-c y" . emmet-next-edit-point))
  :defer 1
  :init (progn
          ;; Auto-start on any markup modes
          (add-hook 'sgml-mode-hook 'emmet-mode)
          ;; enable Emmet's css abbreviation.
          (add-hook 'css-mode-hook  'emmet-mode)
          ;;JSX gets className not class
          (add-hook 'js2-mode-hook 'jsxEmmet)
          (add-hook 'handlebars-mode-hook 'jsEmmet))
  ;; default nil
  :config (setq emmet-move-cursor-between-quotes t))

(use-package sauron
  :pin melpa-stable
  :defer 2
  :init (setq sauron-modules '(sauron-erc sauron-ams-org))
  :config (progn
            (when (file-exists-p "~/.dotfiles/emacs/irc-watch.gpg")
              (load-library "~/.dotfiles/emacs/irc-watch.gpg"))
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
            (sauron-start-hidden)
            (advice-add 'shell-command-sentinel :before #'simpson-shell-command-sentiel)))

(defun jsxEmmet()
  (setq emmet-expand-jsx-className? t))

(defun jsEmmet()
  (setq emmet-expand-jsx-className? nil))

(setq tramp-default-method "ssh")

;;http://emacs.stackexchange.com/a/58
;;to open a file with sudo, invoke C-x C-f and then type /sudo::/path

(setq dired-recursive-deletes t)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired nil)

(use-package editorconfig
  :diminish ""
  :defer 1
  :config (editorconfig-mode 1))

(use-package prettier-js
  :diminish "pretty"
  :defer 1
  :init (progn
          (add-hook 'js2-mode-hook 'prettier-js-mode)
          (add-hook 'rjsx-mode-hook 'prettier-js-mode))
  :config (setq prettier-js-args '(
                                   "--trailing-comma" "es5"
                                   "--bracket-spacing" "true"
                                   "--single-quote" "true")))

;; Mutt support.
(setq auto-mode-alist (append '(("mutt-*" . mail-mode)) auto-mode-alist))

(defun simpson-shell-command-sentiel(proc sig)
  (when (seq-filter (lambda(x)
                      (or (string-match "npm install" x)
                          (string-match "npm i" x)))
                    (process-command proc))
    (sauron-add-event 'shell 3 "npm install is finished" nil))
  (when (and (memq (process-status proc)
                   '(exit))
             (not (string= (string-trim sig) "finished")))
    (sauron-add-event 'shell 3 sig (lambda() #'(switch-to-buffer-other-window
                                                "*Async Shell Command*")))))

(use-package erc
  :bind (:map erc-mode-map ("C-c f" . simpson-format-slack-name))
  :config (progn
            (when simpson-evil (add-to-list 'evil-emacs-state-modes 'erc-mode)
                  (evil-set-initial-state 'erc-mode 'emacs))
            (setq erc-default-port 6667)
            (setq erc-prompt-for-password nil)
            (setq erc-kill-queries-on-quit t)
            (setq erc-log-insert-log-on-open t)
            (setq erc-log-channels-directory "~/.erc/logs/")
            (setq erc-save-buffer-on-part t)
            (setq erc-join-buffer "bury")
            (when (file-exists-p "~/.dotfiles/emacs/irc-accounts.gpg")
              (load-library "~/.dotfiles/emacs/irc-accounts.gpg"))
            (add-hook 'erc-mode-hook 'visual-line-mode)
            (add-hook 'erc-mode-hook (lambda () (setq mode-name "irc")))))

(defun simpson-format-slack-name()
  "prepend the @ symbol in erc for slack"
  (interactive)
  (backward-word)
  (insert "@")
  (forward-word)
  (when (looking-at ":")
    (delete-char 1)
    (insert " ")))

(use-package emoji-cheat-sheet-plus
  :defer 2
  :init (progn
          (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
          (add-hook 'magit-mode-hook 'emoji-cheat-sheet-plus-display-mode)))

(use-package flyspell
  :diminish "spell"
  :defer 1
  :config (progn
            (add-hook 'erc-mode-hook (lambda () (flyspell-mode 1)))
            (setq flyspell-issue-message-flag nil)))

(when (file-exists-p "~/.dotfiles/emacs/authinfo.gpg")
  (setq auth-sources '("~/.dotfiles/emacs/authinfo.gpg")))

(use-package ivy
  :diminish ""
  :defer 1
  :if (not simpson-helm)
  :config (progn
            (setq ivy-use-virtual-buffers t)
            (ivy-mode)
            (setq ivy-height 20)
            (setq ivy-count-format "")
            (global-set-key (kbd "C-SPC A") 'ivy-resume)
            (define-key global-map (kbd "C-=") 'ivy-switch-buffer)
            (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
            (push '(counsel-M-x . "") ivy-initial-inputs-alist)
            (ivy-add-actions 'counsel-projectile-ag '(("O" simpson-other-window "open in new window")))
            (ivy-add-actions 'counsel-ag '(("O" simpson-other-window "open in new window")))
            (ivy-add-actions 'counsel-rg '(("O" simpson-other-window "open in new window")))))

(defun simpson-other-window(x)
  (let ((file (car (split-string x ":"))))
  (find-file-other-window (concat (locate-dominating-file file ".git") file))))

(use-package counsel
  :defer 1
  :if (not simpson-helm)
  :bind ("C-SPC f" . counsel-find-file)
  :config (progn
            (global-set-key (kbd "M-x") 'counsel-M-x)
            (global-set-key (kbd "<f1> f") 'counsel-describe-function)
            (global-set-key (kbd "<f1> v") 'counsel-describe-variable)))

(use-package counsel-projectile
  :if (not simpson-helm)
  :defer 1)

(use-package eyebrowse
  :defer 1
  :init (setq eyebrowse-keymap-prefix (kbd "C-SPC s"))
  :config (progn
            ;;use list-face-display to see all faces
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-foreground 'eyebrowse-mode-line-active (plist-get base16-ocean-colors :base0E)))
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(use-package elisp-mode
  :ensure nil
  :init (progn
          (defun simpson-pretty-lambda()
            "make the word lambda the greek character in elisp files"
            (setq prettify-symbols-alist '(("lambda" . 955))))

          (add-hook 'emacs-lisp-mode-hook 'simpson-pretty-lambda)
          (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
          (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "λ")))))


;;minor modes are set with diminish
;;major modes are changed in the mode hook using the variable mode-name
(use-package diminish
  :defer 1
  :config (progn
            (diminish 'smerge-mode)
            (diminish 'buffer-face-mode)
            (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
            (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
            (add-hook 'shell-mode-hook (lambda () (setq mode-name "shell")))
            (add-hook 'makefile-bsdmake-mode-hook (lambda () (setq mode-name "make")))))

(use-package swiper
  :defer 1
  :if (not simpson-helm)
  :bind ("C-SPC /" . swiper))

(use-package swiper-helm
  :defer 1
  :if simpson-helm
  :bind ("C-SPC /" . swiper-helm))

(use-package command-log-mode)

(use-package elisp-format)

(use-package gist)

(use-package notmuch)

(use-package desktop
  :defer 1
  :config (desktop-save-mode))

(use-package php-mode
  :mode ("\\.php?\\'" . php-mode)
  :config (add-hook 'php-mode-hook (lambda () (setq mode-name "php"))))

(use-package json-mode
  :mode ("\\.json?\\'" . json-mode))

(use-package company
  :defer 1
  :diminish ""
  :config (global-company-mode))

(defun simpson-sauron-toggle(&optional x)
  "A function to keep the sauron window visible and sized correctly after move/balance operation.
Optional argument to satisfy the various ways the evil-window-move- functions are called."
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
  :config (setq ivy-lobsters-keep-focus t)
  :load-path "~/Projects/ivy-lobsters")

(use-package ivy-feedwrangler
  :ensure nil
  :after ivy
  :load-path "~/Projects/ivy-feedwrangler/ivy-feedwrangler.el")

(use-package ox-confluence
  :defer 1
  :ensure nil
  :load-path "~/.dotfiles/emacs/ox-confluence.el")

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
            (global-set-key (kbd "C-SPC v") 'hydra-vimish/body)))

(defhydra hydra-mocha ()
  "
    Mocha:
    _a_ test at point
    _f_ test file
    _p_ test project
  "
  ("a" mocha-test-at-point "file at point")
  ("p" mocha-test-project "project" :exit t)
  ("f" mocha-test-file "whole file"))

(defun simpson-rg-switches(dir switches)
  (interactive
   (let (dir switches)
     (list
      (if (y-or-n-p "Pick dir?")
          (setq dir (read-directory-name "rg dir: "))
        (setq dir nil))
      (setq switches (read-string "rg switches: ")))
     (counsel-rg nil dir switches))))

(defhydra hydra-searching (:exit t)
  "
    ^Searching tools
    ----------------------------------------------
    _a_ ag without switches
    _A_ ag with extra switches
    _r_ rg without switches
    _R_ rg with extra switches

    ^silver searcher options
    ----------------------------------------------
    -Ghtml   - ag search by type
    --ignore - ag ignore file path

    ^ripgrep options
    ----------------------------------------------
    -g - search files matching glob, -g '*spec.js'
    -M - limit lines to N
    -t - search files matching type
       - rg --type-list lists all types
    -C - show context N lines
  "
  ("a" counsel-projectile-ag "projectile ag")
  ("A" simpson-counsel-ag "ag with switches")
  ("r" counsel-rg "projectile rg")
  ("R" simpson-rg-switches "rg with switches"))


(setq compilation-always-kill t)

(defun create-scratch-buffer ()
  "create a scratch buffer"
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
    _l_ see the current file's log
    _b_ blame the current file
  "
  ("f" magit-find-file "find file given rev")
  ("F" magit-find-file-other-window "find file in other window given rev")
  ("l" magit-log-buffer-file "view log for file")
  ("b" magit-blame "view blame for file"))

(use-package aggressive-indent
  :mode("\\.el?\\'" . aggressive-indent-mode))

(defun simpson-trash(file)
  "Prompt for file and trash it"
  (interactive
   (list (read-file-name "Files to trash: ")))
  (call-process "trash" nil nil nil file))

(defhydra hydra-js2 ()
  "
    JS2 Folding/Narrowing:
    _n_ narrow to defun
    _v_ fold
    _d_ highlight defun
  "
  ("n" js2-narrow-to-defun "narrow to defun" :exit t)
  ("v" vimish-fold "fold")
  ("d" js2-mark-defun "highlight defun"))

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
  :ensure nil
  :after hydra
  :load-path "~/Projects/ivy-window-configuration/ivy-window-configuration.el")

;;; settings.el ends here
