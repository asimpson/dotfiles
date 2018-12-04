;;; .emacs --- My Emacs customizations -*- lexical-binding: t; -*-

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(setq custom-file "~/.dotfiles/emacs/emacs-custom.el")
(add-to-list 'load-path "~/.dotfiles/emacs/")
(defvar simpson-helm nil
  "Variable to enable or disable helm specific configurations.")
(defvar simpson-evil t
  "Variable to enable or disable evil specific configurations.")
(setq idle-update-delay 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default
 use-package-always-defer t
 use-package-always-ensure t)

;;debug use-package 👇
;;(setq use-package-verbose t)

(tool-bar-mode -1)
(server-start)
(menu-bar-mode -1)
(show-paren-mode)
(scroll-bar-mode -1)
(setq visible-bell nil)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/lib/node_modules")))
(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "/Users/asimpson/.better-npm/lib/node_modules:/Users/asimpson/.better-npm/bin:/usr/local/bin:/usr/local/sbin")

(global-set-key (kbd "C-SPC") nil)

(defvar simpson-dropbox-path ""
  "Variable that points to the proper Dropbox path.")

(cond
 ((file-exists-p "~/Dropbox (Personal)/") (setq simpson-dropbox-path "~/Dropbox (Personal)/"))
 ((file-exists-p "~/Dropbox/") (setq simpson-dropbox-path "~/Dropbox/")))

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

(use-package osx-trash
  :if (eq system-type 'darwin)
  :defer 1
  :config (progn
            (osx-trash-setup)
            (setq delete-by-moving-to-trash t)))

(use-package base16-theme
  :if (display-graphic-p)
  :init (load-theme 'base16-dracula t))

(use-package tomorrow-theme
  :ensure nil
  :if (and (display-graphic-p) (file-exists-p "~/Projects/tomorrow-theme/GNU Emacs"))
  :disabled
  :load-path "~/Projects/tomorrow-theme/GNU Emacs"
  :init (require 'tomorrow-day-theme)
  :config (set-face-background 'highlight "lavender")
  (load-theme 'tomorrow-day t))

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
            (global-flycheck-mode)
            (setq flycheck-global-modes '(rjsx-mode emacs-lisp-mode rust-mode ruby-mode common-lisp-mode))
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
            (add-to-list 'evil-emacs-state-modes 'image-mode)
            (add-to-list 'evil-emacs-state-modes 'comint-mode)
            (add-to-list 'evil-emacs-state-modes 'eww-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-server-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-channel-mode)
            (add-to-list 'evil-emacs-state-modes 'sql-interactive-mode)
            (add-to-list 'evil-emacs-state-modes 'deadgrep-mode)
            ;;http://spacemacs.org/doc/FAQ#orgheadline31
            (fset 'evil-visual-update-x-selection 'ignore)
            (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
            (simpson-make-neutral evil-normal-state-map)
            (define-key evil-normal-state-map (kbd "gx") 'browse-url)
            (when simpson-helm (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file))
            (when simpson-helm (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-projectile-find-file))
            (unless simpson-helm (define-key evil-normal-state-map (kbd "SPC SPC") 'projectile-find-file-other-window))
            (unless simpson-helm (define-key evil-normal-state-map (kbd "\C-p") 'projectile-find-file-other-window))
            (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
            (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)
            (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
            (add-hook 'post-command-hook 'simpson-evil-mode)))

(eval-after-load 'image-mode (lambda() (simpson-make-neutral image-mode-map)))

(use-package evil-leader
  :if simpson-evil
  :after evil
  :defer 1
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader ",")
            (if simpson-helm
                (progn
                  (evil-leader/set-key "f" 'helm-projectile-ag)
                  (evil-leader/set-key "F" 'helm-do-ag))
              (evil-leader/set-key "f" 'counsel-rg)
              (evil-leader/set-key "s" 'hydra-searching/body)
              (evil-leader/set-key "F" 'simpson-counsel-ag))
            (evil-leader/set-key "c" 'fci-mode)
            (evil-leader/set-key "t" 'hydra-mocha/body)
            (evil-leader/set-key "v" 'simpson-vert-split)
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
            (if simpson-evil
                (progn (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                       (key-chord-define evil-normal-state-map "//" 'comment-region)
                       (key-chord-define evil-normal-state-map "??" 'uncomment-region)
                       (key-chord-define evil-normal-state-map "cc" 'simpson-magit-comment))
              (key-chord-define-global "jk" 'god-local-mode))))

(defun simpson-magit-comment()
  "A small wrapper around 'comment-line' keychord for magit.
  Mashing cc in a magit-status window triggers my custom keybind
  to (comment-line) this function checks what mode is current and then either
  comments or commit"
  (interactive)
  (if (string= major-mode "magit-status-mode")
      (magit-commit)
    (comment-line 1)))

(use-package god-mode
  :if (not simpson-evil)
  :defer 1
  :config (progn
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

(use-package magit
  :pin melpa-stable
  :defer 1
  :bind ("C-SPC g" . magit-status)
  :config (progn
            ;;https://github.com/magit/magit/pull/2513
            ;;Users who use Tramp and experience delays, should consider setting
            ;;the option to `magit-auto-revert-repository-buffer-p'.
            (setq auto-revert-buffer-list-filter nil)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (put 'magit-clean 'disabled nil)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)
            (setq magit-commit-show-diff nil)
            (setq magit-log-section-commit-count 0)
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
  :defer 1
  :after evil
  :if simpson-evil)

(use-package diff-hl
  :defer 1
  :bind (("C-SPC r" . diff-hl-revert-hunk)
         ("C-x p" . diff-hl-previous-hunk)
         ("C-x n" . diff-hl-next-hunk)) :config (progn
         (global-diff-hl-mode)
         (cond
          ((string= (car custom-enabled-themes) "base16-dracula") (progn
                                                                    (set-face-background 'diff-hl-change (plist-get base16-dracula-colors :base0C))
                                                                    (set-face-background 'diff-hl-insert (plist-get base16-dracula-colors :base0B))
                                                                    (set-face-background 'diff-hl-delete (plist-get base16-dracula-colors :base09))))
          ((string= (car custom-enabled-themes) "tomorrow-day") (progn
                                                                  (set-face-background 'diff-hl-delete "red3")
                                                                  (set-face-background 'diff-hl-insert "LightGreen")))
          ((string= (car custom-enabled-themes) "base16-ocean") (progn
                                                                  (set-face-background 'diff-hl-change (plist-get base16-ocean-colors :base0C))
                                                                  (set-face-background 'diff-hl-insert (plist-get base16-ocean-colors :base0B))
                                                                  (set-face-background 'diff-hl-delete (plist-get base16-ocean-colors :base09)))))))

(use-package org
  :defer 2
  :if (file-exists-p (concat simpson-dropbox-path "org/tasks.txt"))
  :pin org
  :bind (("C-SPC c" . org-capture)
         ("C-SPC k B" . simpson-org-blog-capture)
         ("C-SPC t" . org-todo-list)
         ("C-SPC a" . org-agenda)
         ("C-SPC T" . org-tags-view))
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
            (setq org-agenda-custom-commands '(("n" "Agenda and all TODOs"
                                                ((agenda "")
                                                 (alltodo "")))
                                               ("w" "Work tasks" ((agenda "") (todo ""))
                                                ((org-agenda-files `(,(concat simpson-dropbox-path "org/tasks.txt")))))
                                               ("s" "side projects" ((agenda "") (todo ""))
                                                ((org-agenda-files `(,(concat simpson-dropbox-path "org/side.txt")))))))
            (setq org-refile-targets '(
                                       (nil . (:level . 1))
                                       (nil . (:level . 2))
                                       (org-agenda-files . (:level . 1))))
            (setq org-todo-keywords
                  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
            (setq org-capture-templates
                  `(("a" "Add to tasks" entry
                     (file ,(concat simpson-dropbox-path "org/tasks.txt"))
                     "* TODO %? %^g
  :PROPERTIES:
  :CREATED: %T
  :END:")

                    ("s" "Side projects tracker" entry
                     (file ,(concat simpson-dropbox-path "org/side.txt"))
                     "* TODO %? %^g
  :PROPERTIES:
  :CREATED: %T
  :END:")
                    ("b" "My blog post captures" entry
                     (file ,(concat simpson-dropbox-path "org/reading.txt"))
                     "* %? %^g
  :PROPERTIES:
  %(simpson-prompt-for-feedwrangler-url)
  :CREATED: %T
  :END:")
                    ("p" "Personal: " entry
                     (file ,(concat simpson-dropbox-path "org/personal.txt"))
                     "* %? %^g
  :PROPERTIES:
  :CREATED: %T
  :END:")

                    ("t" "Travel information" entry
                     (file ,(concat simpson-dropbox-path "org/travel.txt"))
                     "* %? %^G
  :PROPERTIES:
  :CREATED: %T
  :END:
** Email message: %a")))
            (setq org-agenda-restore-windows-after-quit t)
            (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
            (add-hook 'org-mode-hook 'visual-line-mode)
            (add-hook 'org-mode-hook (lambda () (setq mode-name "org")))
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
             '((shell . t)
               (emacs-lisp . t)
               (plantuml . t)
               (js . t)))
            (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")
            (setq org-confirm-babel-evaluate (lambda (lang src) (not (string-equal lang "plantuml"))))
            (add-to-list
             'org-src-lang-modes '("plantuml" . plantuml))
            (run-at-time 0 (* 60 15) #'simpson-org-refresh)
            (if (string= (car custom-enabled-themes) "base16-ocean")
                (progn
                  (set-face-foreground 'org-link (plist-get base16-ocean-colors :base0B))
                  (set-face-foreground 'org-tag (plist-get base16-ocean-colors :base0A))
                  (set-face-foreground 'org-agenda-structure (plist-get base16-ocean-colors :base03))
                  (set-face-attribute 'org-mode-line-clock nil :foreground (plist-get base16-ocean-colors :base0E) :background nil :box nil :inherit nil))
              (set-face-attribute 'org-mode-line-clock nil :foreground nil :background nil :box nil :inherit nil))
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
  (let ((buffers '("tasks.txt" "refile-beorg.txt")))
    (seq-each (lambda(buf)
                (when (buffer-live-p (get-buffer buf))
                  (set-buffer buf)
                  (revert-buffer t t))) buffers)))

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
            (setq markdown-command "/usr/local/bin/pandoc")
            (setq markdown-live-preview-delete-export t)
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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

(set-face-attribute 'default nil :font "Hack-12" )
(set-frame-font "Hack-12" nil t)

;;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(use-package yaml-mode
  :mode (("\\.yml?\\'" . yaml-mode)))

;;each line gets one line
(set-default 'truncate-lines t)

(setq make-backup-files nil)

(setq auto-save-default nil)

(setq inhibit-splash-screen t)

(defun markdown-fonts ()
  "Use monospaced font faces in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack" :height 120))
  (buffer-face-mode))

(use-package visual-fill-column
  :defer 1
  :config (progn
            (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
            (setq-default visual-fill-column-width 160)))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(setq header-line-format nil)
(set-face-attribute 'header-line nil :background "white" :box nil)

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-foreground 'vertical-border (plist-get base16-ocean-colors :base02))
  (set-face-background 'fringe (plist-get base16-ocean-colors :base00)))

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
                                                                '(:eval simpson-mail-count)
                                                                '(:eval simpson-software-update)
                                                                " "
                                                                mode-line-modes
                                                                mode-line-misc-info))))

(defvar simpson-mail-count nil)
(defvar simpson-software-update nil)

(defun simpson-check-mail()
  "Stolen from https://github.com/iqbalansari/mu4e-alert/blob/master/mu4e-alert.el#L288."
  (let (num (output (shell-command-to-string simpson-mail-update-command)))
    (if (> (length output) 0)
        (progn
          (setq num (number-to-string (length (split-string output "\n" t))))
          (setq simpson-mail-count (concat " " (propertize "mail"
                                                           'display display-time-mail-icon
                                                           'face display-time-mail-face
                                                           'help-echo (concat num " " "unread mail")
                                                           'keymap '(mode-line keymap
                                                                               (mouse-1 . mu4e)))
                                           " " num)))
      (setq simpson-mail-count nil))))

(defun simpson-update-check()
  "Process that check for software update on macOS."
  (set-process-sentinel (start-process "softwareupdate" "*softwareupdate*" "softwareupdate" "-l") 'simpson-update-check--sentinel))

(defun simpson-update-check--sentinel(proc msg)
  "Search for word recommended in MSG in BUFFER output to determine update.
  PROC is not used."
  (when (and (string= msg "finished\n") (buffer-live-p (get-buffer "*softwareupdate*")))
    (if (with-current-buffer "*softwareupdate*"
          (goto-char (point-min))
          (search-forward "recommended" nil t))
        (setq simpson-software-update "*")
      (setq simpson-software-update nil)
      (kill-buffer "*softwareupdate*")))
  (run-at-time (* 60 60) nil #'simpson-update-check))

(run-at-time 0 nil #'simpson-update-check)

(set-face-attribute 'mode-line nil :height 1.0 :box nil)

(defun modeline-theme()
  (when (string= (car custom-enabled-themes) "base16-ocean")
    (set-face-attribute 'mode-line nil
                        :background (plist-get base16-ocean-colors :base06)
                        :foreground (plist-get base16-ocean-colors :base01)
                        :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base06) :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base01) :style nil)))

  (when (string= (car custom-enabled-themes) "tomorrow-day")
    (set-face-attribute 'mode-line nil
                        :background "khaki1" :box '(:line-width 5 :color "khaki1"))
    (set-face-attribute 'mode-line-inactive nil
                        :background "LightCyan2" :box '(:line-width 5 :color "LightCyan2")))

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

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-background 'trailing-whitespace (plist-get base16-ocean-colors :base0F)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :diminish ""
  :defer 1
  :config (which-key-mode))

(use-package fill-column-indicator
  :config (setq fci-rule-column 80))

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
  :defer 1
  :init (progn
          (add-hook 'js2-mode-hook #'yas-minor-mode)
          (add-hook 'rjsx-mode-hook #'yas-minor-mode)
          (add-hook 'ruby-mode-hook #'yas-minor-mode)
          (add-hook 'org-mode-hook #'yas-minor-mode))
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
  :pin melpa-stable
  :after sauron-ams-org
  :config (progn
            (setq sauron-modules '(sauron-erc sauron-ams-org))
            (simpson-load-file "~/.dotfiles/emacs/irc-watch.gpg")
            (when simpson-evil (add-to-list 'evil-emacs-state-modes 'sauron-mode))
            (setq sauron-watch-nicks nil)
            (when (boundp 'simpson-watch-patterns)
              (setq sauron-watch-patterns simpson-watch-patterns))
            (setq sauron-hide-mode-line t)
            (setq sauron-separate-frame t)
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

;;http://emacs.stackexchange.com/a/58
;;to open a file with sudo, invoke C-x C-f and then type /sudo::/path

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
            (define-key dired-mode-map "E" 'epa-dired-do-decrypt)))

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

(use-package prettier-js
  :diminish "pretty"
  :defer 1
  :init (progn
          (add-hook 'js2-mode-hook 'prettier-js-mode)
          (add-hook 'rjsx-mode-hook 'prettier-js-mode))
  :config (progn
            (setq prettier-js-command "prettier-eslint")
            (setq prettier-js-args '(
                                     "--trailing-comma" "es5"
                                     "--bracket-spacing" "true"
                                     "--single-quote" "true"))))

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
            (simpson-make-neutral erc-mode-map)
            (setq erc-modules '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp))
            (simpson-load-file "~/.dotfiles/emacs/irc-accounts.gpg")
            (add-hook 'erc-mode-hook 'visual-line-mode)
            (add-hook 'erc-mode-hook (lambda () (setq mode-name "irc")))))

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
            (define-key dired-mode-map "r" 'counsel-rg)
            (ivy-add-actions 'counsel-projectile-ag '(("O" simpson-other-window "open in new window")))
            (ivy-add-actions 'counsel-ag '(("O" simpson-other-window "open in new window")))
            (ivy-add-actions 'counsel-rg '(("O" simpson-other-window "open in new window")))))

(defun simpson-other-window(x)
  (let* ((string (split-string x ":"))
         (num (string-to-number (car (cdr string))))
         (file (car string)))
    (find-file-other-window (concat (locate-dominating-file file ".git") file))
    (with-current-buffer (get-buffer (file-name-nondirectory file))
      (goto-line num))))

(use-package counsel
  :defer 1
  :if (not simpson-helm)
  :bind ("C-SPC f" . counsel-find-file)
  :config (progn
            (global-set-key (kbd "M-x") 'counsel-M-x)
            (define-key dired-mode-map "f" 'counsel-find-file)
            (ivy-add-actions 'counsel-find-file '(("D" simpson-delete "delete")))
            (ivy-add-actions 'counsel-find-file '(("h" (lambda(file) (dired (file-name-directory file))) "Dired")))
            (global-set-key (kbd "<f1> f") 'counsel-describe-function)
            (global-set-key (kbd "<f1> v") 'counsel-describe-variable)))

(use-package counsel-projectile
  :if (not simpson-helm)
  :defer 1)

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
            (when (string= (car custom-enabled-themes) "base16-ocean")
              (set-face-foreground 'eyebrowse-mode-line-active (plist-get base16-ocean-colors :base0E)))
            (eyebrowse-mode t)
            (set-face-foreground 'eyebrowse-mode-line-active "green4")
            (setq eyebrowse-mode-line-style nil)
            (global-set-key (kbd "C-SPC s t") 'simpson-browse)
            (setq eyebrowse-new-workspace t)))

(use-package elisp-mode
  :ensure nil
  :init (progn
          (defun simpson-pretty-lambda()
            "make the word lambda the greek character in elisp files"
            (setq prettify-symbols-alist '(("lambda" . 955))))
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
  :if (not simpson-helm)
  :bind ("C-SPC /" . swiper))

(use-package swiper-helm
  :defer 1
  :if simpson-helm
  :bind ("C-SPC /" . swiper-helm))

(use-package command-log-mode)

(use-package elisp-format)

(use-package gist)

(use-package desktop
  :defer 1
  :if (display-graphic-p)
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
  :after org)

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

(defun simpson-rg-switches(dir switches)
  (interactive
   (let (dir switches)
     (list
      (when (y-or-n-p "Pick dir? ")
        (setq dir (read-directory-name "rg dir: ")))
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

(defun simpson-delete(x)
  (call-process "trash" nil nil nil x))

(defun simpson-trash(file)
  "Prompt for FILE and trash it."
  (interactive
   (list (read-file-name "Files to trash: ")))
  (simpson-delete file))

(defhydra hydra-js2 ()
  "
    JS2 Folding/Narrowing:
    _n_ narrow to defun
    _v_ fold
    _o_ org narrow to sub tree
    _d_ highlight defun
  "
  ("n" js2-narrow-to-defun "narrow to defun" :exit t)
  ("v" vimish-fold "fold")
  ("o" org-narrow-to-subtree "org narrow to subtree" :exit t)
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
  :after ivy
  :if (file-exists-p "~/Projects/ivy-window-configuration/")
  :load-path "~/Projects/ivy-window-configuration/")

(use-package indium
  :after evil
  :config (progn
            (when simpson-evil (add-to-list 'evil-emacs-state-modes 'indium-repl-mode))
            (simpson-make-neutral indium-repl-mode-map)
            (advice-add 'indium-scratch-setup-buffer :after #'simpson-indium-emacs)))

(defun simpson-indium-emacs (buf)
  "Run after the scratch BUF is setup and add 'use-strict' mode."
  (with-current-buffer buf
    (evil-emacs-state)
    (insert "'use strict';")))

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
                      (setq inferior-lisp-program "/usr/local/bin/sbcl")))

(use-package slime-company
             :defer 1)

(use-package company-quickhelp
             :defer 1
             :config (company-quickhelp-mode))

(defun simpson-org-to-todo()
  "Convert a line (or region) in an org file to a TODO."
  (interactive)
  (let ((heading "") (i 1) (number (read-number "What level?" 1)))
    (while (<= i number)
           (setq heading (concat heading "*"))
           (setq i (+ i 1)))
    (if (region-active-p)
        (let ((strings (seq-map (lambda(x) (concat heading " TODO " x))
                                (split-string (buffer-substring-no-properties (region-beginning) (region-end)) "\n" t))))
          (delete-active-region)
          (insert (mapconcat 'identity strings "\n")))
        (org-beginning-of-line)
        (insert heading " TODO ") t)))

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

(defhydra hydra-eww (:exit t)
  "
    Go forth and browse...text
    _r_ eww-more-readable
    _R_ eww-readable
    _b_ eww-back-url
    _h_ eww-history-browse
    _c_ simpson-eww-external
    _g_ eww
  "
  ("r" eww-more-readable "better readable")
  ("R" eww-readable "default readable")
  ("b" eww-back-url "eww back" :exit nil)
  ("g" eww "eww")
  ("c" simpson-eww-external "simpson-eww-external")
  ("h" eww-history-browse "browse history"))

(defun simpson-eww-external()
  (interactive)
  (browse-url-default-browser (get-text-property (point) 'shr-url)))

(use-package rust-mode
  :mode("\\.rs?\\'" . rust-mode)
  :diminish ""
  :config(progn
           (setq rust-indent-offset 2)
           (add-hook 'rust-mode-hook (lambda () (setq mode-name "rust")))))

(use-package cargo
  :defer 1
  :config(progn
           (setq cargo-process--command-fmt "fmt -- --force")
           (add-hook 'rust-mode-hook 'cargo-minor-mode)))

(use-package flycheck-rust
  :defer 1
  :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package racer
  :defer 1
  :config (progn
            (setq racer-cmd "~/.cargo/bin/racer")
            (setq racer-rust-src-path "/Users/asimpson/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
            (add-hook 'rust-mode-hook 'racer-mode)
            (add-hook 'racer-mode-hook 'eldoc-mode)
            (add-hook 'racer-mode-hook 'company-mode)))

(use-package helpful)

(defun simpson-shell-history()
  "Interact with shell-command-history through Ivy"
  (interactive)
  (ivy-read "Run previous commands:"
            shell-command-history
            :action (lambda(x)
                      (push x shell-command-history)
                      (delete-dups shell-command-history)
                      (async-shell-command x))))

(with-eval-after-load ".emacs"
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/shell-history")
    (setq shell-command-history (split-string (buffer-string) "\n"))))

(defun simpson-save-history()
  "Write contents of 'shell-command-history' to ~/.emacs.d/shell-history."
  (with-temp-buffer
    (insert (mapconcat 'identity shell-command-history "\n"))
    (write-file "~/.emacs.d/shell-history")))

(add-hook 'kill-emacs-hook 'simpson-save-history)

(use-package dockerfile-mode
  :config (add-hook 'dockerfile-mode-hook (lambda() (setq mode-name "dockerfile")))
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package suggest)

;; mu4e-bookmarks, mu4e-contexts, mu4e-user-mail-address are all set in mu4e.gpg
(use-package mu4e
  :ensure nil
  :defer 1
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config (progn
            (simpson-load-file "~/.dotfiles/emacs/mu4e.el.gpg")
            (set-face-attribute 'mu4e-highlight-face nil :background "DarkRed" :foreground nil)
            (setq mu4e-maildir "~/Mail")
            (setq mu4e-view-show-images t)
            (setq send-mail-function 'smtpmail-send-it)
            (setq message-send-mail-function 'smtpmail-send-it)
            (setq message-kill-buffer-on-exit t)
            (setq mu4e-context-policy 'pick-first)
            (setq mu4e-compose-format-flowed t)
            (setq mu4e-view-show-addresses 't)
            (setq smtpmail-stream-type 'ssl)
            (setq mu4e-get-mail-command "true")
            (setq mu4e-update-interval 300)
            (setq smtpmail-smtp-service 465)
            (setq user-full-name "Adam Simpson")
            (setq mu4e-confirm-quit nil)
            (setq mu4e~view-html-text 'text)
            (setq mu4e-change-filenames-when-moving t)
            (setq mu4e-headers-include-related nil)
            (add-to-list 'mu4e-view-actions
                         '("ViewInBrowser" . mu4e-action-view-in-browser) t)
            (define-key mu4e-headers-mode-map (kbd "C-c C-u") 'mu4e-update-index)
            (define-key mu4e-main-mode-map "q" 'simpson-mu4e-quit)
            (add-hook 'mu4e-view-mode-hook (lambda() (text-scale-set 1)))
            (simpson-make-neutral mu4e-headers-mode-map)
            (simpson-make-neutral--keys mu4e-headers-mode-map)
            (simpson-make-neutral--keys mu4e-view-mode-map)
            (run-at-time 0 (* 60 3) #'simpson-check-mail)))

(defun simpson-mu4e-quit()
  "Quit mu4e and then update mail icon."
  (interactive)
  (mu4e-quit)
  (simpson-check-mail))

(use-package org-mu4e
  :ensure nil
  :defer 1
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(defmacro json-parse! (buffer)
  "Parse and return JSON from BUFFER.  Ideally for the 'url-retrieve' family of funcs."
  `(with-current-buffer ,buffer (json-read-from-string (buffer-substring-no-properties url-http-end-of-headers (point-max)))))

(defun ltc()
  "Get LTC price from coinbase API via synchronous url retrieve."
  (interactive)
  (let ((data (json-parse! (url-retrieve-synchronously "https://api.coinbase.com/v2/prices/LTC-USD/spot" t))))
    (message "LTC: $%s" (alist-get 'amount (car data)))))

(defun new-ltc()
  "Get LTC price from coinbase API via async url retrieve."
  (interactive)
  (with-temp-buffer (url-retrieve "https://api.coinbase.com/v2/prices/LTC-USD/spot"
                                  (lambda(_) (let ((data (json-parse! (current-buffer))))
                                               (message "LTC: $%s" (alist-get 'amount (car data))))))))

(defun simpson-lambda(file)
  (interactive
   (list (read-file-name "Which lambda?" "/Users/asimpson/Projects/blog-admin/packages/node_modules/@lambdas")))
  (async-shell-command (concat "node run.js " (file-name-base file))))

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

(use-package emacs-micro-blog
  :commands post-to-micro
  :ensure nil
  :load-path "~/Projects/emacs-micro-blog")

(use-package inf-ruby)

(use-package tide
  :defer 3
  :config (progn
            (defun setup-tide-mode()
              (tide-setup)
              (eldoc-mode)
              (tide-hl-identifier-mode))
            (add-hook 'rjsx-mode-hook 'setup-tide-mode)))

(use-package org-mime
  :config (require 'org-mime))

(use-package rainbow-mode
  :diminish "")

(use-package persistent-scratch
  :defer 1
  :config (progn
            (persistent-scratch-setup-default)
            (persistent-scratch-autosave-mode)))

(setq warning-suppress-types '(undo discard-info))

(defun simpson-clean-shell()
  (font-lock-mode)
  (undo-tree-mode))

(add-hook 'shell-mode-hook 'simpson-clean-shell)

(setq eshell-command-completion-function 'completion-at-point)

(use-package elquery)

(use-package package-lint)

(use-package flymake-json
  :init (add-hook 'json-mode-hook 'flymake-json-load))

(use-package osx-clipboard
  :defer 1
  :if (eq system-type 'darwin)
  :diminish "")

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

(set-face-background 'line-number nil)

(pixel-scroll-mode)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

(setq frame-title-format nil)

(use-package org-preview-html
  :after org)

(use-package alert
  :config (progn
            (setq alert-log-messages nil)
            (setq alert-default-style 'notifier)))

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

(defun gen-multi-term ()
  "Open up a mult-term in a new window."
  (interactive)
  (switch-to-buffer-other-window nil)
  (multi-term))

(defun simpson-pretty-json()
  "Ideal for getting pretty JSON from JSON that is copied from a XHR request."
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (json-pretty-print-buffer)
    (kill-new (buffer-string))))

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
(global-set-key (kbd "C-SPC B") 'simpson-get-git-url)
(global-set-key (kbd "C-SPC k P") 'simpson-pretty-json)
(global-set-key (kbd "C-SPC k t") 'gen-multi-term)
(global-set-key (kbd "C-SPC k r") 'vc-revert-buffer)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k n") 'simpson-smart-shell)
(global-set-key (kbd "C-SPC k N") 'kill-shell-buffer)
(global-set-key (kbd "C-SPC k g") 'ffap)
(global-set-key (kbd "C-SPC k F") 'simpson-byte-compile)

(defun simpson-byte-compile()
  "Force byte compilation."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun simpson-copy-file-buffer-name()
  "Copy the file path for the current buffer to the clipboard."
  (interactive)
  (let (path)
    (if (projectile-project-p)
        (setq path (nth 1 (split-string (buffer-file-name) (projectile-project-name))))
      (setq path (buffer-file-name)))

    (if (y-or-n-p "Code formatting? ")
        (kill-new (concat "`"path"`"))
      (kill-new path))))

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
         (date (replace-regexp-in-string "\n$" "" (shell-command-to-string "date +%m-%d-%y")))
         (file (concat simpson-dropbox-path "Notational Data/" date "-" name ".txt")))
    (if use-buf
        (write-file file)
      (write-region "" "" file))))

(global-set-key (kbd "C-SPC d") (lambda() (interactive)
                                  (dired (concat simpson-dropbox-path "Notational Data/") "-laGht")))

(defun kill-shell-buffer()
  "Kill the Async Shell Command buffer and then balance's the remaining windows."
  (interactive)
  (switch-to-buffer-other-window "*Async Shell Command*")
  (kill-buffer-and-window)
  (balance-windows))

(defun simpson-smart-shell()
  "Run shell from projectile root or from current spot in system."
  (interactive)
  (unless (ignore-errors (projectile-run-async-shell-command-in-root))
    (call-interactively 'async-shell-command)))

(defun simpson-rerun()
  "Rerun last shell command.  Respsects projectile."
  (interactive)
  (if (projectile-project-p)
      (projectile-with-default-dir (projectile-project-root)
        (async-shell-command (car shell-command-history)))
    (async-shell-command (car shell-command-history))))

(global-set-key (kbd "C-SPC .") 'simpson-rerun)

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

(defun simpson-get-git-url()
  "Grab the remote url for origin and assume it's a github url.
Open the url in the default browser"
  (interactive)
  (let (url repo)
    (setq url (shell-command-to-string "git remote get-url origin"))
    (setq repo (nth 0 (split-string (nth 1 (split-string url ":")) "\\.")))
    (shell-command (concat "open https://github.com/" repo) t)))

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

(use-package ivy-hacker-news
  :load-path "~/Projects/ivy-hacker-news"
  :commands ivy-hacker-news
  :ensure nil
  :after ivy)

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
  :mode ("*spec.rb" . rspec-mode))

(use-package web-mode
  :defer 1
  :mode ("\\.html\\.erb\\'" . web-mode)
  :config (setq web-mode-markup-indent-offset 2))

(setq battery-mode-line-format "[%b%p%%/%t]")
(setq battery-mode-line-limit 90)
(display-battery-mode)

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
                       (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")
                       (define-key plantuml-mode-map (kbd "C-c C-c") 'simpson-plantuml-preview)))

  (message "Init time: %s" (emacs-init-time))
;;; .emacs ends here
