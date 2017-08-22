(use-package notmuch)
;debug use-package 👇
;(setq use-package-verbose t)

;turn off toolbar
(show-paren-mode)
(tool-bar-mode -1)

(setq visible-bell nil)

(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/lib/node_modules")))
(setq exec-path (append exec-path '("/Users/asimpson/.better-npm/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "/Users/asimpson/.better-npm/lib/node_modules:/Users/asimpson/.better-npm/bin:/usr/local/bin:/usr/local/sbin")

(setq ring-bell-function (lambda ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line)
))

;no scrollbars, what is this a GUI?!
(scroll-bar-mode -1)

(global-set-key (kbd "C-SPC") nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default
  use-package-always-defer t
  use-package-always-ensure t)

(use-package osx-trash
  :if (eq system-type 'darwin)
  :config (progn
    (osx-trash-setup)
    (setq delete-by-moving-to-trash t)
  )
)

(use-package base16-theme
  :init (load-theme 'base16-ocean-dark t)
)

(use-package exec-path-from-shell
  :defer 2
  :config (progn
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))
  )
)

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy))
)

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package vimish-fold
  :config (vimish-fold-global-mode 1)
  :bind (("C-SPC v" . vimish-fold)
         ("C-SPC V" . vimish-fold-delete))
)

(use-package flycheck
  :diminish "lint"
  :defer 1
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-SPC '" . flycheck-mode)
  :config (setq flycheck-global-modes '(rjsx-mode emacs-lisp-mode json-mode))
)

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
    ;http://spacemacs.org/doc/FAQ#orgheadline31
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
    (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)

  )
)

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
    (unless simpson-helm (evil-leader/set-key "f" 'counsel-ag))
    (evil-leader/set-key "c" 'fci-mode)
    (evil-leader/set-key "v" 'evil-window-vnew)
    (evil-leader/set-key "x" 'evil-window-new)
  )
)

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
    (unless simpson-evil (key-chord-define-global "jk" 'god-local-mode))
  )
)

(defun simpson-magit-comment()
  "mashing cc in a magit-status window triggers my custom keybind to (comment-line)
   this function checks what mode is current and then either comments or commit"
  (interactive)
  (if (string= major-mode "magit-status-mode")
    (magit-commit)
  (comment-line 1))
)

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
      (define-key god-local-mode-map (kbd "F") 'helm-projectile-ag)))
)

(use-package evil-matchit
  :if simpson-evil
  :after evil
  :config (progn
     (global-evil-matchit-mode 1)
     (plist-put evilmi-plugins 'handlebars-mode '((evilmi-simple-get-tag evilmi-simple-jump)))
  )
)

(use-package helm
  :if simpson-helm
  :diminish ""
  :bind (
    ("M-x" . helm-M-x)
    ("C-=" . helm-mini)
    ("C-SPC f" . helm-find-files)
    ("C-SPC k p" . simpson-projects-browser)
  )
  :config (progn
    (helm-mode)
    (set-face-background 'helm-ff-dotted-directory "#2b303b")
    (set-face-background 'helm-ff-dotted-symlink-directory "#2b303b")
    (set-face-foreground 'helm-ff-dotted-directory "#65737e")
    (set-face-foreground 'helm-ff-dotted-symlink-directory "#65737e"))
)

(use-package projectile
  :diminish ""
  :bind (("C-SPC b" . projectile-switch-project)
         ("C-c C-p" . projectile-find-file-other-window))
  :config (progn
    (projectile-global-mode)
    (setq projectile-enable-caching nil)
    (setq projectile-switch-project-action 'projectile-find-file)
    (setq projectile-completion-system (if simpson-helm 'helm 'ivy))
  )
)

(use-package helm-projectile
  :after projectile
  :if simpson-helm
  :config (progn
    (helm-projectile-on)
    (setq projectile-switch-project-action 'projectile-find-file)
    (setq projectile-completion-system (if simpson-helm 'helm 'ivy))
   )
)

(use-package helm-ag
  :if simpson-helm
  :after helm
  :init (setq helm-ag-base-command "ag --nocolor --nogroup")
)

(use-package helm-flyspell
  :diminish "spell"
  :if simpson-helm
  :after helm
  :bind ("C-SPC C" . helm-flyspell-correct)
)

(use-package flyspell-correct-ivy
  :diminish "spell"
  :if (not simpson-helm)
  :after ivy
  :bind ("C-SPC C" . flyspell-correct-previous-word-generic)
)

(defun simpson-projects-browser()
  (interactive)
  (cd "~/Projects/")
  (helm-find-files nil)
)

(use-package magit
  :pin melpa-stable
  :defer 1
  :bind ("C-SPC g" . magit-status)
  :config (progn
    ;https://github.com/magit/magit/pull/2513
    ;Users who use Tramp and experience delays, should consider setting
    ;the option to `magit-auto-revert-repository-buffer-p'.
    (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (set-face-foreground 'magit-blame-date "#ebcb8b")
    (set-face-foreground 'magit-blame-hash "#ebcb8b")
    (set-face-foreground 'magit-blame-heading "#ebcb8b")
    (set-face-foreground 'magit-blame-name "#ebcb8b")
    (set-face-foreground 'magit-blame-summary "#ebcb8b")
    (set-face-foreground 'magit-sequence-onto "#ebcb8b")
    (set-face-foreground 'magit-sequence-done "#ebcb8b")

    (set-face-foreground 'magit-hash "#96b5b4")
    (set-face-background 'magit-section-highlight "#343d46")
  )
)

(use-package evil-magit
  :pin melpa-stable
  :after evil
  :if simpson-evil
)

(use-package diff-hl
  :bind (
    ("C-SPC r" . diff-hl-revert-hunk)
    ("C-x p" . diff-hl-previous-hunk)
    ("C-x n" . diff-hl-next-hunk)
  )
  :config (progn
    (global-diff-hl-mode)
    (set-face-background 'diff-hl-change "#96b5b4")
    (set-face-background 'diff-hl-insert "#a3be8c")
    (set-face-background 'diff-hl-delete "#d08770")
  )
)

(use-package auto-complete
  :diminish ""
  :demand t
  :init (progn
    (ac-config-default)
    ;this prevents the stupid behavior in scss where &:before {___ autocompletes
    ;lang!
    (defconst ac-css-pseudo-classes nil)
  )
)

(use-package org
  :defer 2
  :bind (
    ("C-SPC c" . simpson-org-task-capture)
    ("C-SPC t" . org-todo-list)
    ("C-SPC a" . org-agenda)
    ("C-SPC T" . org-tags-view)
  )
  :mode (
    ("\\.txt\\'" . org-mode)
  )
  :config (progn
    (require 'org-notmuch)
    (require 'ox-md)
    ;look into swapping with txt, org-agenda-file-regexp
    (setq org-agenda-files '("~/Dropbox (Personal)/org"))
    (setq org-log-done t)
    (setq org-deadline-warning-days 3)
    (setq org-export-with-toc nil)
    (setq org-todo-keywords
          '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
    (setq org-capture-templates
          '(("a" "My TODO task format." entry
            (file "~/Dropbox (Personal)/org/tasks.org")
            "* TODO %? %^g
    :PROPERTIES:
    :CREATED: %T
    :END:")))
    (setq org-agenda-restore-windows-after-quit t)
    (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook (lambda () (setq mode-name "org")))
    (set-face-foreground 'org-link "#a3be8c")
    (set-face-foreground 'org-tag "#ebcb8b")
    (set-face-foreground 'org-agenda-structure "#65737e")
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
      </style>
    ")
    (setq exec-path (append exec-path '("/Library/TeX/texbin/latex")))
    (defun simpson-org-task-capture ()
      "Capture a task with my default template."
      (interactive)
      (org-capture nil "a"))
    (global-set-key (kbd "C-SPC k f") 'org-footnote-new)
    (global-set-key (kbd "C-SPC k l") 'org-toggle-link-display)
    (setq org-export-backends '(ascii html icalendar latex odt md))
    (org-babel-do-load-languages
    'org-babel-load-languages
      '( (sh . t)
        (js . t)
      )
    )

    (defun simpson-org-refresh()
      (interactive)
      (set-buffer "tasks.org")
      (revert-buffer t t))

    (run-at-time 0 (* 60 15) #'simpson-org-refresh)
  )
)

(use-package multi-term
  :config (progn
    (setq multi-term-program "/bin/zsh")
    (setq multi-term-program-switches "--login")
    (define-key global-map (kbd "C-SPC p") 'term-paste)
  )
)

(use-package markdown-mode
  :mode (
    ("\\.md\\'" . markdown-mode)
  )
  :config (progn
    ;add custom fonts for markdown mode
    (add-hook 'markdown-mode-hook 'markdown-fonts)
    ;toggle on visual line mode for writing
    (add-hook 'markdown-mode-hook 'visual-line-mode)
    ;toggle on spell-check for writing
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
    (add-hook 'markdown-mode-hook (lambda () (setq mode-name "md")))
    (setq markdown-open-command "/usr/local/bin/marked")
  )
)

(use-package js2-mode
  :disabled
  :interpreter (
    ("node" . js2-mode)
  )
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
      :group 'js2-mode)
  )
)

(use-package rjsx-mode
  :interpreter (
    ("node" . rjsx-mode)
  )
  :mode (
    ("\\.js?\\'" . rjsx-mode)
    ("\\.jsx?\\'" . rjsx-mode)
  )
  :config (progn
    (setq js2-basic-offset 2)
    (setq js2-highlight-level 3)
    (setq js2-bounce-indent-p t)
    (electric-indent-mode -1)
    (setq js2-mode-show-strict-warnings nil)
    (add-hook 'js2-mode-hook (lambda() (setq show-trailing-whitespace t)))
    (add-hook 'rjsx-mode-hook (lambda() (setq mode-name "jsx")))
  )
)

;indents! so brutal, each mode can have their own, e.g. css
;spaces
(setq-default indent-tabs-mode nil)
;2 of em
(setq-default tab-width 2)
;yes, css, even you
(setq-default css-indent-offset 2)

;fonts
(set-face-attribute 'default nil :font "Hack-10" )
(set-frame-font "Hack-10" nil t)

;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;each line gets one line
(set-default 'truncate-lines t)

;backups suck, use Git
(setq make-backup-files nil) ; stop creating backup~ files
;backups suck, use Git
(setq auto-save-default nil)
;splash screen is gross
(setq inhibit-splash-screen t)

;; Use monospaced font faces in current buffer
(defun markdown-fonts ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack" :height 120))
  (buffer-face-mode))
;¯\_(ツ)_/¯

;visual-fill-column
;https://github.com/joostkremers/visual-fill-column/blob/master/visual-fill-column.el
(use-package visual-fill-column
  :defer 1
  :config (progn
    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
    (setq-default visual-fill-column-width 160)
  )
)

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
    (setq header-line-format output)
  )
)

;set the header intially
(simpson-header)

;update the header whenever the buffer-list changes
(add-hook 'buffer-list-update-hook 'simpson-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;change the color of the frame to match the theme
(set-face-foreground 'header-line "#a3adb5")
(set-face-background 'header-line "#4f5b66")

(set-face-attribute 'header-line nil
    :box '(:line-width 3 :color "#4f5b66" :style nil))

(set-face-foreground 'vertical-border "#4f5b66")
(set-face-background 'fringe "#2b303b")
;end color changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "GPG_AGENT" "/usr/local/bin/gpg-agent")
;;read gpg-agent environment
;;https://gist.github.com/jupp0r/08ca64b7c14c6093bba2
(defun read-env-line (line)
  "read a env line and post to environment"
  (let ((key-value-pair (split-string line "=" t)))
    ;car returns first value in list, last return last value
    (setenv (car key-value-pair) (car (last key-value-pair))))
  )
(defvar gpg-agent-info-file)
(setq gpg-agent-info-file (concat (getenv "HOME") "/.gpg-agent-info"))
(when
    ;this is the condition for when
    (file-exists-p gpg-agent-info-file)
  (with-temp-buffer
    ;create a temp buffer and evalute the BODY of the function
    (progn
      ;insert the gpg-agent-info file as a string to the temp buffer
      (insert-file-contents gpg-agent-info-file)
      ;mapc is like map but doesn't accumulate the results
      ;buffer-string is now our temp-buffer which is filled with our gpg file.
      ;split at new lines
      (mapc 'read-env-line (split-string (buffer-string) "\n" t)))
))
(setq epg-gpg-program "/usr/local/bin/gpg")

;y over yes
;http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline15
(fset 'yes-or-no-p 'y-or-n-p)

(defvar simpson-project-name()
  (:eval (when (ignore-errors (projectile-project-root))
    (concat " / " (projectile-project-name))))
)

(setq-default mode-line-format (list
  ;mode-line-modified
  '(:eval (if (buffer-modified-p)
      (propertize " !" 'face '(:foreground "#cf6a4c"))
  ))
  " "
  '(:eval (when simpson-evil (propertize evil-mode-line-tag 'face '(:foreground "#bf616a"))))
  " "
  '(:eval mode-line-position)
  mode-line-modes
  mode-line-misc-info
))

;colors are set for ocean dark
(set-face-attribute 'mode-line nil
    :background "#dfe1e8"
    :foreground "#343d46"
    :box '(:line-width 3 :color "#dfe1e8" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :box '(:line-width 3 :color "#343d46" :style nil))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq confirm-kill-emacs 'yes-or-no-p)

(add-hook 'css-mode-hook '(lambda() (setq show-trailing-whitespace t)))
(set-face-background 'trailing-whitespace "#ab7967")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :diminish ""
  :init (which-key-mode)
)

(use-package fill-column-indicator
  :config (setq fci-rule-column 80)
)

(use-package relative-line-numbers
  :diminish ""
  :bind ("C-SPC l" . relative-line-numbers-mode)
  :config (progn
    ;; (add-hook 'css-mode-hook 'relative-line-numbers-mode)
    ;; (add-hook 'web-mode-hook 'relative-line-numbers-mode)
    ;; (add-hook 'emacs-lisp-mode-hook 'relative-line-numbers-mode)
    (set-face-foreground 'relative-line-numbers-current-line "#d08770")
  )
)

(use-package avy
  :bind (
    ("C-SPC j" . avy-goto-word-1)
    ("C-SPC J" . avy-goto-char)
  )
  :config (progn
    (set-face-background 'avy-lead-face  "#bf616a")
    (set-face-background 'avy-lead-face-0  "#96b5b4")
    (set-face-background 'avy-lead-face-1  "#c0c5ce")
    (set-face-background 'avy-lead-face-2  "#b48ead")
  )
)

(use-package reveal-in-osx-finder)

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("C-SPC e" . yas-expand)
  :load-path "~/.emacs.d/elpa/yasnippet"
  :defer 1
  :init (progn
    (add-hook 'js2-mode-hook #'yas-minor-mode)
    (add-hook 'rjsx-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
  )
  :config (progn
    (yas-reload-all)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
  )
)

(use-package deft
  :bind ("C-SPC d" . deft)
  :config (progn
    (when simpson-evil (add-to-list 'evil-emacs-state-modes 'deft-mode))
    (setq deft-extensions '("txt" "txt.gpg" "org"))
    (setq deft-directory "/Users/asimpson/Dropbox (Personal)/Notational Data")
    (setq deft-use-filename-as-title t)
    (setq deft-auto-save-interval 60.0)
  )
)

(use-package emmet-mode
  :diminish "zen"
  :bind (
    ("C-c e" . emmet-expand-line)
    ("C-c y" . emmet-next-edit-point)
  )
  :defer 1
  :init (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    ;JSX gets className not class
    (add-hook 'js2-mode-hook 'jsxEmmet)
    (add-hook 'handlebars-mode-hook 'jsEmmet)
  )
  :config (progn
    (setq emmet-move-cursor-between-quotes t) ;; default nil
  )
)

(use-package sauron
  :pin melpa-stable
  :defer 2
  :init (setq sauron-modules '(sauron-erc sauron-ams-org))
  :config (progn
    (load-library "~/.dotfiles/emacs/irc-watch.gpg")
    (when simpson-evil (add-to-list 'evil-emacs-state-modes 'sauron-mode))
    (setq sauron-watch-nicks nil)
    (setq sauron-watch-patterns simpson-watch-patterns)
    (setq sauron-hide-mode-line t)
    (setq sauron-separate-frame nil)
    (setq sauron-column-alist '((timestamp . 8)
      (origin . 7)
      (message)))
    (setq sauron-timestamp-format "%H:%M:%S")
    (sauron-start-hidden)
    (advice-add 'shell-command-sentinel :before #'simpson-shell-command-sentiel)
  )
)

(defun jsxEmmet()
  (setq emmet-expand-jsx-className? t)
)

(defun jsEmmet()
  (setq emmet-expand-jsx-className? nil)
)

(setq tramp-default-method "ssh")

;http://emacs.stackexchange.com/a/58
;to open a file with sudo, invoke C-x C-f and then type /sudo::/path

(setq dired-recursive-deletes t)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired nil)

(use-package editorconfig
  :diminish ""
  :defer 1
  :config (editorconfig-mode 1)
)

(use-package prettier-js
  :diminish "pretty"
  :defer 1
  :init (progn
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  )
  :config (setq prettier-js-args '(
      "--trailing-comma" "es5"
      "--bracket-spacing" "true"
      "--single-quote" "true"
    ))
)

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
    (load-library "~/.dotfiles/emacs/irc-accounts.gpg")
    (add-hook 'erc-mode-hook 'visual-line-mode)
    (add-hook 'erc-mode-hook (lambda () (setq mode-name "irc")))
  )
)

(defun simpson-format-slack-name()
  "prepend the @ symbol in erc for slack"
  (interactive)
  (backward-word)
  (insert "@")
  (forward-word)
  (when (looking-at ":")
    (delete-char 1)
    (insert " "))
)

(use-package emoji-cheat-sheet-plus
  :defer 2
  :init (progn
    (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook 'magit-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  )
)

(use-package flyspell
  :diminish "spell"
  :defer 1
  :config (progn
  (add-hook 'erc-mode-hook (lambda () (flyspell-mode 1)))
  (setq flyspell-issue-message-flag nil))
)

(setq auth-sources '("~/.dotfiles/emacs/authinfo.gpg"))

(use-package ivy
  :diminish ""
  :defer 1
  :if (not simpson-helm)
  :config (progn
    (setq ivy-use-virtual-buffers t)
    (ivy-mode)
    (setq ivy-height 20)
    (setq ivy-count-format "")
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "C-SPC /") 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (define-key global-map (kbd "C-=") 'ivy-switch-buffer)
  )
)

(use-package eyebrowse
  :defer 1
  :init (setq eyebrowse-keymap-prefix (kbd "C-SPC s"))
  :config (progn
   ;use list-face-display to see all faces
   (set-face-foreground 'eyebrowse-mode-line-active "#b48ead")
   (eyebrowse-mode t)
   (setq eyebrowse-new-workspace t))
)

(use-package elisp-mode
  :ensure nil
  :init (progn
    (defun simpson-pretty-lambda()
      "make the word lambda the greek character in elisp files"
      (setq prettify-symbols-alist '(("lambda" . 955))))

    (add-hook 'emacs-lisp-mode-hook 'simpson-pretty-lambda)
    (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "λ")))
  )
)

(use-package diminish
  ;minor modes are set with diminish
  ;major modes are changed in the mode hook using the variable mode-name
  :defer 1
  :config (progn
    (diminish 'smerge-mode)
    (diminish 'buffer-face-mode)
    (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  )
)

(add-hook 'shell-mode-hook (lambda () (setq mode-name "shell")))
