;debug use-package 👇
;(setq use-package-verbose t)

;turn off toolbar
(tool-bar-mode -1)

(setq visible-bell nil)
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

(use-package diminish
  :ensure t
  :config (progn
    (diminish 'undo-tree)
    (diminish 'web-mode)
    (diminish 'auto-revert-mode)
    (diminish 'flyspell-mode)
  )
)

(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-ocean-dark t)
)

(use-package exec-path-from-shell
  :ensure t
  :config (progn
    ;(setq exec-path-from-shell-arguments "-l");remove -i
    ;causes error in shell
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))
  )
)

(use-package dired-narrow
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy))
)

(use-package dired-subtree
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle))
)

(use-package vimish-fold
  :ensure t
  :config (vimish-fold-global-mode 1)
  :bind (("C-SPC v" . vimish-fold)
         ("C-SPC V" . vimish-fold-delete))
)

(use-package flycheck
  :ensure t
  :diminish ""
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-SPC '" . flycheck-mode)
  :config (setq-default flycheck-disabled-checkers (list 'javascript-jshint 'emacs-lisp-checkdoc 'emacs-lisp 'json-jsonlist))
)

(use-package evil
  :ensure t
  :diminish "vim"
  :bind ("C-u" . evil-scroll-up)
  :init (progn
    (use-package evil-leader
      :ensure t
      :config (progn
        (global-evil-leader-mode)
        (evil-leader/set-leader ",")
      )
    )
  )
  :config (progn
    (evil-mode 1)
    (add-to-list 'evil-emacs-state-modes 'dired-mode)
    ;http://spacemacs.org/doc/FAQ#orgheadline31
    (fset 'evil-visual-update-x-selection 'ignore)
    (setq-default evil-shift-width 2)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "gx") 'browse-url)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
    (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
    (evil-leader/set-key "f" 'helm-projectile-ag)
    (evil-leader/set-key "F" 'helm-do-ag)
    (evil-leader/set-key "c" 'fci-mode)
    (evil-leader/set-key "v" 'evil-window-vnew)
    (evil-leader/set-key "x" 'evil-window-new)
    (use-package key-chord
      :ensure t
      :config (progn
        (key-chord-mode 1)
        (setq key-chord-two-keys-delay 0.1)
        (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
        (key-chord-define-global "//" 'comment-region)
        (key-chord-define-global "??" 'uncomment-region)
      )
    )
  )
)

(use-package evil-matchit
  :ensure t
  :config (progn
    (global-evil-matchit-mode 1)
    (plist-put evilmi-plugins 'handlebars-mode '((evilmi-simple-get-tag evilmi-simple-jump)
      (evilmi-html-get-tag evilmi-html-jump)))
  )
)

(use-package helm
  :ensure t
  :diminish ""
  :bind (
    ("M-x" . helm-M-x)
    ("C-=" . helm-mini)
    ("C-SPC f" . helm-find-files)
    ("C-SPC k p" . simpson-projects-browser)
  )
  :config (progn
    (helm-mode)
    (require 'helm-config)
    (set-face-background 'helm-ff-dotted-directory "#2b303b")
    (set-face-background 'helm-ff-dotted-symlink-directory "#2b303b")
    (set-face-foreground 'helm-ff-dotted-directory "#65737e")
    (set-face-foreground 'helm-ff-dotted-symlink-directory "#65737e")

    (use-package projectile
      :ensure t
      :diminish ""
      :config (progn
        (projectile-global-mode)
        (setq projectile-switch-project-action 'projectile-dired)
        (setq projectile-enable-caching nil)

        (use-package helm-projectile
          :ensure t
          :config (progn
            (helm-projectile-on)
          )
        )
      )
    )
    (use-package helm-ag
      :ensure t
      :defer t
      :config (progn
        (custom-set-variables
        '(helm-ag-base-command "ag --nocolor --nogroup"))
      )
    )
  )
)

(use-package helm-flyspell
  :ensure t
  :diminish ""
  :bind ("C-SPC C" . helm-flyspell-correct)
)

(defun simpson-projects-browser()
  (interactive)
  (cd "~/Projects/")
  (helm-find-files nil)
)

(use-package magit
  :ensure t
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

    (use-package evil-magit
      :ensure t
    )
  )
)

(use-package diff-hl
  :ensure t
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
  :ensure t
  :diminish ""
  :config (progn
    (ac-config-default)
    ;this prevents the stupid behavior in scss where &:before {___ autocompletes
    ;lang!
    (defconst ac-css-pseudo-classes nil)
  )
)

(use-package org
  :ensure t
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
    ;org mode location
    (setq org-agenda-files '("~/Dropbox (Personal)/org"))
    ;log when done
    (setq org-log-done t)
    ;set deadline warning
    (setq org-deadline-warning-days 3)
    ;org mode keywords
    (setq org-todo-keywords
          '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

    ;capture template http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
    (setq org-capture-templates
          '(("a" "My TODO task format." entry
            (file "~/Dropbox (Personal)/org/tasks.org")
            "* TODO %? %^g
    :PROPERTIES:
    :CREATED: %T
    :END:")))
    ;restore windows after org-todo-list closes
    (setq org-agenda-restore-windows-after-quit t)
    (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
    ;org theme changes for ocean 16
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
  )
)

(use-package escreen
  :ensure t
  :demand t
  :bind ("C-SPC S" . escreen-menu)
  :config (progn
    (setq escreen-prefix-char (kbd "C-SPC s"))
    (global-set-key escreen-prefix-char 'escreen-prefix)
  )
)

(use-package multi-term
  :ensure t
  :config (progn
    (setq multi-term-program "/bin/zsh")
    (setq multi-term-program-switches "--login")
    (define-key global-map (kbd "C-SPC p") 'term-paste)
  )
)

(use-package markdown-mode
  :ensure t
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
    (setq markdown-open-command "/usr/local/bin/marked")
  )
)

(use-package js2-mode
  :ensure t
  :interpreter (
    ("node" . js2-mode)
  )
  :mode (
    ("\\.js?\\'" . js2-mode)
    ("\\.jsx?\\'" . js2-mode)
  )
  :config (progn
    (add-hook 'js2-mode-hook 'relative-line-numbers-mode)
    (setq js2-basic-offset 2)
    (setq js2-highlight-level 3)
    (setq js2-bounce-indent-p t)
    (electric-indent-mode -1)
    (setq js2-mode-show-strict-warnings nil)
    (add-hook 'js2-mode-hook '(lambda() (setq show-trailing-whitespace t)))
    (global-set-key (kbd "C-SPC k j") 'js2-mode-hide-warnings-and-errors)
    (defcustom js2-strict-missing-semi-warning nil
      "Non-nil to warn about semicolon auto-insertion after statement.
    Technically this is legal per Ecma-262, but some style guides disallow
    depending on it."
      :type 'boolean
      :group 'js2-mode)
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
(setq flyspell-issue-message-flag nil)

;visual-fill-column
;https://github.com/joostkremers/visual-fill-column/blob/master/visual-fill-column.el
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(setq-default visual-fill-column-width 160)

;https://joelkuiper.eu/spellcheck_emacs
;brew install hunspell


(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)


;treat new buffers as modified files
;http://stackoverflow.com/a/2592558/2344737
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))


;http://whattheemacsd.com/appearance.el-01.html
;; (defmacro rename-modeline (package-name mode new-name)
;;   `(eval-after-load ,package-name
;;      '(defadvice ,mode (after rename-modeline activate)
;;         (setq mode-name ,new-name))))

(defun simpson-header ()
  (setq header-line-format
    (list
      ;16 characters = /Users/asimpson/
      (if (stringp (buffer-file-name))
        (eval (concat " ▼ ../" (substring (buffer-file-name) 16 nil)))
      "¯\\_(ツ)_/¯"
      )
  ))
)

;set the header intially
(simpson-header)

;update the header whenever the buffer-list changes
(add-hook 'buffer-list-update-hook 'simpson-header)

;change header line color to match ocean dark
(set-face-foreground 'header-line "#a3adb5")
(set-face-background 'header-line "#4f5b66")

(set-face-attribute 'header-line nil
    :box '(:line-width 3 :color "#4f5b66" :style nil))

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
  '(:eval (propertize evil-mode-line-tag 'face '(:foreground "#bf616a")))
  " "
  mode-line-position
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

(set-face-foreground 'vertical-border "#4f5b66")
(set-face-background 'fringe "#2b303b")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq confirm-kill-emacs 'yes-or-no-p)

(add-hook 'css-mode-hook '(lambda() (setq show-trailing-whitespace t)))
(set-face-background 'trailing-whitespace "#ab7967")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package which-key
  :ensure t
  :diminish ""
  :config (which-key-mode)
)

(use-package fill-column-indicator
  :ensure t
  :config (setq fci-rule-column 80)
)

(use-package relative-line-numbers
  :ensure t
  :diminish ""
  :bind ("C-SPC l" . relative-line-numbers-mode)
  :config (progn
    (add-hook 'css-mode-hook 'relative-line-numbers-mode)
    (add-hook 'web-mode-hook 'relative-line-numbers-mode)
    (add-hook 'emacs-lisp-mode-hook 'relative-line-numbers-mode)
    (set-face-foreground 'relative-line-numbers-current-line "#d08770")
  )
)

(use-package avy
  :ensure t
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

(use-package reveal-in-osx-finder
  :ensure t
)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind ("C-SPC e" . yas-expand)
  :load-path "~/.emacs.d/elpa/yasnippet"
  :init (progn
    (add-hook 'js2-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
  )
  :config (progn
    (yas-reload-all)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
  )
)

(use-package deft
  :ensure t
  :bind ("C-SPC d" . deft)
  :config (progn
    (add-to-list 'evil-emacs-state-modes 'deft-mode)
    (setq deft-extensions '("txt" "tex" "org"))
    (setq deft-directory "/Users/asimpson/Dropbox (Personal)/Notational Data")
    (setq deft-use-filename-as-title t)
    (setq deft-auto-save-interval 60.0)
  )
)

(use-package emmet-mode
  :ensure t
  :diminish "zen"
  :bind (
    ("C-c e" . emmet-expand-line)
    ("C-c y" . emmet-next-edit-point)
  )
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

(use-package alert
  :ensure t
  :config (progn
    (setq alert-default-style 'notifier)
    (defun alert-notifier-notify (info)
      (if alert-notifier-command
          (let ((args
                (list "-title"   (alert-encode-string (plist-get info :title))
                      "-message" (alert-encode-string (plist-get info :message))
                )))
            (message "%s" args)
            (apply #'call-process alert-notifier-command nil nil nil args))
        (alert-message-notify info)))
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
;to open a file with sudo, invoke Tramp C-x C-f and then type /sudo::/path

(setq dired-recursive-deletes t)
(setq delete-by-moving-to-trash t)
(setq dired-use-ls-dired nil)

(use-package editorconfig
  :ensure t
  :init (editorconfig-mode 1)
)
