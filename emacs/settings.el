;; -*- lexical-binding: t; -*-
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
  "variable that points to the proper Dropbox path")

(cond
 ((file-exists-p "~/Dropbox (Personal)/") (setq simpson-dropbox-path "~/Dropbox (Personal)/"))
 ((file-exists-p "~/Dropbox/") (setq simpson-dropbox-path "~/Dropbox/")))

(defmacro simpson-make-neutral (map)
  "Create evil-style window movement for a given map."
  `(progn
     (define-key ,map (kbd "C-j") 'evil-window-down)
     (define-key ,map (kbd "C-k") 'evil-window-up)
     (define-key ,map (kbd "C-h") 'evil-window-left)
     (define-key ,map (kbd "C-l") 'evil-window-right)))

(defmacro simpson-make-neutral--keys (map)
  "Create evil-style  movement for a given map."
  `(progn
     (define-key ,map "j" 'next-line)
     (define-key ,map "k" 'previous-line)))

(defmacro simpson-load-file (file)
  "Check if file exists and if it does load it."
  `(when (file-exists-p ,file)
     (load-library ,file)))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :config (progn
            (osx-trash-setup)
            (setq delete-by-moving-to-trash t)))

(use-package base16-theme
  :if (display-graphic-p)
  :disabled
  :init (load-theme 'base16-ocean t))

(use-package tomorrow-theme
  :ensure nil
  :if (and (display-graphic-p) (file-exists-p "~/Projects/tomorrow-theme/GNU Emacs"))
  :load-path "~/Projects/tomorrow-theme/GNU Emacs"
  :init (require 'tomorrow-day-theme)
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
            (add-to-list 'evil-emacs-state-modes 'image-mode)
            (add-to-list 'evil-emacs-state-modes 'comint-mode)
            (add-to-list 'evil-emacs-state-modes 'eww-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-server-mode)
            (add-to-list 'evil-emacs-state-modes 'circe-channel-mode)
            (add-to-list 'evil-emacs-state-modes 'sql-interactive-mode)
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
            (if simpson-evil
                (progn (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                       (key-chord-define evil-normal-state-map "//" 'comment-region)
                       (key-chord-define evil-normal-state-map "??" 'uncomment-region)
                       (key-chord-define evil-normal-state-map "cc" 'simpson-magit-comment))
              (key-chord-define-global "jk" 'god-local-mode))))

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
            (setq auto-revert-buffer-list-filter nil);;'magit-auto-revert-repository-buffers-p)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (put 'magit-clean 'disabled nil)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)
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
            (cond
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
  :bind (("C-SPC c" . simpson-org-task-capture)
         ("C-SPC k B" . simpson-org-blog-capture)
         ("C-SPC t" . org-todo-list)
         ("C-SPC a" . org-agenda)
         ("C-SPC T" . org-tags-view))
  :mode (("\\.txt\\'" . org-mode))
  :config (progn
            (require 'ox-md)
            ;;look into swapping with txt, org-agenda-file-regexp
            (setq org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
            (setq org-agenda-files `(,(concat simpson-dropbox-path "org")))
            (setq org-log-done t)
            (setq org-deadline-warning-days 3)
            (setq org-export-with-toc nil)
            (setq org-refile-targets '(
                                       ("personal.txt" . (:level . 1))
                                       ("tasks.txt" . (:level . 1))
                                       ("reading.txt" . (:level . 1))))
            (setq org-todo-keywords
                  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
            (setq org-capture-templates
                  `(("a" "My TODO task format." entry
                     (file ,(concat simpson-dropbox-path "org/tasks.txt"))
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
    :END:")))
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
             '((sh . t)
               (js . t)))
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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

(set-face-attribute 'default nil :font "Hack-10" )
(set-frame-font "Hack-10" nil t)

;;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(use-package yaml-mode
  :mode (("\\.yml?\\'" . yaml-mode)))

;;each line gets one line
(set-default 'truncate-lines t)

(setq make-backup-files nil)

(setq auto-save-default nil)

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

;;treat new buffers as modified files
;;http://stackoverflow.com/a/2592558/2344737
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(setq header-line-format nil)
(set-face-attribute 'header-line nil :background "white" :box nil)

(when (string= (car custom-enabled-themes) "base16-ocean")
  (set-face-foreground 'vertical-border (plist-get base16-ocean-colors :base02))
  (set-face-background 'fringe (plist-get base16-ocean-colors :base00)))

(setq epg-gpg-program "/usr/local/bin/gpg")

;;http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline15
(fset 'yes-or-no-p 'y-or-n-p)

(use-package shrink-path
  :defer 1)

(setq-default mode-line-format nil)

(eval-after-load 'shrink-path (lambda()
                                (setq-default mode-line-format (list
                                                                '(:eval (when (buffer-modified-p)
                                                                          (propertize " !" 'face '(:foreground "Indianred"))))
                                                                " "
                                                                '(:eval (when (buffer-file-name)
                                                                          (propertize (shrink-path-file (buffer-file-name) t) 'help-echo (buffer-file-name))))
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
  ;; stolen from  https://github.com/iqbalansari/mu4e-alert/blob/master/mu4e-alert.el#L288
  (let (num (output (shell-command-to-string simpson-mail-update-command)))
    (if (> (length output) 0)
        (progn
          (setq num (number-to-string (length (split-string output "\n" t))))
          (setq simpson-mail-count (concat (propertize "mail icon"
                                                       'display display-time-mail-icon
                                                       'face display-time-mail-face
                                                       'help-echo (concat num " " "unread mail")
                                                       'keymap '(mode-line keymap
                                                                           (mouse-1 . mu4e)))
                                           " " num)))
      (setq simpson-mail-count nil))))

(defun simpson-update-check()
  "Process that checks for software updates on macOS."
  (set-process-sentinel (start-process "softwareupdate" "*softwareupdate*" "softwareupdate" "-l") 'simpson-update-check--sentinel))

(defun simpson-update-check--sentinel(proc msg)
  "Search for word recommended in softwareupdate output to determine if there are software updates."
  (when (and (string= msg "finished\n") (buffer-live-p (get-buffer "*softwareupdate*")))
    (if (with-current-buffer "*softwareupdate*"
          (goto-char (point-min))
          (search-forward "recommended" nil t))
        (setq simpson-software-update "‼")
      (setq simpson-software-update nil)
      (kill-buffer "*softwareupdate*"))))

(run-at-time 0 (* 60 60) #'simpson-update-check)

(set-face-attribute 'mode-line nil :height 1.0 :box nil)

(if (string= (car custom-enabled-themes) "base16-ocean")
    (progn
      (set-face-attribute 'mode-line nil
                          :background (plist-get base16-ocean-colors :base06)
                          :foreground (plist-get base16-ocean-colors :base01)
                          :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base06) :style nil))
      (set-face-attribute 'mode-line-inactive nil
                          :box `(:line-width 3 :color ,(plist-get base16-ocean-colors :base01) :style nil)))
  (set-face-attribute 'mode-line nil
                      :background "khaki1" :box '(:line-width 5 :color "khaki1"))
  (set-face-attribute 'mode-line-inactive nil
                      :background "LightCyan2" :box '(:line-width 5 :color "LightCyan2")))

(set-face-background 'fringe nil)

(defun simpson-evil-mode ()
  "Change mode line color based on evil state."
  (cond
   ((evil-insert-state-p) (set-face-attribute 'mode-line nil :background "IndianRed" :foreground "white" :box '(:line-width 5 :color "IndianRed")))
   ((evil-normal-state-p) (set-face-attribute 'mode-line nil :background "khaki1" :foreground "black" :box '(:line-width 5 :color "khaki1")))))

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

(use-package sauron
  :pin melpa-stable
  :defer 2
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
            (advice-add 'shell-command-sentinel :before #'simpson-shell-command-sentiel)))

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
            (setq dired-recursive-deletes t)
            (setq delete-by-moving-to-trash t)
            (setq dired-use-ls-dired nil)
            (define-key dired-mode-map "e" 'epa-dired-do-encrypt)
            (simpson-make-neutral dired-mode-map)
            (simpson-make-neutral--keys dired-mode-map)
            (define-key dired-mode-map "E" 'epa-dired-do-decrypt)))

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
            (simpson-make-neutral erc-mode-map)
            (setq erc-modules '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp))
            (simpson-load-file "~/.dotfiles/emacs/irc-accounts.gpg")
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
            (define-key dired-mode-map "r" 'counsel-rg)
            (simpson-make-neutral ivy-occur-mode-map)
            (setq ivy-use-selectable-prompt t)
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
            (define-key dired-mode-map "f" 'counsel-find-file)
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
            (set-face-foreground 'eyebrowse-mode-line-active "green4")
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
  :if (file-exists-p "~/Projects/ivy-feedwrangler/")
  :load-path "~/Projects/ivy-feedwrangler/")

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
  ("r" simpson-hack-local "reload local vars")
  ("c" simpson-mocha-scope "custom project scope")
  ("f" mocha-test-file "whole file"))

(defun simpson-hack-local()
  (interactive)
  (hack-dir-local-variables-non-file-buffer))

(defun simpson-mocha-scope()
  (interactive)
  (let ((dir (read-directory-name "Which dir?: ")) (string (read-string "What glob? :")))
    (setq mocha-project-test-directory (concat dir string))))

(defun simpson-rg-switches(dir switches)
  (interactive
   (let (dir switches)
     (list
      (when (y-or-n-p "Pick dir?")
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
  :mode("\\.lisp?\\'" . aggressive-indent-mode))

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
  :mode("\\.lisp?\\'" . slime-mode)
  :diminish ""
  :config(progn
           (add-hook 'slime-mode-hook (lambda () (setq mode-name "goo")))
           (setq inferior-lisp-program "/usr/local/bin/ccl")))

(defun simpson-org-to-todo()
  "Convert a line (or region) in an org file to a TODO"
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
  "Makes eww more pleasant to use. Run it after eww buffer is loaded.
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
  :config(progn
           (add-hook 'rust-mode-hook 'cargo-minor-mode)))

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

(with-eval-after-load "keybinds.el"
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/shell-history")
    (setq shell-command-history (split-string (buffer-string) "\n"))))

(defun simpson-save-history()
  "Write contents of shell-command-history to ~/.emacs.d/shell-history"
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
  :load-path "/usr/local/Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu/mu4e"
  :config (progn
            (simpson-load-file "~/.dotfiles/emacs/mu4e.el.gpg")
            (set-face-attribute 'mu4e-highlight-face nil :background "LightGreen" :foreground nil)
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
            (add-to-list 'mu4e-view-actions '("eww view" . jcs-view-in-eww) t)
            (add-to-list 'mu4e-view-actions '("Shr view" . simpson-shr-view) t)
            (define-key mu4e-headers-mode-map (kbd "C-c C-u") 'mu4e-update-index)
            (define-key mu4e-main-mode-map "q" 'simpson-mu4e-quit)
            (simpson-make-neutral mu4e-headers-mode-map)
            (simpson-make-neutral--keys mu4e-headers-mode-map)
            (simpson-make-neutral--keys mu4e-view-mode-map)))

(defun simpson-mu4e-quit()
  "Quit mu4e and then update mail icon"
  (interactive)
  (mu4e-quit)
  (simpson-check-mail))

(use-package org-mu4e
  :ensure nil
  :defer 1
  :load-path "/usr/local/Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu/mu4e")

(defun jcs-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg)))
  (text-scale-set 1)
  (eww-reload t))

(defun simpson-shr-view(msg)
  "Parse m4ue message as html and render it in a new frame using shr."
  (let ((buf "*shr-email*"))
    (when (get-buffer buf) (kill-buffer buf))
    (switch-to-buffer-other-frame buf)
    (with-current-buffer (get-buffer-create buf)
      (insert (mu4e-message-field msg :body-html))
      (shr-render-buffer buf)
      (delete-other-windows))))

(defun simpson-browse-url()
  (interactive)
  (call-process "open" nil nil nil (get-text-property (point) 'shr-url)))

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))

(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))

(eval-after-load 'eww (lambda()
                        (simpson-make-neutral eww-mode-map)
                        (simpson-make-neutral--keys eww-mode-map)
                        (add-hook 'eww-mode-hook #'xah-rename-eww-hook)))

(defhydra hydra-smerge()
  "
    Shortcut to smerge next and previous:
    _n_ smerge-next
    _p_ smerge-prev
  "
  ("n" smerge-next "next conflict")
  ("p" smerge-prev "previous conflict"))

(defun fetch-freenode-password(&rest params)
  "Returns the freenode token from auth-source."
  (let ((entry (auth-source-search :host "irc.freenode.net" :max 1)))
    (funcall (plist-get (car entry) :secret))))

(use-package circe
  :config(progn
           (require 'circe-chanop)
           (simpson-make-neutral circe-mode-map)
           (add-hook 'circe-mode-hook (lambda () (setq mode-name "irc")))
           (setq circe-network-options
                 '(("Freenode"
                    :tls t
                    :nick "asimpson"
                    :sasl-username "asimpson"
                    :sasl-password fetch-freenode-password)))))

(use-package sql
  :ensure nil
  :config (progn
            (simpson-make-neutral sql-interactive-mode-map)))

(use-package restclient)

;;; settings.el ends here
