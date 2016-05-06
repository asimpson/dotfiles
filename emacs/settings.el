(fset 'evil-visual-update-x-selection 'ignore)

;turn off toolbar
(tool-bar-mode -1)

;http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
;better visible bell in status line
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line)
))

;no scrollbars, what is this a GUI?!
(scroll-bar-mode -1)

;linum (line numbers mode)
;disable in neo-tree
(global-linum-mode t)
(add-hook 'neotree-mode-hook (lambda () (linum-mode -1)))

;indents! so brutal, each mode can have their own, e.g. css
;spaces
(setq-default indent-tabs-mode nil)
;2 of em
(setq-default tab-width 2)
;yes, css, even you
(setq-default css-indent-offset 2)
;evil, shift like this too ok?
(setq-default evil-shift-width 2)

;fonts
(set-face-attribute 'default nil :font "Hack-10" )
(set-frame-font "Hack-10" nil t)

;modes w/ file extensions
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;each line gets one line
(set-default 'truncate-lines t)

;default auto-complete
(ac-config-default)

;projectile global
(projectile-global-mode)
;projectile no-cache (enable if on a big project)
(setq projectile-enable-caching nil)

;backups suck, use Git
(setq make-backup-files nil) ; stop creating backup~ files
;backups suck, use Git
(setq auto-save-default nil)

;git-gutter on
(global-git-gutter-mode t)
;git-gutter play nice with line numbers
(git-gutter:linum-setup)
(setq git-gutter:ask-p nil)
;try to stay up to date git-gutter, please?
(setq git-gutter:update-interval 1)
(git-gutter:start-update-timer)
(add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
(add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)

;splash screen is gross
(setq inhibit-splash-screen t)

;org mode location
(setq org-agenda-files '("~/Dropbox (Personal)/org"))
;log when done
(setq org-log-done t)
;org mode keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

;capture template http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "~/Dropbox (Personal)/org/tasks.org")
         "* TODO %? %^g
:CREATED: %T")))

;restore windows after org-todo-list closes
(setq org-agenda-restore-windows-after-quit t)

;set up multiterm as login shell
(setq multi-term-program "/bin/zsh")
(setq multi-term-program-switches "--login")

;; Use monospaced font faces in current buffer
(defun markdown-fonts ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack" :height 120))
  (buffer-face-mode))
;add custom fonts for markdown mode
(add-hook 'markdown-mode-hook 'markdown-fonts)
;toggle on visual line mode for writing
(add-hook 'markdown-mode-hook 'visual-line-mode)
;toggle on spell-check for writing
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
;¯\_(ツ)_/¯
(setq flyspell-issue-message-flag nil)

;visual-fill-column
;https://github.com/joostkremers/visual-fill-column/blob/master/visual-fill-column.el
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(setq-default visual-fill-column-width 160)

;prevent line numbers from getting out of hand
(set-face-attribute 'linum nil :height 100)

;http://cha1tanya.com/2014/07/20/emacs-plus-mac-plus-ispell.html
;brew install aspell
(setq ispell-program-name "aspell")
(setq ispell-program-name "/usr/local/bin/aspell")

;;====emmet config====;;
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil

;JSX gets className not class
(defun jsxEmmet()
  (setq emmet-expand-jsx-className? t)
)
;expand class to className in js-mode
(add-hook 'js2-mode-hook 'jsxEmmet)
;;====end emmet config====;;

;treat new buffers as modified files
;http://stackoverflow.com/a/2592558/2344737
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;set width of fill column rule
(setq fci-rule-column 80)

;Every time when the neotree window is opened, let it find current file and jump to node.
(setq neo-smart-open t)

;open txt or md files in Marked 2
(setq markdown-open-command "/usr/local/bin/marked")

;load escreen
(load "escreen")
(escreen-install)

;http://whattheemacsd.com/appearance.el-01.html
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS")
(setq js2-basic-offset 2)
;all that syntax
(setq js2-highlight-level 3)
(setq js2-bounce-indent-p t)
(electric-indent-mode -1)

(defun simpson-header ()
  (setq-default header-line-format
    (list
      ;16 characters = /Users/asimpson/
      (if (stringp (buffer-file-name))
        '(:eval (concat " ▼ ../" (substring (buffer-file-name) 16 nil)))
      "¯\_(ツ)_/¯"
      )
    )
  )
)

;set the header intially
(simpson-header)

;update the header whenever the buffer-list changes
(add-hook 'buffer-list-update-hook 'simpson-header)

;change header line color to match ocean dark
(set-face-attribute 'header-line nil
    :foreground "#a3adb5")

(require 'evil-magit)

;; To load at the start up
(require 'reveal-in-osx-finder)

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

(setq-default mode-line-format (list
  mode-line-modified 
  evil-mode-line-tag
  " "
  mode-line-position
  '(vc-mode vc-mode)
  " "
  mode-line-modes
  mode-line-misc-info
))

;colors are set for ocean dark
(set-face-attribute 'mode-line nil
    :box '(:line-width 4 :color "#4f5b66" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :box '(:line-width 4 :color "#343d46" :style nil))
