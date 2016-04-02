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
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
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

;scss settings
;scss not css
(autoload 'scss-mode "scss-mode")
;dont compile on save
(setq scss-compile-at-save nil)

;powerline default themej
(powerline-default-theme)
;https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)
;slant variation of powerline
(setq powerline-default-separator 'slant)
;spacemacs color
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;backups suck, use Git
(setq make-backup-files nil) ; stop creating backup~ files
;backups suck, use Git
(setq auto-save-default nil)

;git-gutter on
(global-git-gutter-mode t)
;git-gutter play nice with line numbers
(git-gutter:linum-setup)
;try to stay up to date git-gutter, please?
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
         "* TODO %?
:CREATED: %T")))

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

(setq-default header-line-format
  (list
    '(:eval (concat " ▼ " (buffer-file-name)))
  )
)

(require 'evil-magit)
