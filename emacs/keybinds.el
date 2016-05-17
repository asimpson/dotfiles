;===evil-leader===;
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
		 ;;rebind help key
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-b") 'projectile-switch-project)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

;re-bind evil-scroll-up ¯\_(ツ)_/¯
(global-set-key (kbd "C-u") 'evil-scroll-up)

;===helm===;
;helm init!
(global-set-key (kbd "M-x") 'helm-M-x)
;helm buffers list
(global-set-key (kbd "C-=") 'helm-buffers-list)
;helm + projectfile find files
(define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
;helm + projectile do ag search
(evil-leader/set-key "f" 'helm-projectile-ag)
(evil-leader/set-key "F" 'helm-do-ag)

;diff-hl navigate hunks
(global-set-key (kbd "C-x p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-x n") 'diff-hl-next-hunk)

;magit status key
(global-set-key (kbd "C-SPC") nil)
(global-set-key (kbd "C-SPC g") 'magit-status)

;toggle emmet
(global-set-key (kbd "C-c e") 'emmet-expand-line)

;fill-column-line
(evil-leader/set-key "c" 'fci-mode)

;===neotree===;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(evil-leader/set-key "\S-t" 'neotree-toggle)

;redefine things to work with evil mode
(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;===keychord===;
;;Exit insert mode by pressing j and then k quickly
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
;evil-key
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;comment line/region
(key-chord-define-global "//" 'comment-region)
;uncomment line/region
(key-chord-define-global "??" 'uncomment-region)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(setq escreen-prefix-char (kbd "C-SPC s"))
(global-set-key escreen-prefix-char 'escreen-prefix)

(defun save-windows ()
  "saves windows position"
  (interactive)
  (message "window saved")
  (window-configuration-to-register 0)
  (delete-other-windows))

(defun restore-windows ()
  "restores windows position"
  (interactive)
  (jump-to-register 0))

(global-set-key (kbd "C-SPC z") 'save-windows)
(global-set-key (kbd "C-SPC Z") 'restore-windows)

;; (defun print-path ()
;;   "Print out current buffer path"
;;   (interactive)
;;   (message (buffer-file-name)))

;(global-set-key (kbd "S-SPC p") 'print-path)
(global-set-key (kbd "C-SPC S") 'escreen-menu)

;http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(defun simpson-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))

(define-key global-map (kbd "C-SPC c") 'simpson-org-task-capture)

;(define-key global-map (kbd "C-SPC t") 'org-agenda)
(define-key global-map (kbd "C-SPC t") 'org-todo-list)

(define-key global-map (kbd "C-SPC a") 'org-agenda)

(define-key global-map (kbd "C-SPC T") 'org-tags-view)

(setq minor-mode-perm-list (copy-alist minor-mode-alist))

(setq minor-mode-alist (list))

(defun simpson-toggle-minors ()
  "toggle minor modes on and off"
  (interactive)
  (if (equal minor-mode-alist minor-mode-perm-list)
      (setq minor-mode-alist (list))
    (setq minor-mode-alist (copy-alist minor-mode-perm-list))))

(define-key global-map (kbd "C-SPC m") 'simpson-toggle-minors)

;revert buffer to last-commit
(define-key global-map (kbd "C-SPC r") 'vc-revert)

(global-set-key "\M-h" 'help-command)

;helm find files
(define-key global-map (kbd "C-SPC f") 'helm-find-files)

;write buffer to fil0
(define-key global-map (kbd "C-SPC w") 'write-file)

