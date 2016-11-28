(global-set-key (kbd "C-M-u") 'universal-argument)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-SPC k !") (lambda()
  (interactive)
  (text-scale-set 0)))

(setq window-saved "nothing")

(defun gen-multi-term ()
  (interactive)
  (switch-to-buffer-other-window nil)
  (multi-term)
)

(global-set-key (kbd "C-SPC k e") 'eval-region)
(global-set-key (kbd "C-SPC k t") 'gen-multi-term)
(global-set-key (kbd "C-SPC k r") 'revert-buffer)
(global-set-key (kbd "C-SPC k c") 'clone-indirect-buffer-other-window)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k i") 'erc-select)
(global-set-key (kbd "C-SPC k n") 'projectile-run-async-shell-command-in-root)
(global-set-key (kbd "C-SPC k N") 'kill-shell-buffer)
(global-set-key (kbd "C-SPC !") 'async-shell-command)
(global-set-key (kbd "C-SPC k d") (lambda()
  "shells out to date to return a formatted date string at point"
  (interactive)
  (shell-command "date +%Y-%m-%d-%I:%M" t)))

(global-set-key (kbd "C-SPC /") 'swiper-helm)

(defun kill-shell-buffer()
  (interactive)
  (switch-to-buffer-other-window "*Async Shell Command*")
  (kill-buffer-and-window)
)

(defun simpson-rerun()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (car shell-command-history)))
)

(global-set-key (kbd "C-SPC .") 'simpson-rerun)

(global-set-key (kbd "C-SPC z s") 'save-windows)
(global-set-key (kbd "C-SPC z l") 'restore-windows)

;narrow region
(global-set-key (kbd "C-SPC n") 'narrow-to-region)
;widen
(global-set-key (kbd "C-SPC N") 'widen)

(defun save-windows ()
  "saves windows position"
  (interactive)
  (message "window saved")
  (setq window-saved "saved")
  (window-configuration-to-register 0)
  (delete-other-windows))

(defun restore-windows ()
  "restores windows position"
  (interactive)
  (setq window-saved "nothing")
  (jump-to-register 0))

;; (defun print-path ()
;;   "Print out current buffer path"
;;   (interactive)
;;   (message (buffer-file-name)))

;(global-set-key (kbd "S-SPC p") 'print-path)

;http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html

(setq minor-mode-perm-list (copy-alist minor-mode-alist))

(setq minor-mode-alist (list))

;revert buffer to last-commit
(define-key global-map (kbd "C-SPC R") 'vc-revert)

(global-set-key "\M-h" 'help-command)

;write buffer to fil0
(define-key global-map (kbd "C-SPC w") 'write-file)
(define-key global-map (kbd "s-s") 'write-file)

(define-key global-map (kbd "C-SPC L") 'linum-mode)

(define-key global-map (kbd "s-t") nil)

(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)
