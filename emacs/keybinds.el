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

(defun simpson-pretty-json()
  "ideal for getting pretty JSON from JSON that is copied from a XHR request"
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (json-pretty-print-buffer)
    (kill-new (buffer-string))
  )
)

(defun simpson-project-clone(url)
  "clone a git repo and then change to that project"
  (interactive "sGit url: ")
  (let (
      (projectsDir "~/Projects/")
      (name (nth 0 (split-string (nth 1 (split-string url "/")) "\\.")))
    )
    (shell-command (concat "git clone " url " " projectsDir name))
    (escreen-create-screen)
    (dired (concat projectsDir name))
  )
)

(global-set-key (kbd "C-SPC b") 'simpson-project-clone)
(global-set-key (kbd "C-SPC k P") 'simpson-pretty-json)
(global-set-key (kbd "C-SPC k e") 'eval-region)
(global-set-key (kbd "C-SPC k t") 'gen-multi-term)
(global-set-key (kbd "C-SPC k r") 'revert-buffer)
(global-set-key (kbd "C-SPC k c") 'clone-indirect-buffer-other-window)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k i") 'erc-select)
(global-set-key (kbd "C-SPC k n") 'simpson-smart-shell)
(global-set-key (kbd "C-SPC k N") 'kill-shell-buffer)
(global-set-key (kbd "C-SPC !") 'async-shell-command)
(global-set-key (kbd "C-SPC u") 'universal-argument)
(global-set-key (kbd "C-SPC k g") 'ffap)
(global-set-key (kbd "C-SPC k d") (lambda()
  "shells out to date to return a formatted date string at point"
  (interactive)
  (shell-command "date +%Y-%m-%d-%I:%M" t)))

(global-set-key (kbd "C-SPC k D") (lambda(name)
  "create new file for Deft/nvAlt"
  (interactive "sName of file: ")
  (setq date (shell-command-to-string "date +%m-%d-%y"))
  (setq fixed-date (replace-regexp-in-string "\n$" "" date))
  (write-region "" "" (concat "~/Dropbox (Personal)/Notational Data/" fixed-date "-" name ".txt"))
))

(global-set-key (kbd "C-SPC /") 'swiper-helm)

(defun kill-shell-buffer()
  (interactive)
  (switch-to-buffer-other-window "*Async Shell Command*")
  (kill-buffer-and-window)
  (balance-windows)
)

(defun simpson-smart-shell()
  (interactive)
  (unless (ignore-errors (projectile-run-async-shell-command-in-root))
  (call-interactively 'async-shell-command))
)

(defun simpson-rerun()
  (interactive)
  (if (projectile-project-p)
    (projectile-with-default-dir (projectile-project-root)
      (async-shell-command (car shell-command-history)))
    (async-shell-command (car shell-command-history))
  )
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
(define-key global-map (kbd "s-p") nil)

(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)

(defun simpson-delete-file-for-buffer()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      ;could add the option to delete files that are not tracked by VC
      ;http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
      (if (vc-backend filename)
        (vc-delete-file filename))
    )
  )
)

(define-key global-map (kbd "C-SPC X") 'simpson-delete-file-for-buffer)
