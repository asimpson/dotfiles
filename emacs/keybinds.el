(global-set-key (kbd "C-M-u") 'universal-argument)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-SPC k !") (lambda()
  (interactive)
  (text-scale-set 0)))

(setq window-saved "nothing")

(defun gen-multi-term ()
  "open up a mult-term in a new window"
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
(global-set-key (kbd "C-SPC B") 'simpson-get-git-url)
(global-set-key (kbd "C-SPC k P") 'simpson-pretty-json)
(global-set-key (kbd "C-SPC k e") 'eval-region)
(global-set-key (kbd "C-SPC k t") 'gen-multi-term)
(global-set-key (kbd "C-SPC k r") 'vc-revert-buffer)
(global-set-key (kbd "C-SPC k c") 'clone-indirect-buffer-other-window)
(global-set-key (kbd "C-SPC k v") 'visual-line-mode)
(global-set-key (kbd "C-SPC k i") 'erc-select)
(global-set-key (kbd "C-SPC k n") 'simpson-smart-shell)
(global-set-key (kbd "C-SPC k N") 'kill-shell-buffer)
(global-set-key (kbd "C-SPC !") 'async-shell-command)
(global-set-key (kbd "C-SPC u") 'universal-argument)
(global-set-key (kbd "C-SPC k g") 'ffap)
(global-set-key (kbd "C-SPC k F") (lambda()
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))
)

(global-set-key (kbd "C-SPC m l") 'windmove-right)
(global-set-key (kbd "C-SPC m h") 'windmove-left)
(global-set-key (kbd "C-SPC m j") 'windmove-down)
(global-set-key (kbd "C-SPC m k") 'windmove-up)

(global-set-key (kbd "C-SPC k a") (lambda()
  "copy the file path for the current buffer to the clipboard"
  (interactive)
  (kill-new (buffer-file-name))
))

(global-set-key (kbd "C-SPC D") 'simpson-dired-project)

(defun simpson-dired-project()
  "returns dired mode for the current project"
  (interactive)
  (dired (projectile-project-root))
)

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
(global-set-key (kbd "C-SPC k ?") 'eww)

(global-set-key (kbd "C-SPC /") 'swiper-helm)

(defun kill-shell-buffer()
  "kills the Async Shell Command buffer and then balance's the remaining windows"
  (interactive)
  (switch-to-buffer-other-window "*Async Shell Command*")
  (kill-buffer-and-window)
  (balance-windows)
)

(defun simpson-smart-shell()
  "run shell from projectile root or from current spot in system"
  (interactive)
  (unless (ignore-errors (projectile-run-async-shell-command-in-root))
  (call-interactively 'async-shell-command))
)

(defun simpson-rerun()
  "rerun last shell command. respsects projectile."
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
  (window-configuration-to-register 0)
  (delete-other-windows))

(defun restore-windows ()
  "restores windows position"
  (interactive)
  (jump-to-register 0))

(setq minor-mode-perm-list (copy-alist minor-mode-alist))

(setq minor-mode-alist (list))

;revert buffer to last-commit
(define-key global-map (kbd "C-SPC R") 'vc-revert)

(global-set-key "\M-h" 'help-command)

;write buffer to fil0
(define-key global-map (kbd "C-SPC w") 'write-file)
(define-key global-map (kbd "s-s") 'simpson-save)

(defun simpson-save()
"save buffer"
  (interactive)
  (save-buffer))

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
(define-key global-map (kbd "C-SPC E") 'simpson-erc)
(define-key global-map (kbd "C-SPC k E") 'simpson-kill-erc)

(defun simpson-erc()
  "loads all irc servers defined (as a list) in irc-accounts.gpg"
  (interactive)
  (seq-doseq (x simpson-irc)
    (erc-tls :server x
      :nick (car (auth-source-user-and-password x))
      :password (cadr (auth-source-user-and-password x)))
  )
)

(defun simpson-freenode()
  "connect to freenode irc"
  (interactive)
  (let ((x "irc.freenode.net"))
    (erc :server x
      :nick (car (auth-source-user-and-password x))
      :password (cadr (auth-source-user-and-password x)))
  )
)

(defun simpson-kill-erc()
  "quits all erc servers"
  (interactive)
  (erc-cmd-GQ nil)
)

(defun simpson-macos-mail-link()
  "gets the Message-ID of the current notmuch message and constructs a Mail.app appropriate link
   reference: https://daringfireball.net/2007/12/message_urls_leopard_mail."
  (interactive)
  (let ((id (notmuch-show-get-message-id t)))
    (kill-new (concat "message://%3c" id "%3e"))
  )
)

(defun simpson-open-mail-in-mail()
 "open the current notmuch mail in Mail.app"
 (interactive)
 (let ((id (notmuch-show-get-message-id t)))
   (shell-command (concat "open message://%3c" id "%3e"))
   )
 )

(defun simpson-get-git-url()
  "Grab the remote url for origin and assume it's a github url.
   Open the url in the default browser"
  (interactive)
  (let (url repo)
    (setq url (shell-command-to-string "git remote get-url origin"))
    (setq repo (nth 0 (split-string (nth 1 (split-string url ":")) "\\.")))
    (shell-command (concat "open https://github.com/" repo) t)
  )
)
