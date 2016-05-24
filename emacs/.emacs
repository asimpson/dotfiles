(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(require 'evil)
(require 'org)
(require 'flycheck)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#151718" "#Cd3f45" "#9fca56" "#e6cd69" "#55b5db" "#a074c4" "#55b5db" "#d6d6d6"])
 '(custom-enabled-themes (quote (base16-ocean-dark)))
 '(custom-safe-themes
   (quote
    ("b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "0bd7a42bd443517e5e61dac3cabc24018fbd0c6b2b4199b3c4efd9e3727efd30" "e1551b5516e0a439b6ab019ba00cee866e735f66f22ff67a5d882ad0f1383454" "09669536b4a71f409e7e2fd56609cd7f0dff2850d4cbfb43916cc1843c463b80" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "f2503f0a035c2122984e90eb184185769ee665de5864edc19b339856942d2d2d" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" default)))
 '(org-agenda-files nil)
 '(safe-local-variable-values (quote ((css-indent-offset . 4) (evil-shift-width . 4)))))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; )
(defcustom js2-strict-missing-semi-warning nil
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)
(require 'helm-config)
(helm-mode 1)
(require 'helm-ag)
(require 'projectile)
(require 'helm-projectile)
(helm-projectile-on)
(global-diff-hl-mode)
;(setq exec-path-from-shell-arguments "-l");remove -i
;causes error in shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(require 'multi-term)
(require 'fill-column-indicator)
(require 'command-log-mode)
(require 'relative-line-numbers)
(require 'avy)
(require 'which-key)
(add-to-list 'load-path
              "~/.emacs.d/elpa/yasnippet")
(require 'yasnippet)
;http://emacs.stackexchange.com/questions/7372/stray-trailing-4m-before-prompt-with-zsh-in-m-x-ansi-term
;also good to know to 'send-eof' in helm

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;http://stackoverflow.com/a/6697964/2344737
;(setq split-height-threshold nil) 
;(setq split-width-threshold 0) 

;prevent new frame
(defun switch-to-buffer-other-frame ())

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keybinds.el")

;https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;set local variable per project :point_up:
