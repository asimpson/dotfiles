(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                            ;; restore after startup
                            (setq gc-cons-threshold 800000)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq custom-file "~/.dotfiles/emacs/emacs-custom.el")
(add-to-list 'load-path "~/.dotfiles/emacs/")
(defvar simpson-helm nil
  "Variable to enable or disable helm specific configurations")
(defvar simpson-evil t
  "Variable to enable or disable evil specific configurations")
(setq idle-update-delay 1)
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keybinds.el")
(put 'narrow-to-region 'disabled nil)
