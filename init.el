;;; package --- Puda
;;; Commentary:
;;; Puda's config files
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; load config files
(add-to-list 'load-path "~/.emacs.d/puda")
(require 'puda-packages-to-install)
(require 'puda-ui)
(require 'puda-functions)
(require 'puda-packages)
(require 'puda-misc-keybindings)


(puda-turn-off-mouse)

;;; init.el ends here
