;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; No splash screen
(setq inhibit-startup-message t)
;; no scrollbars
(scroll-bar-mode -1)
;; the blinking cursor disabled
(blink-cursor-mode -1)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; delete the selection with a keypress
(delete-selection-mode t)
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files
;; start emacs in full screen mode
(toggle-frame-fullscreen)
;;disable bells and visual feedback too
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
;; (global-subword-mode 1) ;; move through camelCase
(global-auto-revert-mode 1) ;; reload open files
(setq column-number-mode t)
(setq x-underline-at-descent-line t)
(recentf-mode 1)
;; (electric-pair-mode 1)
(smartparens-global-mode t)
(show-paren-mode t) ;; show matching parenthesis

;; set fonts
(setq default-frame-alist '((font . "Source Code Pro for Powerline-14:weight=normal:width=medium:powerline-scale=1.1")))
(set-frame-font "Source Code Pro for Powerline-14:weight=normal:width=medium:powerline-scale=1.1")

;comment bottom lines if on mac
;; (setq x-select-enable-clipboard t) ; copy to system clipboard
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value) ; paste from system clipboard

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4) ; set tab width to 4 for all buffers

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

;; Set 4 spaces
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces

;; more useful frame title, that show either a file
(setq frame-title-format
      ;;"" invocation-name " before the RHPH if needed to put "emacs"
      '("RHPH - " (:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name))
                           "%b"))))
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; uncomment above if emacs is being slow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; start certain buffers in only one full frame
(fullframe magit-status magit-mode-quit-window)
(fullframe magit-revision magit-mode-quit-window)

;; transparency
(push '(alpha . (0.94 . 0.94)) default-frame-alist)
(set-frame-parameter (selected-frame) 'alpha '(0.94 . 0.94))

(setq default-directory "/var/www/community/" )
(setq initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Config!!")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9864c2e956c25b3098fbc935ba0969e333dd74ecd7a1013c8dd39a6c171e1cca" "d9a0d14596e3d0bdb81f052fa9b99741dcd239af402d42e35f80822e05557cb2" "8f0334c430540bf45dbcbc06184a2e8cb01145f0ae1027ce6b1c40876144c0c9" "cadc97db0173a0d0bfc40473cab4da462af0ba8d60befd0a4879b582bcbc092d" "0788bfa0a0d0471984de6d367bb2358c49b25e393344d2a531e779b6cec260c5" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "b5fe3893c8808466711c1b55bb7e66b9c6aa2a86811783375a43e1beabb1af33" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "8e7ca85479dab486e15e0119f2948ba7ffcaa0ef161b3facb8103fb06f93b428" "532769a638787d1196bc22c885e9b85269c3fc650fdecfc45135bb618127034c" "977513781c8dd86f4f0a04dbf518df5ba496da42b71173368b305478703eea42" "86a731bda96ed5ed69980b4cbafe45614ec3c288da3b773e4585101e7ece40d2" "70340909b0f7e75b91e66a02aa3ad61f3106071a1a4e717d5cdabd8087b47ec4" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "aab598c4d024d544b4e8b356a95ca693afa9de000b154bd2f86eed68c9e75557" default))
   ))

(provide 'puda-ui)
;;; puda-ui ends here
