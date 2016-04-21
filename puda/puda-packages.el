;;; package --- Summary
;;; Commentary:
;;; All the packages in one place loaded using use-package
;;; Code:

(use-package monokai-theme
  :ensure t
  :config
  (progn
    ;; load the theme for emacsclient instances (when running emacs as server)
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (select-frame frame)
                    (toggle-frame-fullscreen)
                    (if (window-system frame)
                        (load-theme 'monokai t)
                      (load-theme 'monokai t))
                    ;; (load-theme 'airline-kalisi t)
                    )))

    ;; terminal
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))

(use-package airline-themes
  :ensure t
  :init
  (progn
    (setq airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xe0a0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1)
    (setq powerline-default-separator 'arrow
          powerline-display-hud nil
          ))
  :config
  (progn
    (load-theme 'airline-molokai t)))

(use-package golden-ratio
  :ensure t
  :init
  (progn
    (setq
     golden-ratio-exclude-modes '("ediff-mode"
                                       "eshell-mode"
                                       "helm-mini"
                                       "guide-key-mode"
                                       "undo-tree-visualizer-mode"
                                       "diff-mode"
                                       "dired-mode"
                                       )
     split-width-threshold nil))
  :config
  (progn
    (append-to-list 'golden-ratio-extra-commands '(ace-window
                                                   ace-swap-window
                                                   buf-move-left
                                                   buf-move-right
                                                   buf-move-down
                                                   buf-move-up
                                                   evil-window-down
                                                   evil-window-left
                                                   evil-window-right
                                                   evil-window-up))
    (golden-ratio-mode t)))

(use-package linum-relative
  :ensure t
  :config
  (progn
    (linum-relative-global-mode 1)))

(use-package auto-yasnippet
  :ensure t
  )

(use-package pretty-mode
  :ensure t
  :config
  (progn
    (global-pretty-mode t)
    (global-prettify-symbols-mode t) ;; is there extra package for this?
    ))

(use-package helm
  :ensure t
  :config
  (progn
    (defhydra helm-like-unite ()
      "movement"
      ("?" helm-help "help")
      ("<escape>" nil "exit")
      ;; ("<escape>" keyboard-escape-quit "exit")
      ("<SPC>" helm-toggle-visible-mark "mark")
      ("a" helm-toggle-all-marks "(un)mark all")
      ("/" (lambda ()
             (interactive)
             (execute-kbd-macro [?\C-s]))
       "search")
      ("v" helm-execute-persistent-action)
      ("g" helm-beginning-of-buffer "top")
      ("G" helm-end-of-buffer "bottom")
      ("j" helm-next-line "down")
      ("k" helm-previous-line "up"))

    (helm-mode 1) ;; for helm to do everything instead of ido
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x o") 'other-window)
    ;; for helm completion
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)
    (global-set-key (kbd "C-c h i") 'helm-imenu) ; helm search for string in project
    ;; helm fuzzy matchings
    (setq helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t)))

(use-package buffer-move
  :ensure t
  :config
  (progn
    (defhydra puda-hydra-buffer-move ()
      "move buffers"
      ("<escape>" nil "exit")
      ("h" buf-move-left "move left")
      ("j" buf-move-down "move down")
      ("k" buf-move-up "move up")
      ("l" buf-move-right "move right")
      ("q" nil "exit"))
    (defhydra puda-hydra-buffer-actions ()
      "buffer actions"
      ("<escape>" nil "exit")
      ("l" evil-next-buffer "next-buffer")
      ("h" evil-prev-buffer "previous-buffer")
      ("d" evil-delete-buffer "delete-buffer")
      ("q" nil "exit")
      )
    (defhydra puda-hydra-scroll-page ()
      "page actions"
      ("<escape>" nil "exit")
      ("j" evil-scroll-down "scroll down")
      ("k" evil-scroll-up "scroll up")
      ("z" evil-scroll-line-to-center "center")
      ("b" evil-scroll-line-to-bottom "bottom")
      ("t" evil-scroll-line-to-top "top")
      ("q" nil "exit")
      )
    ))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-vertical-mode 1) ;; if ido is used, use vertical mode
    (setq ido-vertical-define-keys 'C-n-and-C-p-only) ;; use C-n and C-p
    ))

(use-package anzu
  :ensure t
  :config
  (progn
    (global-anzu-mode t)))

(use-package evil-anzu
  :ensure t
  )

(use-package auto-package-update
  :ensure t
  :config
  (progn
    (auto-package-update-maybe)))

(use-package org-bullets
  :ensure t
  :config
  (progn
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org
  :ensure t
  :config
  (progn
    (setq org-log-done 'time)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome")
    (setq org-src-fontify-natively t)))

(use-package winner
  :ensure t
  :config
  (progn
    (winner-mode t)
    (windmove-default-keybindings 'meta)))

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode t)))

(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-enable-caching t) ;;projectile caching;
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c h f") 'helm-projectile-grep)))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt)) ;;for promt that uses C-n and C-p
    (yas-global-mode t)))

(use-package undo-tree
  :ensure t
  :init
  (progn
    (setq undo-tree-visualizer-diff t))
  :config
  (progn
    (global-undo-tree-mode)))

(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-,") 'er/expand-region)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-=") 'mc/mark-all-like-this)))

(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'web-mode-hook  'emmet-mode)))

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    ;; ;; rainbow mode
    (add-hook 'sgml-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook  'rainbow-mode)
    (add-hook 'web-mode-hook  'rainbow-mode)
    (add-hook 'scss-mode-hook  'rainbow-mode)
    ))

(use-package js2-mode
  :ensure t
  :mode (
         ("\\.js\\'" . js2-mode))
  :config
  (progn
    ;; (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    ;; rainbow delimiters
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package web-mode
  :ensure t
  :mode (;;when to open in web-mode
         ("\\.phtml\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :init
  (progn
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-indent-style 4)
    (setq web-mode-enable-current-element-highlight t))
  :config
  (progn
    (setq web-mode-engines-alist
          '(("php"    . "\\.phtml\\'")
            ("blade"  . "\\.blade\\."))
          )
    (setq web-mode-disable-auto-pairing t)
    (setq web-mode-script-padding 4)))

(use-package sass-mode
  :ensure t
  :mode (
         ("\\.scss\\'" . sass-mode)))

(use-package php-mode
  :ensure t
  :mode (("\\.module\\'" . php-mode)
         ("\\.views\\.inc\\'" . php-mode)))

(use-package shell-pop
  :ensure t
  :init
  (progn
    (setq shell-pop-full-span t)
    (setq shell-pop-shell-type
          (quote
           ("ansi-term" "*ansi-term*"
            (lambda nil
              (ansi-term shell-pop-term-shell)))))
    (setq shell-pop-term-shell "/bin/zsh"
          shell-pop-window-position "right"
          shell-pop-window-size 50)
    )
  :config
  (progn
    ;; for ansi term tab completion
    (add-hook 'term-mode-hook (lambda()
                                (yas-minor-mode -1)))
    (define-key term-raw-map (kbd "C-y") 'term-paste)))

(use-package ggtags
  :ensure t
  :config
  ;; make sure gnu globals is installed
  (progn
    (defun puda-drupal-gtags-create ()
      (interactive)
      (let ((inhibit-message t) (default-directory "/var/www/community/"))
        (shell-command "gtags --gtagslabel drupal")))

      (evil-leader/set-key-for-mode 'php-mode "." 'ggtags-find-definition)
      (evil-leader/set-key-for-mode 'php-mode ">" 'ggtags-prev-mark)
      (evil-leader/set-key-for-mode 'php-mode "rt" 'puda-drupal-gtags-create) ;;recreate tags
      (company-mode t)
      (add-to-list 'company-backends '(company-dabbrev-code company-gtags company-dabbrev))))

(use-package php-auto-yasnippets
  :ensure t
  :config
  (progn
    (setq php-auto-yasnippet-php-program "/home/puda/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php")
    (define-key php-mode-map (kbd "C-'") 'yas/create-php-snippet)
    ))

(use-package helm-swoop
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c h s") 'helm-swoop) ; helm swoop
    ))

(use-package magit
  :ensure t
  :init
  (progn
    (setq magit-completing-read-function 'magit-builtin-completing-read)
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
    (setq magit-diff-hide-trailing-cr-characters t)
    )
  :config
  (progn
    ;; Magit
    (global-set-key (kbd "C-c g s") 'magit-status)
    (global-set-key (kbd "C-c g l") 'magit-log)
    (global-set-key (kbd "C-c g a") 'magit-stage)
    (global-set-key (kbd "C-c g c") 'magit-commit)
    (global-set-key (kbd "C-c g p") 'magit-push)
    (global-set-key (kbd "C-c g f") 'magit-pull) ;; f for fork
    (global-set-key (kbd "C-c g b") 'magit-branch)
    (global-set-key (kbd "C-c g r t") 'magit-revert)
    (global-set-key (kbd "C-c g r h") 'magit-reset-hard)
    ))

(use-package guide-key
  :ensure t
  :config
  (progn
    (setq guide-key/guide-key-sequence t) ;; Enable guide-key for all keys
    (setq guide-key/idle-delay 0.4)
    ;; guide key to show hints
    (guide-key-mode 1) ; Enable guide-key-mode
    (setq guide-key/idle-delay 0.2)
    (setq guide-key/popup-window-position 'bottom)
    ))

(use-package volatile-highlights
  :ensure t
  :config
  (progn
    (volatile-highlights-mode t)))

;; company mode (complete any)
(use-package company
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-dabbrev-code-everywhere t
          company-dabbrev-everywhere t
          company-dabbrev-code t
          company-dabbrev t
          company-dabbrev-minimum-length 1
          company-dabbrev-downcase nil
          )
    (defvar-local company-fci-mode-on-p nil)
    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
    ;; hippie expand configure
    (setq hippie-expand-try-functions-list
          '(
            ;; Try to expand word "dynamically", searching the current buffer.
            try-expand-dabbrev
            ;; Try to expand word "dynamically", searching all other buffers.
            try-expand-dabbrev-all-buffers
            ;; Try to expand word "dynamically", searching the kill ring.
            try-expand-dabbrev-from-kill
            ;; Try to complete text as a file name, as many characters as unique.
            try-complete-file-name-partially
            ;; Try to complete text as a file name.
            try-complete-file-name
            ;; Try to expand word before point according to all abbrev tables.
            try-expand-all-abbrevs
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-list
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-line
            ;; Try to complete as an Emacs Lisp symbol, as many characters as
            ;; unique.
            try-complete-lisp-symbol-partially
            ;; Try to complete word as an Emacs Lisp symbol.
            try-complete-lisp-symbol))
    )
  :config
  (progn
    (company-quickhelp-mode 1)
    (global-set-key (kbd "M-;") 'company-dabbrev)
    (global-company-mode t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-leader
  :ensure t
  :config
  (progn
    ;; Evil Leader
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      ;; registries
      "u" 'undo-tree-visualize
      "q" 'helm-show-kill-ring
      ;; file actions
      "b" 'helm-mini
      "k" 'kill-this-buffer
      "e" 'helm-find-files
      "E" 'sudo-edit
      "w" 'save-buffer
      "d" 'deer ;;zp to switch to ranger
      "v" 'er/expand-region
      ";" 'evil-commentary
      ;; window navigation
      "o" 'other-window
      "O" 'ace-window
      "C-o" 'ace-swap-window
      "1" 'delete-other-windows
      "2" 'split-window-below
      "3" 'split-window-right
      "0" 'delete-window
      ;; macro
      "(" 'kmacro-start-macro
      ")" 'kmacro-end-or-call-macro
      ;; evil window navigation
      "<tab>" 'evil-switch-to-windows-last-buffer
      "H" 'evil-window-left
      "L" 'evil-window-right
      "J" 'evil-window-down
      "K" 'evil-window-up
      ;; dumb-jump to go to definitions
      "." 'dumb-jump-go
      ">" 'dumb-jump-back
      ;; M-x
      "<SPC>" 'helm-M-x
      ;; jumps
      "jj" 'evil-avy-goto-char
      "jl" 'evil-avy-goto-line
      "jw" 'evil-avy-goto-word-1
      ;; helm
      "hs" 'helm-swoop-without-pre-input
      "hS" 'helm-swoop
      "hi" 'helm-imenu
      "hI" 'helm-imenu-in-all-buffers
      "he" 'helm-recentf
      "hc" 'helm-colors
      "hpg" 'helm-projectile-grep
      "hpa" 'helm-do-grep-ag
      ;; describe
      "hdk" 'describe-key
      "hdv" 'describe-variable
      "hdf" 'describe-function
      "pf" 'helm-projectile
      "pe" 'helm-projectile-recentf
      "zz" 'save-buffers-kill-terminal
      "tl" 'transpose-lines
      "tw" 'transpose-words
      "'" 'shell-pop
      ;; f for hydra prefix hydra
      "fB" 'puda-hydra-buffer-move/body
      "fb" 'puda-hydra-buffer-actions/body
      "fs" 'puda-hydra-scroll-page/body
      ;; frame control
      "52" 'make-frame-command
      "50" 'delete-frame
      "5o" 'other-frame
      ;;magit
      "gs" 'magit-status
      ;; auto-yasnippet
      "ac" 'aya-create
      "ae" 'aya-expand
      "ao" 'aya-open-line
      ;; ag
      ;; "/" 'ag
      "/" 'ag-project
      "*" 'ag-project-at-point
      ;; winner
      "[" 'winner-undo
      "]" 'winner-redo
      )
    (global-evil-leader-mode t)))

(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode t)
    ;; tweaks
    (define-key evil-normal-state-map "Y" (kbd "y$")) ;; because Y should do y$ theoretically
    (define-key evil-motion-state-map "Y" (kbd "y$")) ;; because Y should do y$ theoretically
    (define-key key-translation-map (kbd "ESC") 'my-esc) ;; for using esc to exit like C-g
    ;;;;;;;;;; default modes ;;;;;;;;;;;;;;;
    (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
    (setq evil-emacs-state-modes nil)
    ;; modes to start in emacs state
    (evil-set-initial-state 'magit-popup-mode 'emacs)
    (evil-set-initial-state 'ediff-mode 'emacs)

    ;; remove all keybindings from insert-state keymap
    ;; but [escape] should switch back to normal state
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    ))

(use-package evil-commentary
  :ensure t
  :config
  (progn
    (evil-commentary-mode)
    ))

(use-package evil-surround
  :ensure t
  :config
  (progn
    (global-evil-surround-mode t)
    ))

(use-package evil-visualstar
  :ensure t
  :config
  (progn
    (global-evil-visualstar-mode)
    ))

(use-package evil-matchit
  :ensure t
  :config
  (progn
    (global-evil-matchit-mode t)
    ))

(use-package evil-escape
  :ensure t
  :config
  (progn
    (setq-default evil-escape-key-sequence "fd")
    (setq-default evil-escape-unordered-key-sequence t)
    (evil-escape-mode t)))

(use-package evil-snipe
  :ensure t
  :init
  (progn
    (setq
     evil-snipe-scope 'visible
     evil-snipe-repeat-scope 'visible
     ))
  :config
  (progn
    (evil-snipe-override-mode t)
  ))

(use-package evil-mc
  :ensure t
  :config
  (progn
    (global-evil-mc-mode t)
    (add-hook 'evil-mc-after-cursors-deleted
              (defun puda/clear-anzu () (setq anzu--state nil)))))

(use-package evil-org
  :ensure t
  )

(use-package evil-magit
  :ensure t
  :config
  (progn
    (evil-magit-init)
    ;; go to insert mode when commiting
    (add-hook 'with-editor-mode-hook 'evil-insert-state)))

(use-package diff-hl
  :ensure t
  :config
  (progn
    (global-diff-hl-mode t)
    ))

(use-package dumb-jump
  :ensure t
  :config
  (progn
    ;; dumb-jump   | create .dumbjump file if project not found
    (dumb-jump-mode t)))

(use-package hydra
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (progn
    ;; keychords
    (key-chord-mode t)
    (setq key-chord-two-keys-delay 0.1)
    (key-chord-define evil-normal-state-map "fk" 'vertigo-jump-up)
    (key-chord-define evil-normal-state-map "fj" 'vertigo-jump-down)
    (key-chord-define evil-motion-state-map "fk" 'vertigo-jump-up)
    (key-chord-define evil-motion-state-map "fj" 'vertigo-jump-down)
    ;; using hydra for evil keybindings in minibuffer
    (key-chord-define minibuffer-local-map "jk" 'helm-like-unite/body)))

(use-package evil-args
  :ensure t
  :config
  (progn
    ;; evil-args
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "gl" 'evil-forward-arg)
    (define-key evil-normal-state-map "gh" 'evil-backward-arg)
    (define-key evil-motion-state-map "gl" 'evil-forward-arg)
    (define-key evil-motion-state-map "gh" 'evil-backward-arg)
    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)))

(use-package evil-exchange
  :ensure t
  :config
  (progn
    ;; evil exchange
    (evil-exchange-install)))

;;;;;;;;;;;;;;;;;;;;;;;;;; cleaning modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :ensure t
  :config
  (progn
    (eval-after-load "smartparens"
      '(diminish 'smartparens-mode))

    (eval-after-load "rainbow"
      '(diminish 'rainbow-mode))

    (eval-after-load "beacon"
      '(diminish 'beacon-mode))

    (eval-after-load "volatile-highlights-mode"
      '(diminish 'volatile-highlights-mode))

    (eval-after-load "auto-revert"
      '(diminish 'auto-revert-mode))

    (eval-after-load "evil-escape"
      '(diminish 'evil-escape-mode))

    (eval-after-load "evil-snipe"
      '(diminish 'evil-snipe-mode))

    (eval-after-load "anzu"
      '(diminish 'anzu-mode))

    (eval-after-load "evil-anzu"
      '(diminish 'evil-anzu))

    (eval-after-load "linum-relative"
      '(diminish 'linum-relative-mode))

    (eval-after-load "vim-empty-lines"
      '(diminish 'vim-empty-lines-mode))

    (eval-after-load "golden-ratio"
      '(diminish 'golden-ratio-mode))

    (eval-after-load "guide-key"
      '(diminish 'guide-key-mode))

    (eval-after-load "undo-tree"
      '(diminish 'undo-tree-mode))

    (eval-after-load "projectile"
      '(diminish 'projectile-mode))

    (eval-after-load "yas-minor-mode"
      '(diminish 'yas-minor-mode))

    (eval-after-load "auto-complete"
      '(diminish 'auto-complete-mode))

    (eval-after-load "yasnippet"
      '(diminish 'yas-minor-mode))

    (eval-after-load "flyspell"
      '(diminish 'flyspell-mode))

    (eval-after-load "eldoc"
      '(diminish 'eldoc-mode))

    (eval-after-load "git-gutter"
      '(diminish 'git-gutter-mode))

    (eval-after-load "whitespace-cleanup-mode"
      '(diminish 'whitespace-cleanup-mode))

    (eval-after-load 'flycheck
      '(diminish 'flycheck-mode))

    (diminish 'abbrev-mode)
    (eval-after-load 'auto-revert-mode
      '(diminish 'auto-revert-mode))

    (eval-after-load 'magit-wip
      '(diminish 'magit-wip-save-mode))

    (defvar mode-line-cleaner-alist
      `((auto-complete-mode . " α")
        (yas/minor-mode . " υ")
        (paredit-mode . " π")
        (eldoc-mode . "")
        (abbrev-mode . "")
        (company-mode . "")
        (rainbow-mode . "")
        (beacon-mode . "")
        (volatile-highlights-mode . "")
        (auto-revert-mode . "")
        (evil-escape-mode . "")
        (evil-snipe-mode . "")
        (anzu-mode . "")
        (evil-anzu . "")
        (vim-empty-lines-mode . "")
        (linum-relative-mode . "")
        (helm-mode . "")
        ;; Major modes
        (lisp-interaction-mode . "λ")
        (hi-lock-mode . "")
        (python-mode . "Py")
        (emacs-lisp-mode . "EL")
        (nxhtml-mode . "nx"))
      "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


    (defun clean-mode-line ()
      (interactive)
      (loop for cleaner in mode-line-cleaner-alist
            do (let* ((mode (car cleaner))
                      (mode-str (cdr cleaner))
                      (old-mode-str (cdr (assq mode minor-mode-alist))))
                 (when old-mode-str
                   (setcar old-mode-str mode-str))
                 ;; major mode
                 (when (eq mode major-mode)
                   (setq mode-name mode-str)))))


    (add-hook 'after-change-major-mode-hook 'clean-mode-line)

    (defalias 'flymake-report-status 'flymake-report-status-slim)
    (defun flymake-report-status-slim (e-w &optional status)
      "Show \"slim\" flymake status in mode line."
      (when e-w
        (setq flymake-mode-line-e-w e-w))
      (when status
        (setq flymake-mode-line-status status))
      (let* ((mode-line " Φ"))
        (when (> (length flymake-mode-line-e-w) 0)
          (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
        (setq mode-line (concat mode-line flymake-mode-line-status))
        (setq flymake-mode-line mode-line)
        (force-mode-line-update)))
    ))

(use-package sudo-edit
  :ensure t
  )

(provide 'puda-packages)
;;; puda-packages.el ends here
