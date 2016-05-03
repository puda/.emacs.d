;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key evil-normal-state-map (kbd "[ SPC") (lambda() (interactive)(evil-insert-newline-above) (forward-line)))
(define-key evil-normal-state-map (kbd "] SPC") (lambda() (interactive)(evil-insert-newline-below) (forward-line -1)))

(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)

(define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
(define-key evil-visual-state-map (kbd "] e") ":move'>+1")

;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)

(define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
(define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)

(define-key evil-normal-state-map (kbd "] l") 'spacemacs/next-error)
(define-key evil-normal-state-map (kbd "[ l") 'spacemacs/previous-error)

(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)

(define-key evil-normal-state-map (kbd "[ t") (lambda () (interactive)(raise-frame (previous-frame) (focus-frame))))
(define-key evil-normal-state-map (kbd "] t") (lambda () (interactive)(raise-frame (next-frame) (focus-frame))))

(define-key evil-normal-state-map (kbd "[ w") 'previous-multiframe-window)
(define-key evil-normal-state-map (kbd "] w") 'next-multiframe-window)

;; select pasted text
(define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

;; paste above or below with newline
(define-key evil-normal-state-map (kbd "[ p") 'puda-paste-above)
(define-key evil-normal-state-map (kbd "] p") 'puda-paste-below)

;;;; general
(global-set-key (kbd "C-v") 'yank) ; paste
(global-set-key (kbd "C-z") 'undo) ; undo
(global-set-key (kbd "C-x m") 'eshell) ; terminal
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "<C-return>") 'puda-add-semi-colon)
(global-set-key (kbd "<C-S-return>") 'puda-move-cursor-above)
;;;; helm transient
(define-key helm-map (kbd "M-SPC") 'helm-like-unite/body)
(provide 'puda-misc-keybindings)
;;; puda-misc-keybindings.el ends here
