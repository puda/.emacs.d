;;; package --- Puda functions
;;; Commentary:
;;; hi :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;functions;;;;;;;;;;;;;;;;;;;;;

(defun puda-move-cursor-below ()
  "Inserts newline below the line and moves the pointer there and indents if necessary"
  (interactive)
  (move-end-of-line 1) (newline)
  (indent-for-tab-command))

(defun puda-move-cursor-above ()
  "Inserts newline above the line and moves the pointer there and indents if necessary"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun puda-add-semi-colon ()
  "Adds semi colon at the end"
  (interactive)
  (move-end-of-line 1)
  (insert ";"))

(defun my-esc (prompt)
     "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
     (cond
      ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
      (t (kbd "C-g"))))

(defun my-evil-toggle ()
  (interactive)
  (cond
    ((eq evil-state 'insert)
     (evil-emacs-state))
    ((eq evil-state 'emacs)
     (evil-exit-emacs-state))
    ((eq evil-state 'normal)
     (evil-emacs-state))))

(defun puda-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; disable mouse for emacs gui ;;;;;;;;;;;;;;;;;
(defun puda-turn-off-mouse ()
  "Turn off the trackpad as it clicks around."
  (defun turn-off-mouse (&optional frame)
    (interactive)
    (let ((inhibit-message t) (default-directory "~"))
      (shell-command "synclient TouchpadOff=1")))

  (defun turn-on-mouse (&optional frame)
    (interactive)
    (let ((inhibit-message t) (default-directory "~"))
      (shell-command "synclient TouchpadOff=0")))

  (add-hook 'focus-in-hook #'turn-off-mouse)
  (add-hook 'focus-out-hook #'turn-on-mouse)
  (add-hook 'kill-emacs-hook #'turn-on-mouse)
  (add-hook 'delete-frame-functions #'turn-on-mouse)
  )

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(global-set-key [remap move-beginning-of-line]
                'puda-move-beginning-of-line)

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

(provide 'puda-functions)
;;; puda-functions ends here
