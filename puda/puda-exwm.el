(use-package exwm
  :ensure t
  :config
  (progn
    (require 'exwm)
    (require 'exwm-config)
    (exwm-config-default)

    ;; window movement (easier for exwm)
    (global-unset-key "\M-h")
    ;; window splits
    (global-set-key (kbd "M-1") 'delete-other-windows)
    (global-set-key (kbd "M-2") 'split-window-below)
    (global-set-key (kbd "M-3") 'split-window-right)
    (global-set-key (kbd "M-0") 'delete-window)
    ;; window movement
    (global-set-key (kbd "M-h") 'evil-window-left)
    (global-set-key (kbd "M-j") 'evil-window-down)
    (global-set-key (kbd "M-k") 'evil-window-up)
    (global-set-key (kbd "M-l") 'evil-window-right)
    ;; ace window
    (global-set-key (kbd "M-o") 'ace-window)
    ;; switch buffers
    (global-set-key (kbd "M-b") 'helm-mini)
    ;; kill buffer
    (global-set-key (kbd "M-q") 'kill-this-buffer)
    ;; launch apps
    (global-set-key (kbd "M-d") '(closure (t) (command) (interactive (list
(read-shell-command "$ "))) (start-process-shell-command command nil
command)))
    ;; layouts options for exwm
    ;; (global-set-key (kbd "M-l") 'evil-window-right)

    ))

(provide 'puda-exwm)
;;; puda-exwm.el ends here
