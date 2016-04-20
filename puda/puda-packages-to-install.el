;;; package --- Puda
;;; Commentary:
;;; Puda install missing packages
;;; Code:
(setq puda-packages '(ac-emmet ac-html ac-js2 ace-window ag airline-themes angular-mode angular-snippets anzu async auto-complete auto-indent-mode auto-package-update auto-yasnippet avy avy-zap better-defaults bind-key bind-map browse-kill-ring buffer-move cl-generic company company-flx company-quickhelp config-parser dash diff-hl diminish dmenu dumb-jump ecb elscreen emmet-mode epl esxml evil evil-anzu evil-args evil-avy evil-commentary evil-ediff evil-escape evil-exchange evil-indent-plus evil-indent-textobject evil-leader evil-magit evil-matchit evil-mc evil-org evil-snipe evil-surround evil-visualstar expand-region f flx flx-ido flycheck flymake-easy flymake-php fullframe geben ggtags git-commit golden-ratio goto-chg grizzl guide-key haml-mode helm helm-core helm-cscope helm-projectile helm-swoop htmlize hydra ido-completing-read+ ido-ubiquitous ido-vertical-mode iedit impatient-mode js2-mode key-chord kv let-alist linum-relative magit magit-popup memoize monokai-theme multiple-cursors names org org-bullets php-auto-yasnippets php-mode pkg-info popup popwin pos-tip powerline powerline-evil pretty-mode projectile rainbow-delimiters rainbow-mode ranger rich-minority s sass-mode scss-mode seq shell-pop simple-httpd skewer-mode skewer-reload-stylesheets smart-mode-line smart-mode-line-powerline-theme smartparens smex start-menu switch-window tablist tree-mode undo-tree use-package vertigo vimgolf visual-fill-column volatile-highlights web-beautify web-completion-data web-mode windata with-editor xcscope xelb yasnippet))

; fetch the list of packages available
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; install the missing packages
(dolist (package puda-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'puda-packages-to-install)
;;; puda-packages-to-install.el ends here
