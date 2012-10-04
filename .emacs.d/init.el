;; "Customize" customizations
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;; set emacs to interface with system clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; show current column number
(setq column-number-mode t)
;; keep backups out of the way
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
