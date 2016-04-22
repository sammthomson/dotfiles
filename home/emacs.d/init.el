;; "Customize" customizations
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;; set emacs to interface with system clipboard
(setq x-select-enable-clipboard t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; show current column number
(setq column-number-mode t)
;; keep backups out of the way
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; too easy to suspend process by accident
(global-unset-key (kbd "C-z"))

;; JavaScript
(add-to-list 'load-path "~/.emacs.d/js")
(load "js3.elc")
(setq auto-mode-alist (cons '("\\.json\\'" . js3-mode) auto-mode-alist))

;; LKB mode
(let ((root (or (getenv "DELPHINHOME")
                  "/opt/delphin")))
    (if (file-exists-p (format "%s/lkb/etc/dot.emacs" root))
      (load (format "%s/lkb/etc/dot.emacs" root) nil t t)))

;; scala mode
(add-to-list 'load-path "~/.emacs.d/scala")
(require 'scala-mode-auto)

;; MELPA package management
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
;;  (add-to-list 'package-archives
;;	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
)

