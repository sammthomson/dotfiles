;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A small, built-in-first configuration for Emacs 29 and later.

;;; Code:

(defconst samt/state-directory
  (expand-file-name "state/" user-emacs-directory))
(make-directory samt/state-directory t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-dialog-box nil
      sentence-end-double-space nil
      custom-file (expand-file-name "custom.el" samt/state-directory)
      package-user-dir (expand-file-name "elpa/" samt/state-directory)
      recentf-save-file (expand-file-name "recentf" samt/state-directory)
      save-place-file (expand-file-name "places" samt/state-directory)
      savehist-file (expand-file-name "savehist" samt/state-directory)
      auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" samt/state-directory))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(delete-selection-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(winner-mode 1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(when (fboundp 'repeat-mode)
  (repeat-mode 1))
(when (fboundp 'which-key-mode)
  (which-key-mode 1))

(setq-default indent-tabs-mode nil
              tab-width 4
              indicate-empty-lines t)

(defconst samt/backup-directory
  (expand-file-name "backups/" samt/state-directory))
(make-directory samt/backup-directory t)
(setq backup-by-copying t
      backup-directory-alist `(("." . ,samt/backup-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-x a r") #'align-regexp)
(global-unset-key (kbd "C-z"))

(windmove-default-keybindings 'super)
(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)

(defun samt/smarter-move-beginning-of-line (arg)
  "Move to indentation or the beginning of the line.

Move forward ARG - 1 lines first when ARG is not 1."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((origin (point)))
    (back-to-indentation)
    (when (= origin (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                #'samt/smarter-move-beginning-of-line)

(defun samt/kill-region-or-line (begin end)
  "Kill the active region from BEGIN to END, or the current line."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2))))
  (kill-region begin end))

(global-set-key [remap kill-region] #'samt/kill-region-or-line)

;; Package setup
(require 'package)
(require 'seq)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defconst samt/packages
  '(expand-region
    magit
    markdown-mode
    multiple-cursors
    powershell
    yaml-mode))

(when (seq-some (lambda (package)
                  (not (package-installed-p package)))
                samt/packages)
  (package-refresh-contents)
  (dolist (package samt/packages)
    (unless (package-installed-p package)
      (package-install package))))

(require 'use-package)

;; Built-in navigation, completion, and programming support
(use-package completion-preview
  :ensure nil
  :if (fboundp 'global-completion-preview-mode)
  :config
  (global-completion-preview-mode 1))

(use-package eglot
  :ensure nil
  :commands eglot eglot-ensure
  :bind ("C-c l" . eglot))

(use-package flymake
  :ensure nil
  :commands flymake-mode
  :bind ("M-n" . flymake-goto-next-error)
  :bind ("M-p" . flymake-goto-prev-error))

(use-package project
  :ensure nil
  :bind (("C-c p f" . project-find-file)
         ("C-c p p" . project-switch-project)
         ("C-c p s" . project-find-regexp)))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1))

;; Maintained external packages
(use-package expand-region
  :bind ("M-=" . er/expand-region))

(use-package magit
  :commands magit-status magit-blame
  :bind (("C-c g" . magit-status)
         ("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package powershell
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psd1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-mode))

(defun samt/configure-zsh-buffer ()
  "Use Zsh syntax for Zsh files opened in `sh-mode'."
  (when (and buffer-file-name
             (string-match-p "\\(?:\\.zsh\\|zshrc\\)\\'" buffer-file-name))
    (sh-set-shell "zsh")))

(add-hook 'sh-mode-hook #'samt/configure-zsh-buffer)

(provide 'init)
;;; init.el ends here
