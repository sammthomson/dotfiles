;;; init.el -- initialization script for Emacs
;;; Commentary:
;; many of these come from
;; http://aaronbedra.com/emacs.d/
;; https://github.com/bodil/emacs.d
;; https://zeekat.nl/articles/making-emacs-work-for-me.html
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html


;;; Code:

;; TODO: break up into separate files?

;; GLOBAL SETTINGS

(setq user-full-name "Sam Thomson")
(setq user-mail-address "sammthomson@gmail.com")

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(when (not window-system)
  (menu-bar-mode -1)
  (xterm-mouse-mode +1))

(setq echo-keystrokes 0.05
      use-dialog-box nil)
;; (setq visible-bell t)  ;; still too annoying
(setq ring-bell-function 'ignore)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; act a little more like normal text editors for selections
(delete-selection-mode t)
(transient-mark-mode t)
;; interface with system clipboard
(setq x-select-enable-clipboard t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; show column number
(setq column-number-mode t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; keep backups out of the way
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; too easy to suspend process by accident
(global-unset-key (kbd "C-z"))

;; shortcuts for switching window panes
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>")  'windmove-right)
(global-set-key (kbd "s-<up>")  'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)

;; http://endlessparentheses.com/Meta-Binds-Part-1-3A-Drunk-in-the-Dark.html
(global-set-key "\M-[" 'backward-sexp)
(global-set-key "\M-]" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-0" 'delete-window)

;; don't have to type out yes or no to prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; period single space ends sentence
(setq sentence-end-double-space nil)

(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(setq standard-indent 2)
;; auto indent after hitting return
(global-set-key (kbd "RET") 'newline-and-indent)
;;(global-set-key (kbd "M-/") 'hippie-expand)

;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(global-set-key "\M-a" 'endless/backward-paragraph)
(global-set-key "\M-e" 'endless/forward-paragraph)

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line.  Optionally repeat N times."
  (interactive "p")
  (let ((para-commands
         '(endless/forward-paragraph endless/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or (use-region-p)
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))))

(defun endless/backward-paragraph (&optional n)
  "Go back up to previous blank line.  Optionally repeat N times."
  (interactive "p")
  (endless/forward-paragraph (- n)))




;; PACKAGES

;; MELPA package management
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
;;  (add-to-list 'package-archives
;;        '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))  ;; use-package is a macro, so not needed at runtime

(require 'diminish)
(require 'bind-key)



;;clojure-mode
;;coffee-mode
;;deft
;;feature-mode
;;graphviz-dot-mode
;;haml-mode
;;nodejs-repl
;;paredit
;;web-mode
;;yaml-mode

;; (defun samt/all-packages-installed-p ()
;;   (loop for pkg in samt/packages
;;         when (not (package-installed-p pkg)) do (return nil)
;;         finally (return t)))

;; (unless (samt/all-packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg samt/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))


;;;; TODO: check network connectivity before auto-installing packages?
;;(setq samt/ensure-p t)

;; OSX doesn’t set the environment from the shell init files for graphical
;; applications but we want it to.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package org
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (use-package flycheck-cask
	:commands flycheck-cask-setup
	:config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup))))


(setq ns-use-srgb-colorspace t)

(use-package smart-mode-line
  :ensure t
  :init
  (use-package nyan-mode
    :ensure t
    :init
    (setq nyan-wavy-trail nil
          nyan-animate-nyancat t)
    :config
    (nyan-mode t))
  ;; (setq sml/shorten-directory t)
  ;; (setq sml/shorten-modes t)
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode)
;; (global-auto-complete-mode t)

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :init
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 2)
  :config
  ;;;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))
(global-company-mode t)

(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

;;;; Trying smartparens instead
;; (use-package autopair
;;   :ensure t
;;   :diminish autopair-mode)
;; (autopair-global-mode t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  (sp-pair "\(" "\)" :wrap)  ;; for latex
  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))
(smartparens-global-mode t)
(show-paren-mode t)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;(require 'ido)
(use-package flx-ido
  :ensure t
  :init
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t
      ido-use-faces nil
      flx-ido-threshold 10000)
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

(use-package ido-completing-read+
  :ensure t)

;; ;; imenu-anywhere doesn't work without this
;; (use-package helm
;;   :ensure t)

;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind ("C-." . imenu-anywhere))

(use-package projectile
  :ensure t
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ido)
  :config (projectile-global-mode t)
  :bind   (("M-p" . projectile-switch-project)
		   ("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(use-package expand-region
  :ensure t
  :bind ("M-=" . er/expand-region))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init (setq magit-revert-buffers nil)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:hide-gutter t)  ;; hide if no changes
  ;; indicate with colors only
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign    " "
        git-gutter:deleted-sign  " ")
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added    "green")
  (set-face-background 'git-gutter:deleted  "red"))
(global-git-gutter-mode t)

;; provides history and searching on top of M-x.
(use-package smex
  :ensure t
  :init (setq
         smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))


;; Programming Languages

;; scala mode
(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :config
  (require 'ensime-expand-region))
(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
            (company-mode)
            (ensime-mode)
            (make-local-variable 'company-backends)
            (projectile-visit-project-tags-table)
            (setq company-backends
                  '(ensime-company (company-keywords
                                    company-dabbrev-code
                                    company-etags
                                    company-yasnippet)))
            (scala-mode:goto-start-of-code)))

(use-package haskell-mode
  :ensure t)

(use-package idris-mode
  :ensure t)

(add-hook 'js-mode-hook
		  (lambda ()
			(setq js-indent-level 2)))

;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;   (make-local-variable 'tab-width)
;;   (set 'tab-width 2))
;; (add-hook 'coffee-mode-hook 'coffee-custom)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$"))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))


(use-package writegood-mode
  :ensure t)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md$"
		 "\\.mdown$")
  :config
  (visual-line-mode t)
  (writegood-mode t)
  (flyspell-mode t))
;; (setq markdown-command "pandoc --smart -f markdown -t html")
;; (setq markdown-css-paths `(,(expand-file-name "markdown.css" abedra/vendor-dir)))


;; Fonts n colors n stuff
(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-eighties-dark t))

(set-face-attribute 'default nil
                    :family (if (eq system-type 'darwin)
                                "Menlo"  ;; i like its "a" better than Monaco's
                              "DejaVu Sans Mono")
                    :height 140)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Make terminal output with colors work."
  (read-only-mode t)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode nil))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; init.el ends here
