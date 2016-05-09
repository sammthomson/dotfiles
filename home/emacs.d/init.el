;; many of these come from http://aaronbedra.com/emacs.d/

;; GLOBAL SETTINGS

(setq user-full-name "Sam Thomson")
(setq user-mail-address "sammthomson@gmail.com")

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
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
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)

;; don't have to type out yes or no to prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; period single space ends sentence
(setq sentence-end-double-space nil)

(setq tab-width 2
      indent-tabs-mode nil)
;; auto indent after hitting return
(global-set-key (kbd "RET") 'newline-and-indent)
;;(global-set-key (kbd "M-/") 'hippie-expand)

;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(global-set-key "\M-a" 'endless/backward-paragraph)
(global-set-key "\M-e" 'endless/forward-paragraph)

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line."
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
  "Go back up to previous blank line."
  (interactive "p")
  (endless/forward-paragraph (- n)))



;; PACKAGES

;; MELPA package management
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
;;  (add-to-list 'package-archives
;;	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
;; haml-mode
;;nodejs-repl
;;paredit
;;web-mode
;;yaml-mode

;; (defun sthomson/all-packages-installed-p ()
;;   (loop for pkg in sthomson/packages
;;         when (not (package-installed-p pkg)) do (return nil)
;;         finally (return t)))

;; (unless (sthomson/all-packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg sthomson/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))

(use-package org
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package flycheck
  :ensure t)
(use-package flycheck-cask
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

(use-package powerline
  :ensure t)
(powerline-default-theme)

;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode)
;; (global-auto-complete-mode t)

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 3)
  :config
  ;;;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))
(global-company-mode t)

(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

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

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))
(smartparens-global-mode t)
(show-paren-mode t)

;;(require 'ido)
(use-package flx-ido
  :ensure t)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t
      ido-use-faces nil
      flx-ido-threshold 10000)

(use-package projectile
  :ensure t
  :demand
  :diminish projectile-mode
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-gutter
  :ensure t)
(global-git-gutter-mode t)

;; provides history and searching on top of M-x.
(use-package smex
  :ensure t)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; Programming Languages

;; scala mode
(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :config
  (use-package ensime-expand-region
    :ensure t))
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

;; JavaScript
;; (add-to-list 'load-path "~/.emacs.d/js")
;; (load "js3.elc")
;; (setq auto-mode-alist (cons '("\\.json\\'" . js3-mode) auto-mode-alist))

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'js-custom)

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))
(add-hook 'coffee-mode-hook 'coffee-custom)

(use-package yaml-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))


(use-package writegood-mode
  :ensure t)
;; Markdown mode
(use-package markdown-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
;; (setq markdown-command "pandoc --smart -f markdown -t html")
;; (setq markdown-css-paths `(,(expand-file-name "markdown.css" abedra/vendor-dir)))


;; Fonts n colors n stuff
(use-package base16-theme
  :ensure t)
(load-theme 'base16-eighties-dark t)


(set-face-attribute 'default nil
		    :family (if (eq system-type 'darwin)
				"Menlo"  ;; i like its "a" better than Monaco's
			      "DejaVu Sans Mono")
		    :height 140)

;; make terminal output with colors work
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
