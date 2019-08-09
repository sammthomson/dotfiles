;;; init.el -- initialization script for Emacs
;;; Commentary:
;; many of these come from
;; http://aaronbedra.com/emacs.d/
;; https://github.com/bodil/emacs.d
;; https://zeekat.nl/articles/making-emacs-work-for-me.html
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; https://github.com/sachac/.emacs.d

;;; Code:

;; TODO: break up into separate files?

;; GLOBAL SETTINGS
(setq user-full-name "Sam Thomson")
(setq user-mail-address "sammthomson@gmail.com")

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(when (not (display-graphic-p))
  (menu-bar-mode -1)
  (xterm-mouse-mode -1))

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

;; too easy to suspend process by accident
(global-unset-key (kbd "C-z"))

;; align text according to a delimiter
(global-set-key (kbd "C-x a r") 'align-regexp)

;; shortcuts for switching windows
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>")  'windmove-right)
(global-set-key (kbd "s-<up>")  'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)

;; http://endlessparentheses.com/Meta-Binds-Part-1-3A-Drunk-in-the-Dark.html
(global-set-key "\M-[" 'backward-sexp)
(global-set-key "\M-]" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-0" 'delete-window)

;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(global-set-key "\M-{" 'endless/backward-paragraph)
(global-set-key "\M-}" 'endless/forward-paragraph)
(global-set-key (kbd "C-<up>") 'endless/backward-paragraph)
(global-set-key (kbd "C-<down>") 'endless/forward-paragraph)

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

(setq ad-redefinition-action 'accept)


;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))


;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)



;; PACKAGES

;; MELPA package management
(when (>= emacs-major-version 24)
  (require 'package)
;;  (add-to-list 'package-archives
;;               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
        '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))  ;; use-package is a macro, so not needed at runtime
;;(require 'diminish)
(require 'bind-key)

;;; Enable to debug slow startup
;;(setq use-package-minimum-reported-time 0)
;;(setq use-package-verbose t)

(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Fonts n colors n stuff
;; (use-package base16-theme
;;   :init
;;   (load-theme 'base16-eighties-dark t))

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



;;coffee-mode
;;graphviz-dot-mode
;;paredit
;;web-mode

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
  (use-package exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


;; # Winner mode - undo and redo window configuration
;; `winner-mode' lets you use `C-c <left>' and `C-c <right>' to switch between
;; window configurations.
;; This is handy when something has popped up a buffer that you want to look at
;; briefly before returning to whatever you were working on.
;; When you're done, press `C-c <left>'.
(use-package winner
  :defer t
  :init (winner-mode t))

(use-package org
  :disabled t)

(use-package htmlize
  :disabled t)

(use-package flycheck
  :defer t
  :init
  (global-flycheck-mode t)
  :config
  (use-package flycheck-cask
	:commands flycheck-cask-setup
	:config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup))))


(setq ns-use-srgb-colorspace t)

(use-package smart-mode-line
  :init
  ;; (use-package nyan-mode
  ;;   :init
  ;;   (setq nyan-wavy-trail nil
  ;;         nyan-animate-nyancat t)
  ;;   :config
  ;;   (nyan-mode t))
  ;; (setq sml/shorten-directory t)
  ;; (setq sml/shorten-modes t)
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; (use-package auto-complete
;;   :diminish auto-complete-mode)
;; (global-auto-complete-mode t)

(use-package company
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

;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode)
;;   :bind ("s-/" . undo-tree-visualize))

;;;; Trying smartparens instead
;; (use-package autopair
;;   :diminish autopair-mode)
;; (autopair-global-mode t)

(use-package smartparens
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
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package flx-ido
  :defer t
  :init
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t
		ido-use-faces nil
		flx-ido-threshold 10000)
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t)
  :config
  (use-package ido-vertical-mode
	:config (ido-vertical-mode t))

  (use-package ido-completing-read+))

;; ;; imenu-anywhere doesn't work without this
;; (use-package helm)

;; (use-package imenu-anywhere
;;   :bind ("C-." . imenu-anywhere))

(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ido)
  :config (projectile-global-mode t)
  :bind   (("s-P" . projectile-switch-project)
		   ("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(use-package expand-region
  :bind ("M-=" . er/expand-region))

(use-package yasnippet
  :disabled                  ;; not actually using yet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package magit
  :commands magit-status magit-blame
  :init (setq magit-revert-buffers nil)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-gutter
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
  :init (setq
         smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))


;; Programming Languages

;; scala mode
(use-package ensime
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


;; ;;; LaTeX with AUCTeX, from https://github.com/ryanakca/ryanakca-dotfiles
;; (use-package tex-site                   ; AUCTeX initialization
;;   :ensure auctex)

;; (use-package tex                        ; TeX editing/processing
;;   :ensure auctex
;;   :defer t
;;   :config
;;   (progn
;;     (setq TeX-parse-self t              ; Parse documents to provide completion
;;                                         ; for packages, etc.
;;           TeX-auto-save t               ; Automatically save style information
;;           ;; Don't ask for confirmation when cleaning
;;           TeX-clean-confirm nil
;;           ;; Provide forward and inverse search with SyncTeX
;;           TeX-source-correlate-mode t
;;           TeX-source-correlate-method 'synctex)))


;; ;; Was messing up `forward-word' and `backward-word'.
;; (use-package haskell-mode
;;   :mode ("\\.hs$" "\\.lhs$" "\\.purs$"))

;; ;; Idris
;; (use-package idris-mode
;;   :mode ("\\.idr$" "\\.lidr" "\\.ipkg"))


;; ;; Purescript
;; (use-package purescript-mode
;;   :commands purescript-mode
;;   :mode (("\\.purs$" . purescript-mode))
;;   :config
;;   (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

;; ;; Install the psci mode.
;; (use-package psci
;;   :commands psci)

;; ;; Extend purescript-mode with psc-ide.
;; (use-package psc-ide
;;   :init
;;   ;; ;; psc-ide
;;   ;; (setq psc-ide-client-executable (or (ohai/resolve-exec "psc-ide-client") "psc-ide-client"))
;;   ;; (setq psc-ide-server-executable (or (ohai/resolve-exec "psc-ide-server") "psc-ide-server"))
;;   :after purescript-mode
;;   :config
;;   (add-hook 'purescript-mode-hook
;;             (lambda ()
;;               ;;(my/use-psc-ide-from-node-modules)
;;               (psc-ide-mode t)
;;               (company-mode t)
;;               (flycheck-mode t)
;;               (haskell-indentation-mode t)
;; 			  (setq psc-ide-rebuild-on-save nil))))

;; (use-package psc-ide
;;   :mode ("\\.purs"))

;; (add-hook 'purescript-mode-hook
;;   (lambda ()
;;     (psc-ide-mode)
;;     (company-mode)
;;     (flycheck-mode)
;;     (turn-on-purescript-indentation)))


;; ;; ATS
;; (add-to-list 'load-path
;; 			 (concat user-emacs-directory
;; 					 (convert-standard-filename "ats/")))
;; (load-library "ats2-mode")
;; ;(use-package ats2-mode
;; ;  :pin manual
;; ;  :mode ("\\.sats" "\\.dats" "\\.cats" "\\.hats"))

(use-package js2-mode
  :commands js2-mode
  :mode ("\\.js$" "\\.json$")
  :interpreter "node"
  :bind (:map js2-mode-map
			  ;; ("C-c C-c" . compile)
			  ("C-x C-e" . js-send-last-sexp)
			  ("C-c b"   . js-send-buffer)
			  ("C-c C-b" . js-send-buffer-and-g))
  :init
  (setq-default js2-basic-offset 2)
  :config
  (js2-imenu-extras-setup)
  (use-package js2-refactor
	:commands js2-refactor-mode
	:init
	(js2r-add-keybindings-with-prefix "C-c C-m")))


;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;   (make-local-variable 'tab-width)
;;   (set 'tab-width 2))
;; (add-hook 'coffee-mode-hook 'coffee-custom)

(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$"))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc" . sh-mode))
(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))

(use-package markdown-mode
  :mode ("\\.md$"
		 "\\.mdown$")
  :config
  (visual-line-mode t)
  (use-package writegood-mode)
  (writegood-mode t)
  (flyspell-mode t))
;; (setq markdown-command "pandoc --smart -f markdown -t html")
;; (setq markdown-css-paths `(,(expand-file-name "markdown.css" abedra/vendor-dir)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(markdown-mode yaml-mode js2-refactor js2-mode psc-ide psci purescript-mode idris-mode auctex ensime smex git-gutter magit expand-region projectile ido-completing-read+ ido-vertical-mode flx-ido multiple-cursors smartparens undo-tree company smart-mode-line flycheck-cask flycheck use-package pkg-info auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
