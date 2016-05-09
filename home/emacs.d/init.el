;; Many of these come from http://aaronbedra.com/emacs.d/

;; GLOBAL SETTINGS

(setq user-full-name "Sam Thomson")
(setq user-mail-address "sammthomson@gmail.com")

;; for `loop for ...`, etc.
(require 'cl)

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



;; PACKAGES / MODES

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
  (require 'use-package))  ;; macro, not needed at runtime

(require 'diminish)
(require 'bind-key)


(defvar sthomson/packages
  '(;;clojure-mode
    ;;coffee-mode
    ;;deft
    ;;feature-mode
    flycheck
    ;;graphviz-dot-mode
    ;; haml-mode
    haskell-mode
    htmlize
    idris-mode
    markdown-mode
    ;;nodejs-repl
    org
    paredit
    ;; scala-mode  ;; manually installed
    ensime ;; for Scala
    smex
    base16-theme
    web-mode
    writegood-mode
    yaml-mode)
  "Default packages")

(defun sthomson/all-packages-installed-p ()
  (loop for pkg in sthomson/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (sthomson/all-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg sthomson/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(use-package powerline
  :ensure t)
(powerline-default-theme)

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode)
(global-auto-complete-mode t)

(use-package autopair
  :ensure t
  :diminish autopair-mode)
(autopair-global-mode 1)

(show-paren-mode t)

;;(require 'ido)
(use-package flx-ido
  :ensure t)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq flx-ido-threshold 10000)

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

;; provides history and searching on top of M-x.
(use-package smex
  :ensure t)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)




;; Programming Languages

;; scala mode
;;(add-to-list 'load-path "~/.emacs.d/scala")
;;(require 'scala-mode-auto)
(use-package ensime
  :ensure t
  :commands ensime ensime-mode)
(add-hook 'scala-mode-hook 'ensime-mode)
(add-hook 'scala-mode-hook 'scala-mode:goto-start-of-code)

(setq tab-width 2
      indent-tabs-mode nil)
;; auto indent after hitting return
(global-set-key (kbd "RET") 'newline-and-indent)
;;(global-set-key (kbd "M-/") 'hippie-expand)

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

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

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
