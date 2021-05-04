;; init --- Initial setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Pre-Package Initialization

;; Set all packages to compile async
(setq-default comp-deferred-compilation t)

;; Remove built-in version of Org from the load-path
(require 'cl-seq)
(setq-default load-path
	      (cl-remove-if
	       (lambda (x)
		 (string-match-p "org$" x))
	       load-path))

;;;; Initialize Package

;; This is only needed once, near the top of the file

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t)

;; Bootstrap `use-package'
(setq-default use-package-verbose nil ; Don't report loading details
              use-package-enable-imenu-support t ; Let imenu find use-package defs
              use-package-expand-minimally t) ; minimize expanded code

(straight-use-package 'use-package)
(straight-use-package 'general)
(require 'general)

;;;; Global Helper Functions
(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(defun my/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;;; Package Configuration

(use-package gcmh
  ;; Minimizes GC interference with user activity.
  :config (gcmh-mode 1))

(use-package no-littering
  ;; cleanup all the clutter from varios modes
  ;; places configs in /etc and data in /var
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Automatically bisects init file
(use-package bug-hunter)

(use-package company
  ;; text completion framework
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-show-numbers t)
  (company-minimum-prefix-length 3)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay 0)
  :config
  (global-company-mode t))

(use-package aggressive-indent
  ;; Indent as you type
  :hook ((prog-mode text-mode) . aggressive-indent-mode)
  :config (add-to-list 'aggressive-indent-excluded-modes 'org-mode))

(use-package rainbow-delimiters
  ;; Change color of each inner block delimiter
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package guix
  :if (memq system-type '(gnu/linux))
  :hook (scheme-mode . guix-devel-mode))

(use-package geiser-guile
  :if (memq system-type '(gnu/linux))
  :custom
  (geiser-mode-start-repl-p t)
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

;; Line wrap at the fill column, not buffer end
(use-package visual-fill-column)

(use-package exec-path-from-shell
  ;; Load path from user shell
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  ;; code linter
  :init (global-flycheck-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height 18)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (display-graphic-p))
  :config
  (doom-modeline-mode 1))

(use-package selectrum
  ;; selection/completion manager
  :config (selectrum-mode +1))

(use-package prescient
  ;; sorting manager
  :config
  (prescient-persist-mode +1))

(use-package org)

(use-package selectrum-prescient
  ;; make selectrum use prescient sorting
  :after (selectrum prescient)
  :config (selectrum-prescient-mode +1))

(use-package company-prescient
  ;; make company use prescient filtering
  :after (company prescient)
  :config (company-prescient-mode +1))

(use-package consult
  ;; enhances navigation with selectrum completions
  :after (selectrum projectile)
  :commands (projectile-project-root)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (xref-show-xrefs-finction #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<"))

(use-package consult-flycheck
  ;; add a consult-flycheck command
  :after (consult flycheck))

(use-package marginalia
  ;; adds annotations to consult
  :straight (:branch "main")
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (marginalia-mode))

(use-package embark
  ;; provide actions on competion candidates, or text at point
  :custom
  (embark-prompt-style 'completion))

(use-package projectile
  ;; project traversal
  :preface
  (defun my/projectile-ignore-project (project-root)
    (f-descendant-of? project-root (expand-file-name "~/.emacs.d/straight/")))
  :config
  (projectile-mode 1))

;; faster grep
(use-package rg)

(use-package magit
  ;; emacs interface for git
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    "Set magit status to full-screen."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defadvice magit-mode-quit-window (around magit-quit-session activate)
    "Restore previous window configuration if we are burying magit-status."
    (if (equal (symbol-name major-mode) "magit-status-mode")
        (progn
          ad-do-it
          (jump-to-register :magit-fullscreen))
      ad-do-it
      (delete-other-windows))))

(use-package apheleia
  :straight
  (apheleia :type git
	    :host github
	    :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--single-quote" "true"
              "--trailing-comma" "es5"
              "--jsx-single-quote" "true"
              "--arrow-parens" "avoid"
              file))
  (apheleia-global-mode +1))

(use-package json-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js-mode
          json-mode
          mhtml-mode
          yaml-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-indentation nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-prefer-capf t)
  ;; Config specific to tsserver/theia ide
  (lsp-clients-typescript-log-verbosity "off")
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :config
  (push "[/\\\\]node_modules$" lsp-file-watch-ignored)
  (push "[/\\\\]build$" lsp-file-watch-ignored)
  (setenv "TSSERVER_LOG_FILE" (no-littering-expand-var-file-name "lsp/tsserver.log"))
  (advice-add 'lsp :before (lambda (&rest _args)
                             (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package sml-mode)

(use-package paredit)

(use-package tree-sitter
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package undo-tree
  ;; make undo a tree rather than line
  :config (global-undo-tree-mode))

(use-package gruvbox-theme
  ;; Groovy
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package yaml-mode
  ;; formatting for yml files
  :mode "\\.yml\\'")

(use-package which-key
  ;; shows list of available completions when key sequences begin
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode))

(use-package general
  ;; key binding manager
  :preface
  (defun my/switch-to-last-buffer ()
    "Flip between two buffers."
    (interactive)
    (switch-to-buffer nil))
  :general
  (;; Basic Overrides
   "C-." 'consult-line
   "C-," 'comment-or-uncomment-region
   "M-y" 'consult-yank-pop
   "<help> a" 'consult-apropos
   "M-g g" 'consult-goto-line
   "M-g M-g" 'consult-goto-line
   "M-g m" 'consult-mark
   ;; C-x bindings
   "C-x b" 'consult-buffer
   "C-x 4 b" 'consult-buffer-other-window
   "C-x f" 'consult-find
   "C-x r q" 'save-buffers-kill-terminal
   ;; C-c bindings (user-map)
   "C-c i" 'consult-imenu
   "C-c I" 'consult-project-imenu
   "C-c t" 'tab-bar-switch-to-tab
   "C-c f" 'consult-flycheck
   "C-c v" 'magit
   "C-c h" 'consult-history
   "C-c m" 'consult-mode-command
   "C-c r" 'my/switch-to-last-buffer
   '[f1] 'projectile-find-file
   '[f2] 'consult-ripgrep
   '[f3] 'projectile-switch-project)
  (:keymaps 'isearch-mode-map
            "M-e" 'consult-isearch
            "M-s l" 'consult-line))

;;;; Built-in Package Config

(use-package xref
  ;; find identifier in prog modes
  :straight nil
  :custom
  (xref-search-program 'ripgrep))

(use-package elec-pair
  ;; automatically match pairs
  :straight nil
  :config
  (electric-pair-mode 1))

(use-package tab-bar
  ;; save workspaces as groups of windows
  :preface
  (defun my/no-tab-bar-lines (&rest _)
    "Hide the tabs from tab-bar-mode."
    (dolist (frame (frame-list)) (set-frame-parameter frame 'tab-bar-lines 0)))
  :straight nil
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show nil)
  :config
  (advice-add #'tab-bar-mode :after #'my/no-tab-bar-lines)
  (advice-add #'make-frame :after #'my/no-tab-bar-lines)
  (tab-bar-mode 1))

(use-package desktop
  ;; Save buffers and windows on exit
  :straight nil
  :if (memq system-type '(gnu/linux))
  :custom
  (desktop-restore-eager 5)
  :config
  (desktop-save-mode 1))

(use-package dired
  ;; directory management
  :straight (:type built-in)
  :hook (dired-mode . dired-hide-details-mode))

(use-package server
  ;; The emacs server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package vc
  ;; Make backups of files, even when they're in version control
  :straight nil
  :custom
  (vc-make-backup-files t))

(use-package uniquify
  ;; Add parts of each file's directory to the buffer name if not unique
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package eldoc
  ;; Use Eldoc for elisp
  :straight nil
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode))

(use-package autorevert
  ;; Auto refresh buffers
  :straight nil
  :config
  (global-auto-revert-mode 1))

(use-package subword
  ;; Easily navigate sillycased words
  :straight nil
  :config
  (global-subword-mode 1))

(use-package saveplace
  ;; Store cursor location between sessions
  :straight nil
  :config
  (save-place-mode 1))

(use-package sql
  :straight nil
  :custom
  (sql-product 'postgres))

(use-package files
  ;; General file handling
  :straight nil
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (vc-follow-symlinks t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "2:00am"))

(use-package emacs
  :straight nil
  :custom
  (inhibit-startup-message t)     ; no splash screen
  (visible-bell t)                ; be quiet
  (indicate-empty-lines t)        ; show lines at the end of buffer
  (sentence-end-double-space nil) ; single space after a sentence
  (indent-tabs-mode nil)          ; use spaces instead of tabs
  (reb-re-syntax 'rx)             ; interactive regex builder
  (cursor-type '(bar . 2))
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  :config
  (delete-selection-mode)
  (fset 'yes-or-no-p 'y-or-n-p)   ; use y or n to confirm
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode 1)             ; Show matching parens
  (set-frame-font "Hack-14")
  (when (eq system-type 'darwin)
    (set-face-attribute 'default (selected-frame) :font "Hack" :height 180)
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)
    (setq auto-save-default nil))
  (global-hl-line-mode 1)
  (set-face-background 'cursor "red")
  (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
  (windmove-default-keybindings))

(provide 'init)
;;; init.el ends here
