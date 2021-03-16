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
(eval-when-compile (require 'use-package)
                   (require 'general))

;;;; Global Helper Functions

(defun my/rails-server()
  "Start rails."
  (interactive)
  (shell "**RAILS**")
  (comint-send-string "**RAILS**" "cd ~/projects/evms-dashboard/; rails s")
  (comint-send-input))

(defun my/startup-evms-dashboard ()
  "Start rails server and grunt watcher."
  (interactive)
  (grunt-server)
  (rails-server))

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

;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)
(defun my/projectile-root-child-of (dir &optional list)
  "Let projectile find project root by LIST regexp of DIR."
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first
      (if (and
           (s-equals? (file-remote-p it) (file-remote-p dir))
           (string-match-p (expand-file-name it) (expand-file-name dir)))
          dir)
      (or list project-root-regexps (list))))))

(defvar project-root-regexps ()
  "List of regexps to match against when projectile is searching for project root directories.")
(add-to-list 'project-root-regexps "/Users/nicolas.winslow/interpreta/hca-ui/$")
;;(add-to-list 'project-root-regexps "/path/to/another/project/$")
;;(add-to-list 'project-root-regexps "/path/to/one/more/project/$")

(nconc projectile-project-root-files-functions '(my/projectile-root-child-of))

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
  :ghook ('(prog-mode-hook text-mode-hook)))

(use-package rainbow-delimiters
  ;; Change color of each inner block delimiter
  :ghook ('(prog-mode-hook text-mode-hook)))

(use-package ace-window)

;; Install a newer version of Org after removing the old
(use-package org)

;; Line wrap at the fill column, not buffer end
(use-package visual-fill-column)

(use-package outshine
  ;; Easier navigation for source files, especially this one
  :general
  (outshine-mode-map
   :states '(normal)
   "<tab>" 'outshine-cycle
   "<backtab>" 'outshine-cycle-buffer)
  :hook (emacs-lisp))

(use-package exec-path-from-shell
  ;; Load path from user shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  ;; code linter
  :preface
  (defun my/flycheck-error-selector ()
    (select-window (get-buffer-window "*Flycheck errors*")))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode)
  ;; Popup flycheck buffer at bottom
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33)))
  (advice-add 'flycheck-list-errors :after #'my/flycheck-error-selector))

(use-package flycheck-pos-tip
  ;; popup flycheck messages
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package doom-modeline
  :custom
  (doom-modeline-height 18)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-icon (display-graphic-p))
  :config
  (doom-modeline-mode 1))

(use-package selectrum
  ;; selection/completion manager
  :config (selectrum-mode 1))

(use-package prescient
  ;; sorting manager
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  ;; make selectrum use prescient sorting
  :after (selectrum prescient)
  :config (selectrum-prescient-mode 1))

(use-package orderless
  ;; candidate filtering package
  :after (selectrum-prescient)
  :init (icomplete-mode)
  :custom
  (completion-styles '(orderless))
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candiates-function #'orderless-highlight-matches))

(use-package company-prescient
  ;; make company use prescient filtering
  :after (company prescient)
  :config (company-prescient-mode 1))

(use-package consult
  ;; enhances navigation with selectrum completions
  :init (fset 'multi-occur #'consult-multi-occur))

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
  :custom
  (projectile-ignored-project-function #'my/projectile-ignore-project)
  :config
  (projectile-mode 1))

(use-package rg
  ;; faster grep
  :preface
  (defun my/rg-buffer-selector()
    (select-window (get-buffer-window "*rg*")))
  (defun my/rg-candidate-selector ()
    (interactive)
    (compilation-next-error 1)
    (compilation-display-error))
  (defun my/rg-select-and-quit ()
    (interactive)
    (compile-goto-error))
  :custom
  (next-error-hook nil)
  :hook (rg-filter . my/rg-buffer-selector)
  :general
  (rg-mode-map
   :states 'motion
   "TAB" 'my/rg-candidate-selector
   "RET" 'compile-goto-error))

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
  :commands lsp
  :hook ((rjsx-mode
          json-mode
          mhtml-mode
          yaml-mode) . lsp)
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-snippet nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-indentation nil)
  ;; (lsp-eldoc-enable-hover nil)
  (lsp-signature-render-documentation nil)
  :config
  (setenv "TSSERVER_LOG_FILE" (no-littering-expand-var-file-name "lsp/tsserver.log")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-hover nil)
  ;; (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package sml-mode)

(use-package rjsx-mode
  ;; react jsx formatting
  :mode "\\/.*\\.js\\'"
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package undo-tree
  ;; make undo a tree rather than line
  :config (global-undo-tree-mode))

(use-package gruvbox-theme
  ;; Groovy
  :config
  (load-theme 'gruvbox-dark-hard t))

;; (use-package vterm
;;   ;; better terminal
;;   :general
;;   (vterm-mode-map
;;    "<f11>" 'toggle-frame-fullscreen))

(use-package yaml-mode
  ;; formatting for yml files
  :mode "\\.yml\\'")

(use-package which-key
  ;; shows list of available completions when key sequences begin
  :custom
  (which-key-idle-delay 1) ;; Time before which-key pops up
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(use-package general
  ;; key binding manager
  :preface
  (defun my/switch-to-last-buffer ()
    "Flip between two buffers."
    (interactive)
    (switch-to-buffer nil))
  :general
  ("C-x b" 'consult-buffer
   "C-." 'consult-line
   "C-," 'comment-or-uncomment-region
   "C-c C-g" 'magit
   "C-c i" 'consult-imenu
   "C-c t" 'tab-bar-switch-to-tab
   "C-x r q" 'save-buffers-kill-terminal
   "C-c f" 'consult-flycheck
   '[f1] 'projectile-find-file
   '[f2] 'project-find-regexp
   '[f3] 'projectile-switch-project
   '[f5] 'call-last-kbd-macro
   '[f6] 'consult-project-imenu))

;; (:states '(normal visual insert emacs)
;;          :prefix "SPC"
;;          :non-normal-prefix "C-SPC"
;;          "" '(nil :which-key "Commands")
;;          "a" 'embark-act
;;          "c" 'evilnc-comment-or-uncomment-lines
;;          "r" #'my/switch-to-last-buffer
;;          "w" 'save-buffer
;;          "W" 'save-some-buffers
;;          "q" 'kill-buffer-and-window
;;          "SPC" 'ace-window
;;          "f" 'find-file
;;          "g" 'magit-status
;;          "G" 'magit-blame-mode
;;          "k" 'kill-this-buffer
;;          "K" 'kill-buffer
;;          "t" 'tab-bar-switch-to-tab
;;          "T" 'vterm-other-window
;;          "u" 'undo-tree-visualize
;;          "b" 'consult-buffer
;;          "i" 'consult-imenu
;;          "o" 'consult-outline
;;          "x" 'execute-extended-command
;;          "0" 'delete-window
;;          "1" 'delete-other-windows
;;          "2" 'split-window-below
;;          "3" 'split-window-right
;;          "h" '(:ignore t :which-key "Help Functions")
;;          "h a" 'consult-apropos
;;          "h h" 'help-for-help
;;          "h k" 'describe-key
;;          "h v" 'describe-variable
;;          "h b" 'describe-bindings
;;          "h m" 'describe-mode
;;          "h f" 'describe-function
;;          "h w" 'which-key-show-major-mode
;;          "l" '(:ignore t :which-key "LSP Mappings")
;;          "l d" 'lsp-find-definition
;;          "l r" 'lsp-find-references
;;          "l n" 'lsp-rename
;;          "l i" 'lsp-ui-imenu
;;          "l e" 'consult-flycheck
;;          "m" '(:ignore t :which-key "Extra Motions")
;;          "m a" 'beginning-of-defun
;;          "m e" 'end-of-defun)
;; (:states '(normal visual)
;;          "p" 'consult-yank-pop
;;          "/" 'consult-line

;;;; Built-in Package Config

(use-package xref
  ;; find identifier in prog modes
  :straight nil
  :general
  (xref--xref-buffer-mode-map
   :states 'motion
   "TAB" 'xref-quit-and-goto-xref)
  :custom
  (xref-search-program 'ripgrep))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep) ;project-find-regexp
  (when (functionp 'xref--show-defs-minibuffer)
    (setq xref-show-definitions-function 'xref--show-defs-minibuffer)
    (setq xref-show-xrefs-function 'xref--show-defs-minibuffer)))

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
  :custom
  (desktop-restore-eager 5)
  :config
  (desktop-save-mode 1))

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
  :config
  (delete-selection-mode)
  (fset 'yes-or-no-p 'y-or-n-p)   ; use y or n to confirm
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode 1)             ; Show matching parens
  (set-face-attribute 'completions-annotations nil
        	      :inherit '(italic magit-sequence-drop))
  (set-face-attribute 'default (selected-frame) :font "Hack" :height 140)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default (selected-frame) :font "Hack" :height 180)
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)
    (setq auto-save-default nil))
  (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
  (global-hl-line-mode 1)
  (set-face-background 'cursor "red"))

(provide 'init)
;;; init.el ends here
