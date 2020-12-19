;;; init --- Initial setup -*- lexical-binding: t -*-

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
(eval-when-compile (require 'use-package))

;;;; Global Helper Functions

(defun my/grunt-server()
  "Start the grunt server."
  (interactive)
  (shell "**GULP**")
  (comint-send-string "**GULP**" "cd ~/projects/evms-dashboard/public/client; gulp")
  (comint-send-input))

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

(defun my/ruby-rubocop-header ()
  "Add headers demanded by rubocop to head of file."
  (interactive)
  (save-excursion
		(goto-char (point-min))
		(insert "# frozen_string_literal: true\n\n# :nodoc:\n")))

;;;; Package Configuration

(use-package gcmh
	;; Minimizes GC interference with user activity.
	:config (gcmh-mode 1))

(use-package no-littering
  ;; cleanup all the clutter from varios modes
  ;; places configs in /etc and data in /var
  :init (setq-default auto-save-file-name-transforms
											`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
											custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Automatically bisects init file
(use-package bug-hunter)

(use-package evil
  ;; Imports vim motion/states into emacs
  :init
  (setq-default evil-emacs-state-cursor '("yellow" box)
								evil-normal-state-cursor '("green" box)
								evil-visual-state-cursor '("orange" box)
								evil-insert-state-cursor '("red" bar)
								evil-replace-state-cursor '("red" bar)
								evil-operator-state-cursor '("red" hollow)
								evil-move-cursor-back nil
								evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
	:after evil
	:config
	(push '"TAB" evil-collection-key-blacklist)
	(push '"<tab>" evil-collection-key-blacklist)
	(evil-collection-init)
	(setq-default evil-collection-setup-minibuffer t)
	(general-def
		:states '(normal visual)
		[escape] 'keyboard-quit)
	(general-def
		:keymaps
		'(minibuffer-local-map
			minibuffer-local-ns-map
			minibuffer-local-completion-map
			minibuffer-local-must-match-map
			minibuffer-local-isearch-map)
		[escape] 'minibuffer-keyboard-quit))

;; Install a newer version of Org after removing the old
(use-package org)

(use-package outshine
  ;; Easier navigation for source files, especially this one
	:preface
	(defun my/init-file-checker ()
		"Collapses outline when entering init file."
		(when (string= user-init-file buffer-file-name)
			(outline-hide-body)))
  :ghook 'emacs-lisp-mode-hook
  ;;:gfhook 'my/init-file-checker
  :general
  (outshine-mode-map
   :states '(normal)
   "<tab>" 'outshine-cycle
   "<backtab>" 'outshine-cycle-buffer))

(use-package rainbow-delimiters
  ;; Change color of each inner block delimiter
  :ghook 'prog-mode-hook)

(use-package aggressive-indent
  ;; Indent as you type
  :ghook 'prog-mode-hook)

(use-package company
  ;; text completion framework
  :init
  (setq-default completion-styles '(flex))
  :config
  (global-company-mode t)
	:general
  (company-active-map
	 :states 'insert
	 "<return>" nil
	 "RET" nil
	 "<backtab>" 'company-select-next-or-abort
	 "C-<backtab>" 'company-select-previous-or-abort
	 "<tab>" 'company-complete-selection))

(use-package exec-path-from-shell
  ;; Load path from user shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rg)

(use-package flycheck
  ;; code linter
	:preface
	(defun my/flycheck-error-selector ()
		(select-window (get-buffer-window "*Flycheck errors*")))
	:init
	(setq-default flycheck-emacs-lisp-load-path 'inherit)
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

(use-package doom-modeline
	:config
	(doom-modeline-mode 1)
	(setq-default doom-modeline-height 18
								doom-modeline-buffer-encoding nil
								doom-modeline-buffer-file-name-style 'relative-to-project
								doom-modeline-icon (display-graphic-p)))

(use-package selectrum
	;; selection/completion manager
	:config (selectrum-mode +1))

(use-package prescient
  ;; sorting/filtering manager
  :config
	(prescient-persist-mode +1)
	(setq-default prescient-filter-method '(literal regexp fuzzy)))

(use-package selectrum-prescient
  ;; make selectrum use prescient filtering
  :config (selectrum-prescient-mode +1))

(use-package company-prescient
  ;; make company use prescient filtering
  :config (company-prescient-mode +1))

(use-package consult
  ;; enhances navigation with selectrum completions
  :init (fset 'multi-occur #'consult-multi-occur)
  :config (consult-preview-mode))

(use-package consult-selectrum)

(use-package consult-flycheck)

(use-package projectile
  ;; project traversal
	:preface
	(defun my/projectile-ignore-project (project-root)
		(f-descendant-of? project-root (expand-file-name "~/.emacs.d/straight/")))
  :config
	(projectile-mode +1)
  (setq projectile-ignored-project-function #'my/projectile-ignore-project))

(use-package marginalia
  ;; adds annotations to consult
  :straight (:branch "main")
  :init
  (marginalia-mode)
  (setq-default marginalia-annotators
								'(marginalia-annotators-heavy marginalia-annotators-light)))

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
	(apheleia-global-mode +1)
  (add-to-list 'apheleia-mode-alist '(ruby-mode . prettier)))

(use-package json-mode)

(use-package lsp-mode
  :commands lsp
  :hook ((rjsx-mode
          ruby-mode
          json-mode
          mhtml-mode
          sql-mode
          yaml-mode) . lsp)
	:config
	(setq-default lsp-enable-symbol-highlighting t
								lsp-enable-snippet nil
								lsp-modeline-diagnostics-enable nil
								))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq-default lsp-ui-sideline-show-hover nil
								lsp-ui-doc-enable nil
								lsp-ui-sideline-show-code-actions nil
								lsp-ui-sideline-show-symbol nil
								lsp-headerline-breadcrumb-enable nil))

;; (use-package dap-mode
;; 	:preface
;; 	(defun my/rails-debug ()
;; 		(interactive)
;; 		(dap-debug
;; 		 (list :name "Run"
;; 					 :type "Ruby"
;; 					 :cwd #'projectile-project-root
;; 					 :request "launch"
;; 					 :program (format "%s/bin/rails" #'projectile-project-root)
;; 					 :args "server")))
;; 	:init
;; 	(setq-default dap-auto-configure +1)
;; 	:config
;; 	(dap-mode +1)
;; 	(require 'dap-ruby))

(use-package rjsx-mode
  ;; react jsx formatting
  :init (add-to-list 'auto-mode-alist '("\\/.*\\.js\\'" . rjsx-mode)))

(use-package inf-ruby
  ;; provides a ruby repl
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package undo-tree
  ;; make undo a tree rather than line
  :config (global-undo-tree-mode))


(use-package gruvbox-theme
  ;; coding theme
  :config (load-theme 'gruvbox-dark-hard t))

(use-package vterm
  ;; better terminal
  :general
  (vterm-mode-map
   "<f11>" 'toggle-frame-fullscreen))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package which-key
	;; shows list of available completions when key sequences begin
	:commands (which-key-mode)
	:init (which-key-mode)
	:config
	(setq-default
	 which-key-idle-delay 0.5 ;; Time before which-key pops up
	 which-key-allow-evil-operators t ;; Show evil keybindings
	 which-key-show-operator-state-maps t
	 which-key-sort-order 'which-key-key-order-alpha)
	(which-key-setup-side-window-right))

(use-package general
	;; key binding manager
	:preface
	(defun my/save-all () "Save all open buffers." (interactive) (save-some-buffers t))

	(defun my/switch-to-last-buffer ()
		(interactive)
		(switch-to-buffer nil))

	:general
	(:states '(normal visual insert emacs)
					 :prefix "SPC"
					 :non-normal-prefix "C-SPC"
					 "c" 'comment-or-uncomment-region
					 "r" #'my/switch-to-last-buffer
					 "w" 'save-buffer
					 "W" #'my/save-all
					 "q" 'kill-buffer-and-window
					 "SPC" 'other-window
					 "f" 'find-file
					 "g" 'magit-status
					 "G" 'magit-blame-mode
					 "k" 'kill-this-buffer
					 "K" 'kill-buffer
					 "t" 'tab-bar-switch-to-tab
					 "T" 'vterm-other-window
					 "u" 'undo-tree-visualize
					 "b" 'consult-buffer
					 "e" 'consult-flycheck
					 "i" 'consult-imenu
					 "o" 'consult-outline
					 "x" 'execute-extended-command
					 "0" 'delete-window
					 "1" 'delete-other-windows
					 "2" 'split-window-below
					 "3" 'split-window-right
					 "h" '(:ignore t :which-key "Help Functions")
					 "h a" 'consult-apropos
					 "h h" 'help-for-help
					 "h k" 'describe-key
					 "h v" 'describe-variable
					 "h b" 'describe-bindings
					 "h m" 'describe-mode
					 "l" '(:ignore t :which-key "LSP Mappings")
					 "l d" 'lsp-find-definition
					 "l r" 'lsp-find-references
					 "l n" 'lsp-rename
					 "l i" 'lsp-ui-imenu
					 "l e" 'flycheck-list-errors)

	(:states '(normal)
					 "p" 'consult-yank-pop
					 "y" 'consult-yank
					 "/" 'consult-line)
	
	(:states '(insert replace)
					 "j" (general-key-dispatch 'self-insert-command
								 :timeout 0.25
								 "k" 'evil-normal-state))

	("C-x r q" 'save-buffers-kill-terminal
	 '[f1] 'project-find-file
	 '[f2] 'rg-project
	 '[f3] 'projectile-switch-project
	 '[f4] 'projectile-run-vterm
	 '[f5] 'call-last-kbd-macro))

;;;; General Settings

;; Seed the random-number generator
(random t)

;; Alias ielm as repl
(defalias 'repl 'ielm)

;;;; Built-in Package Config

(use-package xref
	;; find identifier in prog modes
	:straight nil
	:general
	(xref--xref-buffer-mode-map
	 :states 'motion
	 "TAB" 'xref-quit-and-goto-xref))

(use-package elec-pair
	;; automatically match pairs
  :straight nil
  :config
  (electric-pair-mode 1))

(use-package tab-bar
	;; save workspaces as groups of windows
	:straight nil
	:preface
	(defun my/no-tab-bar-lines (&rest _)
		"Hide the `tab-bar' ui."
		(dolist (frame (frame-list)) (set-frame-parameter frame 'tab-bar-lines 0)))
	:init
	(setq-default tab-bar-new-tab-choice "*scratch*")
	(advice-add #'tab-bar-mode :after #'my/no-tab-bar-lines)
	(advice-add #'make-frame :after #'my/no-tab-bar-lines)
	:config
	(tab-bar-mode 1))

(use-package desktop
	;; Save buffers and windows on exit
	:straight nil
	:init
	(setq-default desktop-restore-eager 10)
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
	;; originial modeline set to vc-parent-buffer-name
	:straight nil
	:init
	(setq-default vc-make-backup-files t))

(use-package uniquify
  ;; Add parts of each file's directory to the buffer name if not unique
  :straight nil
  :init
  (setq-default uniquify-buffer-name-style 'forward))

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

(use-package files
	;; General file handling
	:straight nil
	:init
	(setq-default backup-by-copying t
								delete-old-versions t
								kept-new-versions 6
								kept-old-versions 2
								version-control t
								vc-follow-symlinks t
								create-lockfiles nil
								delete-by-moving-to-trash t))

(use-package sql
	:straight nil
	:init
	(setq-default sql-product 'postgres))

;;;; Appearance

;; No splash screen
(setq-default inhibit-startup-message t)

;; Be quiet
(setq-default visible-bell t)

;; Don't scroll horizontally
(setq-default auto-hscroll-mode nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Two spaces for tab
(setq-default standard-indent 2
							tab-width 2
							indent-tabs-mode nil)

;; force line word wrapping in text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Highlight current line
(set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq-default redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; add fill column
(global-display-fill-column-indicator-mode)
(set-face-attribute 'fill-column-indicator nil :foreground "grey27")
(setq-default display-fill-column-indicator-column 99)

(set-face-attribute 'completions-annotations nil
										:inherit '(italic magit-sequence-drop))

(set-face-attribute 'default (selected-frame) :font "Hack" :height 130)

;; Full Screen at the end
(toggle-frame-fullscreen)

(provide 'init)
;;; init.el ends here
