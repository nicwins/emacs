;;; init --- Initial setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Pre-Package Initialization

;; Remove built-in version of Org from the load-path
(require 'cl-seq)
(setq load-path
      (cl-remove-if
       (lambda (x)
	 (string-match-p "org$" x))
       load-path))

;;;; Initialize Package

;; Bootstrap `straight'
;; this also initializes package.el
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
(straight-use-package 'delight)
(straight-use-package 'general)
(require 'delight)
(require 'general)

;;;; Global Helper Functions

;; Helper to interactively save all
(defun save-all () "Save all open buffers." (interactive) (save-some-buffers t))

;; Helper to quickly switch to prior buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun tar-compress-dashboard ()
  "Tars up the dashboard project."
  (interactive)
  (shell "*tar*")
  (switch-to-buffer "*tar*")
  (comint-send-string "*tar*" "tar --exclude='/var/www/evms-dashboard/log' --exclude='/var/www/evms-dashboard/solr/data' --exclude='/var/www/evms-dashboard/tmp' --exclude='/var/www/evms-dashboard/.git'  --exclude='/var/www/evms-dashboard/client/bower_components' --exclude='/var/www/evms-dashboard/client/node_modules' -zcvf rails.tgz /var/www/evms-dashboard")
  (comint-send-input))

(defun grunt-server()
  "Start the grunt server."
  (interactive)
  (shell "**GULP**")
  (comint-send-string "**GULP**" "cd ~/projects/evms-dashboard/public/client; gulp")
  (comint-send-input))

(defun rails-server()
  "Start rails."
  (interactive)
  (shell "**RAILS**")
  (comint-send-string "**RAILS**" "cd ~/projects/evms-dashboard/; rails s")
  (comint-send-input))

(defun startup-evms-dashboard ()
  "Start rails server and grunt watcher."
  (interactive)
  (grunt-server)
  (rails-server))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(defun delete-file-and-buffer ()
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

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun Fuco1/lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

;;;; Package Configuration

(use-package gcmh
  ;; Minimizes GC interference with user activity.
  :delight
  :config (gcmh-mode 1))

(use-package no-littering
  ;; cleanup all the clutter from varios modes
  ;; places configs in /etc and data in /var
  :init (setq auto-save-file-name-transforms
	      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	      custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Automatically bisects init file
(use-package bug-hunter)

;; Install a newer version of Org after removing the old
(use-package org)

;; Built-in project defines
(use-package project)

(use-package projectile
  ;; project traversal
  :init
  (setq-default projectile-mode-line-function '(lambda () (format " [%s] " (projectile-project-name)))
                projectile-switch-project-action 'project-switch-project
                projectile-remember-window-configs t)
  :config (projectile-mode))

(use-package outshine
  ;; Easier navigation for source files, especially this one
  :delight
  :ghook 'emacs-lisp-mode-hook
  :gfhook '(lambda ()
	     (when (string= user-init-file buffer-file-name)
	       (outline-hide-body)))
  :general
  (outshine-mode-map
   :states '(normal)
   "<tab>" 'outshine-cycle
   "<backtab>" 'outshine-cycle-buffer))

(use-package rainbow-delimiters
  ;; Change color of each inner block delimiter
  :delight
  :ghook 'prog-mode-hook)

(use-package aggressive-indent
  ;; Indent as you type
  :delight
  :ghook 'prog-mode-hook)

(use-package company
  ;; text completion framework
  :delight
  :init
  (setq completion-styles '(flex))
  :config
  (global-company-mode t)
  (general-def company-active-map
    "<return>" nil
    "RET" nil
    "<backtab>" 'company-select-next-or-abort
    "C-<backtab>" 'company-select-previous-or-abort
    "<tab>" 'company-complete-selection))

(use-package dired
  ;; Setup Dired
  :straight nil)

(use-package exec-path-from-shell
  ;; Load path from user shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rg)

(use-package xref
  ;; search project for string
  :init
  (setq xref-search-program 'ripgrep))

(use-package evil
  ;; Imports vim motion/states into emacs
  :init
  (setq evil-emacs-state-cursor '("yellow" box)
        evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("red" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow)
        evil-move-cursor-back nil)
  :config
  (evil-mode 1)
  (general-add-hook 'evil-normal-state-entry-hook '(lambda () (setq display-line-numbers t)))
  (general-add-hook 'evil-normal-state-exit-hook '(lambda () (setq display-line-numbers 'relative))))

(use-package flycheck
  ;; code linter
  :config (global-flycheck-mode))

(use-package selectrum
  ;; selection/completion manager
  :config (selectrum-mode +1))

(use-package prescient
  ;; sorting/filtering manager
  :config (prescient-persist-mode +1))

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

(use-package consult-selectrum
  :straight nil)

(use-package consult-flycheck
  ;; adds the consult-flycheck command
  :straight nil)

(use-package marginalia
  ;; adds annotations to consult
  :straight (:branch "main")
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

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

(use-package prettier
  ;; prettify javascript on save
  :hook (rjsx-mode . prettier-mode))

(use-package json-mode)

(use-package lsp-mode
  :commands lsp
  :hook ((rjsx-mode
	  ruby-mode
	  json-mode
	  mhtml-mode) . lsp)
  :config
  (setq lsp-eldoc-hook nil
	lsp-enable-symbol-highlighting t
	lsp-enable-snippet nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-hover nil))

(require 'lsp-ui-flycheck)

(use-package rjsx-mode
  ;; react jsx formatting
  :init (add-to-list 'auto-mode-alist '("\\/.*\\.js\\'" . rjsx-mode)))

(use-package ruby-mode
  ;; ruby editor
  :init (setq ruby-deep-indent-paren nil))

(defun ruby-prettier ()
  "Run ruby prettier on save in `ruby-mode'."
  (interactive)
  (when (eq major-mode 'ruby-mode)
    (shell-command-to-string (format "yarn prettier --write %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'after-save-hook 'ruby-prettier)

(use-package undo-tree
  ;; make undo a tree rather than line
  :delight
  :config (global-undo-tree-mode))

(use-package gruvbox-theme
  ;; coding theme
  :config (load-theme 'gruvbox-dark-hard t))

(use-package smart-mode-line
  ;; pretties up the mode line
  :config (sml/setup))

(use-package which-key
  ;; shows list of available completions when key sequences begin
  :delight
  :commands (which-key-mode)
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2 ;; Time before which-key pops up
	which-key-allow-evil-operators t ;; Show evil keybindings
	which-key-show-operator-state-maps t
	which-key-sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right))

(use-package general
  ;; key binding manager
  :general
  (:states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "r" #'er-switch-to-previous-buffer
   "w" 'save-buffer
   "W" 'save-all
   "q" 'kill-buffer-and-window
   "v" 'split-window-right
   "SPC" 'other-window
   "f" 'find-file
   "g" 'magit-status
   "G" 'magit-blame-mode
   "k" 'kill-this-buffer
   "K" 'kill-buffer
   "T" 'eshell
   "u" 'undo-tree-visualize
   "b" 'consult-buffer
   "d" 'consult-line
   ;;"e" 'consult-flycheck
   "i" 'consult-imenu
   "o" 'consult-outline
   "x" 'execute-extended-command
   "0" 'delete-window
   "h" '(:ignore t :which-key "Help Functions")
   "h a" 'consult-apropos
   "h h" 'help-for-help
   "h k" 'describe-key
   "h v" 'describe-variable
   "l" '(:ignore t :which-key "LSP Mappings")
   "l d" 'xref-find-definitions
   "l r" 'xref-find-references
   "l n" 'lsp-rename)

  (:states '(normal)
   "p" 'consult-yank-pop)
  
  (:states '(insert replace)
   "j" (general-key-dispatch 'self-insert-command
	 :timeout 0.25
	 "k" 'evil-normal-state))
  
  ("C-x r q" 'save-buffers-kill-terminal
   '[f1] 'project-find-file
   '[f2] 'rg-project
   '[f5] 'call-last-kbd-macro))

(use-package delight
  ;; customize mode modeline display
  :config
  (delight '((eldoc-mode nil eldoc)
	     (emacs-lisp-mode "Elisp " :major)
	     (outline-minor-mode nil outline)
	     (subword-mode nil subword))))

;;;; General Settings

;; Emacs Interlock
(setq create-lockfiles nil)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about buffers with processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Seed the random-number generator
(random t)

;;;; Built-in Package Config

;; originial modeline set to vc-parent-buffer-name
(use-package vc
  ;; Make backups of files, even when they're in version control
  :straight nil
  :config
  (setq vc-make-backup-files t))

(use-package uniquify
  ;; Add parts of each file's directory to the buffer name if not unique
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package eldoc
  ;; Use Eldoc for elisp
  :straight nil
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))


;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Store cursor location between sessions
(save-place-mode 1)

;; Emacs Backup Settings - Auto-save config
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-follow-symlinks t)

;; Shell-mode
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(setq-default comint-buffer-maximum-size 2000)
(setq-default multi-term-program-switches "--login")
(put 'erase-buffer 'disabled nil)

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; Tramp
(setq-default tramp-default-method "ssh")

;;;; Appearance

;; No splash screen
(setq inhibit-startup-message t)
(setq-default visible-bell t)

;; 	      font-lock-maximum-decoration t
;; 	      color-theme-is-global t
;; 	      truncate-partial-width-windows nil)

;; Two spaces for tab
(setq-default standard-indent 2)
(setq tab-width 2)

;; force line word wrapping in text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Highlight current line
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq-default redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; add fill column
(global-display-fill-column-indicator-mode)
(set-face-attribute 'fill-column-indicator nil :foreground "grey27")
(setq-default display-fill-column-indicator-column 99)

;; Show lambda plase
(global-prettify-symbols-mode 1)

;; Full Screen at the end
(toggle-frame-fullscreen)

(provide 'init)
;;; init.el ends here
