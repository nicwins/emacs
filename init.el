;;; init --- Initial setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; General Config

;; Set path to .emacs.d
(defvar dotfiles-dir)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(defvar site-lisp-dir)
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(setq make-backup-files nil)

(save-place-mode 1)
(setq-default save-place-file (expand-file-name ".places" (concat dotfiles-dir "backups")))
(setq standard-indent 2)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Emacs Autosave Settings
(setq auto-save-list-file-prefix "~/.emacs.d/backups/.saves/")
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/.saves/" t)))

;; Emacs Backup Settings
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 `((".*" . "~/.emacs.d/backups/.saves/")) ; don't litter fs

 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

;; Tramp litter
(setq-default tramp-backup-directory-alist backup-directory-alist
              tramp-auto-save-directory "~/.emacs.d/backups/.saves/"
              tramp-default-method "ssh")

;; Eshell litter
(setq-default eshell-directory-name "~/.emacs.d/backups/eshell/")

;; Emacs Interlock
(setq create-lockfiles nil)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

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

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Save a list of recent files visited. (open recent file with C-x f)
(setq-default recentf-exclude '("\.recentf")
              recentf-save-file (expand-file-name
                                 "~/.emacs.d/backups/.recentf" user-emacs-directory)
              recentf-max-saved-items 50 ;; just 20 is too recent
              recentf-auto-cleanup 300
              recentf-auto-save-timer (run-with-idle-timer 300 t 'recentf-save-list))
(recentf-mode 1)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)


;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Seed the random-number generator
(random t)

;;;; Appearance

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)
(setq-default visible-bell t
	      font-lock-maximum-decoration t
	      color-theme-is-global t
	      truncate-partial-width-windows nil)

;; Never insert tabs
(setq tab-width 2)

;; force line word wrapping in text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Highlight current line
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq-default redisplay-dont-pause t)

;; org-mode colors
(setq-default org-todo-keyword-faces
              '(
                ("INPR" . (:foreground "yellow" :weight bold))
                ("DONE" . (:foreground "green" :weight bold))
                ("IMPEDED" . (:foreground "red" :weight bold))
                ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; add fill column
(global-display-fill-column-indicator-mode)
(set-face-attribute 'fill-column-indicator nil :foreground "grey27")
(setq-default display-fill-column-indicator-column 99)

;; Show lambda plase
(global-prettify-symbols-mode 1)

;;;; Initialize Package

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(setq-default use-package-verbose nil ; Don't report loading details
              use-package-enable-imenu-support t ; Let imenu find use-package defs
              use-package-expand-minimally t) ; minimize expanded code

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'delight)
(require 'general)

;;;; Package Configuration

(use-package outshine
  ;; Easier navigation for source files, especially this one
  :delight
  :ghook 'emacs-lisp-mode-hook
  :gfhook '(lambda()
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
(use
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
     "C-f" 'company-filter-candidates
     "<tab>" 'company-complete-selection))

;; Setup Dired
(use-package dired)

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

(use-package projectile
  ;; project traversal
  :init
  (setq-default projectile-mode-line-function '(lambda () (format "[%s]" (projectile-project-name)))
                projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld"
                projectile-cache-file "~/.emacs.d/backups/projectile.cache"
                projectile-switch-project-action 'project-switch-project
                projectile-remember-window-configs t)
  :config (projectile-mode))

(use-package ivy
  :delight
  :init
  (setq-default ivy-use-virtual-buffers t
		enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (ivy-configure 'counsel-imenu
    :update-fn 'auto))

(use-package counsel
  :delight
  :config (counsel-mode))

(use-package swiper)

;; remember recent commands in m-x
(use-package smex)

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
  :init (setq-default prettier-js-args
                      '("--bracket-spacing" "true"
                        "--single-quote" "true"
                        "--no-semi" "true"
                        "--jsx-single-quote" "true"
                        "--jsx-bracket-same-line" "true"))
  :config (global-prettier-mode))

(use-package rjsx-mode
  ;; react jsx formatting
  :init (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package ruby-mode
  ;; ruby editor
  :init (setq ruby-deep-indent-paren nil))

;; javascript lsp
(use-package tide)

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
	which-key-sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right))

(use-package general
  ;; key binding manager
  :general
  (:states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "r" #'er-switch-to-previous-buffer
   "w" 'save-buffer
   "W" 'save-all
   "q" 'kill-buffer-and-window
   "d" 'swiper
   "v" 'split-window-right
   "e" 'pp-eval-last-sexp
   "SPC" 'other-window
   "b" 'ibuffer
   "x" 'counsel-M-x
   "g" 'magit-status
   "G" 'magit-blame-mode
   "k" 'kill-this-buffer
   "K" 'kill-buffer
   "T" 'eshell
   "u" 'undo-tree-visualize
   "i" 'counsel-imenu
   "x" 'counsel-M-x
   "f" 'counsel-find-file
   "0" 'delete-window
   "h" '(:ignore t :which-key "Help Functions")
   "h a" '(counsel-apropos :which-key "Apropos")
   "h b" '(counsel-descbinds :which-key "Describe Bindings")
   "h f" '(counsel-describe-function :which-key "Describe Function")
   "h h" '(help-for-help :which-key "Help for Help")
   "h m" '(describe-mode :which-key "Describe Mode")
   "h v" '(counsel-describe-variable :which-key "Describe Variable"))

  (:states '(insert replace)
   "j" (general-key-dispatch 'self-insert-command
	 :timeout 0.25
	 "k" 'evil-normal-state))
  
  ("C-x r q" 'save-buffers-kill-terminal
   '[f1] 'projectile-find-file
   '[f2] 'projectile-ag
   '[f5] 'call-last-kbd-macro
   '[f6] 'ivy-resume))

(use-package delight
  ;; customize mode modeline display
  :config
  (delight '((eldoc-mode nil eldoc)
	     (emacs-lisp-mode "Elisp " :major)
	     (outline-minor-mode nil outline)
	     (subword-mode nil subword))))


;;;; Global Helper Functions

(require 'comint)

;; Helper to interactively save all
(defun save-all () "Save all open buffers." (interactive) (save-some-buffers t))

;; Helper to quickly switch to prior buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; "after" macro definition
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun duplicate-line()
  "Duplicates the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

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

(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

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

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)

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

;;;; Built-in Package Config

;; Easily navigate sillycased words
(global-subword-mode 1)


;; Eldoc Mapping
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Shell-mode
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(setq-default comint-buffer-maximum-size 2000)
(setq-default multi-term-program-switches "--login")
(put 'erase-buffer 'disabled nil)

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(toggle-frame-fullscreen)

(provide 'init)
;;; init.el ends here
