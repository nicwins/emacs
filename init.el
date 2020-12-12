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
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  (general-def 'outshine-mode-map
    :prefix "f"
    :states '(normal)
    "j" 'outline-next-heading
    "k" 'outline-previous-heading)
  (general-def 'outshine-mode-map
    :states '(normal)
    "<tab>" 'outshine-cycle
    "<backtab>" 'outshine-cycle-buffer))

(use-package rainbow-delimiters
  ;; Change color of each inner block delimiter
  :delight
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package aggressive-indent
  ;; Indent as you type
  :delight
  :hook (prog-mode . aggressive-indent-mode))

(use-package company
  ;; text completion framework
  :delight
  :config (global-company-mode t))

;; Setup Dired
(use-package dired)

(use-package key-chord
  ;; Key-chord, to map jk to evil escape
  :config (key-chord-mode 1))

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
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
  :hook
  (evil-normal-state-entry . (lambda () (setq display-line-numbers t)))
  (evil-normal-state-exit . (lambda () (setq display-line-numbers 'relative))))

(use-package flycheck
  ;; code linter
  :config (global-flycheck-mode))

(use-package projectile
  ;; project traversal
  :init
  (setq-default projectile-mode-line-function '(lambda () (format "[%s]" (projectile-project-name)))
                projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld"
                projectile-cache-file "~/.emacs.d/backups/projectile.cache"
                projectile-switch-project-action 'helm-projectile
                projectile-remember-window-configs t)
  :config (projectile-mode))

(use-package helm
  ;; completion and selection framework
  :commands helm-autoresize-mode
  :delight
  :init
  (setq-default helm-idle-delay 0.1
                helm-input-idle-delay 0.1
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-locate-fuzzy-match t
                helm-M-x-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-split-window-in-side-p t
                helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area"
                                                 "\\*Minibuf" "\\*Compile-Log\\*" "\\*magit"
                                                 "\\*Customize Group"))
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1))

;; Helm-ag
(use-package helm-ag)

(use-package helm-projectile
  ;; integrates helm and projectile
  :config (helm-projectile-on))

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
	which-key-sort-order 'which-key-key-order-alpha))

(use-package general
  ;; key binding manager
  :config
  (defun save-all () "Save all open buffers." (interactive) (save-some-buffers t))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "w" 'save-buffer
   "W" 'save-all
   "q" 'kill-buffer-and-window
   "h" 'dired-jump
   "v" 'split-window-right
   "e" 'pp-eval-last-sexp
   "SPC" 'other-window
   "b" 'ibuffer
   "x" 'helm-M-x
   "g" 'magit-status
   "G" 'magit-blame-mode
   "k" 'kill-this-buffer
   "K" 'kill-buffer
   "o" 'helm-occur
   "T" 'eshell)
  (general-def
    "C-x r q" 'save-buffers-kill-terminal
    '[f1] 'helm-projectile
    '[f2] 'projectile-ag
    '[f5] 'call-last-kbd-macro))

(use-package delight
  ;; customize mode modeline display
  :config
  (delight '((eldoc-mode nil eldoc)
	     (emacs-lisp-mode "Elisp " :major)
	     (outline-minor-mode nil outline)
	     (subword-mode nil subword))))
  

;;;; Global Helper Functions

(require 'comint)

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
