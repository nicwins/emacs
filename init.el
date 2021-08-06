;; init --- Initial setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Pre-Package Initialization

;; Set all packages to compile async
;; (setq-default comp-deferred-compilation t)

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
(defvar straight-fix-flycheck)
(setq straight-fix-flycheck t)

;; Bootstrap `use-package'
(setq-default use-package-verbose nil            ; Don't report loading details
              use-package-enable-imenu-support t ; Let imenu find use-package defs
              use-package-expand-minimally t) ; minimize expanded code

(straight-use-package 'use-package)

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
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun my/switch-to-last-buffer ()
  "Flip between two buffers."
  (interactive)
  (switch-to-buffer nil))

(defun my/newline-below ()
  "Insert newline below and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my/newline-above ()
  "Insert newline above and indent."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline-and-indent))

(defun my/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)
    (back-to-indentation)))

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

(defun my/three-column-layout ()
  "Set the frame to three columns."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defvar my/re-builder-positions nil
  "Store point and region bounds before calling `re-builder'.")
(advice-add 're-builder
            :before
            (defun my/re-builder-save-state (&rest _)
              "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
              (setq my/re-builder-positions
                    (cons (point)
                          (when (region-active-p)
                            (list (region-beginning)
                                  (region-end)))))))
(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of `re-builder'.
With non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
                               (if current-prefix-arg
                                   (if (eq current-prefix-arg '-) " backward" " word")
                                 "")
                               " regexp"
                               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end))))

;;;; Package Configuration
(use-package use-package-ensure-system-package
  ;; ensure global binaries are installed
  )

(use-package exec-path-from-shell
  ;; Load path from user shell
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

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

(use-package expand-region
  ;; Expand the region by step
  :bind
  (("C-=" . er/expand-region)
   ("C-\\" . er/contract-region)))

(use-package visible-mark
  ;; Makes the mark visible
  :custom
  (visible-mark-max 1)
  (visible-mark-faces `(visible-mark-face1 visible-mark-face2))
  :config
  (global-visible-mark-mode 1))

(use-package aggressive-indent
  ;; Indent as you type
  :hook (prog-mode . aggressive-indent-mode))

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

(use-package vertico
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("?" . minibuffer-completion-help)))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (savehist-mode))

(use-package consult
  ;; enhanced selection ui
  :after (projectile)
  :commands (projectile-project-root)
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<"))


(use-package consult-flycheck
  ;; add a consult-flycheck command
  :after (consult flycheck))

(use-package marginalia
  ;; adds annotations to consult
  :init
  (marginalia-mode))

(use-package embark
  ;; provide actions on competion candidates, or text at point
  :bind
  ("M-." . embark-dwim)
  ("C-." . embark-act)
  :custom
  (embark-prompt-style 'completion)
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package projectile
  ;; project traversal
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :preface
  (defun my/projectile-ignore-project (project-root)
    (f-descendant-of? project-root (expand-file-name "~/.config/emacs/straight/")))
  :config
  (projectile-mode 1))

;; needed for projectile-ripgrep
(use-package ripgrep)

(use-package rg
  ;; ripgrep for consult
  ;;:ensure-system-package
  ;;(rg . ripgrep)
  )

(use-package magit
  ;; emacs interface for git
  :hook
  (magit-credential . magit-process-buffer)
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
      (delete-other-windows)))

  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
                                   branch
                                   (or (magit-get-upstream-branch branch)
                                       (magit-get "branch" branch "remote"))))
        (user-error "Push to upstream aborted by user")))))

(use-package apheleia
  :straight
  (apheleia :type git
	    :host github
	    :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              ;; "--single-quote" "true"
              file))
  (apheleia-global-mode +1))

(use-package json-mode)

(use-package rjsx-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js-mode
          rjsx-mode
          json-mode
          mhtml-mode
          yaml-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-indentation nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-prefer-capf t)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-enable-text-document-color nil)
  (lsp-completion-enable nil)
  (lsp-completion-show-kind nil)
  (lsp-file-watch-threshold 2000)
  (lsp-headerline-breadcrumb-enable nil)
  
  ;; Need to toggle this to get eslint alongside
  ;; (lsp-disabled-clients '(ts-ls))
  (lsp-disabled-clients nil)
  ;; Config specific to tsserver/theia ide
  (lsp-clients-typescript-log-verbosity "off")
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setenv "TSSERVER_LOG_FILE" (no-littering-expand-var-file-name "lsp/tsserver.log")))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-update-mode "line")
  (lsp-ui-peek-enable nil)
  (lsp-ui-doc-enable nil))

(use-package consult-lsp
  :after (consult lsp)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . #'consult-lsp-symbols)))

(use-package sml-mode)

(use-package paredit
  :hook
  ((emacs-lisp-mode
    eval-expression-minibuffer-setup
    lisp-mode
    lisp-interaction-mode
    scheme-mode) . enable-paredit-mode))

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

(use-package org
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/index.org")
  (org-agenda-files (list "~/notes/index.org"))
  (org-hide-emphasis-markers t)
  :config
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package visual-fill-column
  ;; Line wrap at the fill column, not buffer end
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package yasnippet
  ;; template system for emacs
  :hook (prog-mode . yas-minor-mode))

(use-package perspective
  ;; window and buffer manager
  :hook
  (kill-emacs . persp-state-save)
  :bind
  (("C-c t" . persp-switch)
   ("C-x b" . persp-switch-to-buffer*)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-state-default-file (no-littering-expand-var-file-name "perspective/perspectives.el"))
  (persp-modestring-short t)
  :config
  (persp-mode))

(use-package disable-mouse
  ;; Globally disable the mouse
  :init
  (setq disable-mouse-wheel-events nil)
  :config
  (global-disable-mouse-mode))

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

(use-package dired
  ;; directory management
  :straight (:type built-in)
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  ;; Dired listing switches - see man ls
  (dired-listing-switches "-alhF --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-guess-shell-alist-user
   '(("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\|webm\\)\\(?:\\.part\\)?\\'"
      "mpv")
     ("\\.html?\\'" "firefox"))))

(use-package dired-x
  ;; extension for dired
  :straight (:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$")))

(use-package diredfl
  ;; dired font-lock
  :custom (diredfl-global-mode t))

(use-package server
  ;; The emacs server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

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

(use-package recentf
  ;; Recent file list
  :straight (:type built-in)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode 1))

(use-package re-builder
  ;; interactive regex builder
  :straight (:type built-in)
  :bind
  (("C-M-%" . re-builder)
   :map reb-mode-map
   ("RET" . reb-replace-regexp)
   :map reb-lisp-mode-map
   ("RET" . reb-replace-regexp))
  :custom
  (reb-re-syntax 'rx))

(use-package emacs
  ;; Stuff that doesn't seem to belong anywhere else
  :straight nil
  :init
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  :bind
  (;; Basic Overrides
   ("C-a" . my/smarter-move-beginning-of-line)
   ("C-," . my/comment-or-uncomment-region-or-line)
   ("M-s" . consult-line)
   ("C-o" . my/newline-below)
   ("C-S-o" . my/newline-above)
   ("M-y" . consult-yank-pop)
   ("<help> a" . consult-apropos)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g m" . consult-mark)
   ("C-<tab>" . push-mark-no-activate)
   ("M-<tab>" . jump-to-mark)
   ("M-'" . consult-register-store)
   ("M-#" . consult-register-load)
   ;; C-x bindings
   ("C-x C-b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x f" . consult-find)
   ("C-x r q" . save-buffers-kill-terminal)
   ;; C-c bindings (user-map)
   ("C-c c" . org-capture)
   ("C-c i" . consult-imenu)
   ("C-c I" . consult-project-imenu)
   ("C-c f" . consult-flycheck)
   ("C-c F" . consult-lsp-diagnostics)
   ("C-c s" . consult-lsp-symbols)
   ("C-c v" . magit)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c r" . my/switch-to-last-buffer)
   ([f1] . projectile-find-file)
   ([f2] . consult-ripgrep)
   ([f3] . projectile-switch-project)
   ([f5] . kmacro-call-macro)
   :map isearch-mode-map
   ("M-e" . consult-isearch)
   ("M-s l" . consult-line))
  :hook
  ((text-mode . visual-line-mode)
   (minibuffer-setup . cursor-intangible-mode))
  :custom
  (inhibit-startup-message t)           ; no splash screen
  (visible-bell t)                      ; be quiet
  (indicate-empty-lines t)              ; show lines at the end of buffer
  (sentence-end-double-space nil)       ; single space after a sentence
  (indent-tabs-mode nil)                ; use spaces instead of tabs
  (cursor-type '(bar . 2))              ; no fat cursor
  (js-indent-level 2)                   ; js settings needed for rjsx
  (js-switch-indent-offset 2)           ; more js settings
  (fill-column 80)                      ; default fill column
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ; no cursor in minibuffer
  (enable-recursive-minibuffer t)
  :config
  (add-to-list 'completion-ignored-extensions ".DS_STORE")
  (delete-selection-mode)
  (fset 'yes-or-no-p 'y-or-n-p)         ; use y or n to confirm
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode 1)                   ; Show matching parens
  (set-frame-font "Hack-14")
  (when (eq system-type 'darwin)
    (set-face-attribute 'default (selected-frame) :font "Hack" :height 180)
    (setq visible-bell nil)
    (setq ring-bell-function 'ignore)
    (setq auto-save-default nil))
  (global-hl-line-mode 1)
  (set-face-background 'cursor "red")
  (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
  (windmove-default-keybindings)
  (persp-state-load (no-littering-expand-var-file-name "perspective/perspectives.el")))

(provide 'init)
;;; init.el ends here
