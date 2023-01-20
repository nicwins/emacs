;; init --- Initial setup -*- lexical-binding: t -*-

;; Copyright (C) 2011-2022 Nicolas Winslow

;; Author: Nicolas Winslow

;;; Commentary:

;;; Code:

;;;; Pre-Package Initialization

;;;; Initialize Package

;; This is only needed once, near the top of the file
(defvar straight-fix-flycheck)
(setq straight-fix-flycheck t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-softwars/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t)

;; Bootstrap `use-package'
(setq-default use-package-verbose nil            ; Don't report loading details
              use-package-enable-imenu-support t ; Let imenu find use-package defs
              use-package-expand-minimally t)    ; minimize expanded code

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;;;; Package Configuration
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

(use-package bug-hunter) ;; Automatically bisects init file

(use-package aggressive-indent
  ;; Indent as you type
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-mode))

(use-package geiser-guile
  ;; major mode for guile with repl
  :if (eq system-type 'gnu/linux)
  :hook (scheme-mode . guix-devel-mode)
  :custom
  (geiser-mode-start-repl-p t)
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(use-package guix
  ;; front end for guix commands
  ;; requires guix install guix!
  :after (geiser-guile)
  :if (eq system-type 'gnu/linux))

(use-package doom-modeline
  ;; fancy modeline
  :custom
  (doom-modeline-height 18)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-hud t)
  :config
  (doom-modeline-mode 1))

(use-package vertico
  ;; displays lists using completing-read
  :init
  (vertico-mode)
  :load-path "~/.config/emacs/straight/repos/vertico/extensions/"
  :bind
  (:map vertico-map
        ("?" . minibuffer-completion-help)))

(use-package vertico-directory
  ;; easy bindings for navigating dirs
  :straight nil
  :after (vertico)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("^" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  ;; narrowing and filtering for selections
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (orderless-component-separator "*")
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (add-hook 'minibuffer-setup-hook (lambda () (setq-local orderless-component-separator " ")))
  (savehist-mode))

(use-package corfu
  ;; autocomplete package
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

(use-package rg) ; ripgrep for consult

(use-package consult
  ;; enhanced selection ui
  :bind
  (("M-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("<help> a" . consult-apropos)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g m" . consult-mark)
   ("M-'" . consult-register-store)
   ("M-#" . consult-register-load)
   ;; C-x bindings
   ("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ;; C-c bindings (user-map)
   ("C-c i" . consult-imenu)
   ("C-c I" . consult-project-imenu)
   ("C-c z" . consult-flycheck)
   ;;("C-c F" . consult-lsp-diagnostics)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ([f2] . consult-ripgrep)
   :map isearch-mode-map
   ("M-e" . consult-isearch)
   ("M-s l" . consult-line))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package consult-dir
  ;; browse recent dirs and bookmarks
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  ;; adds annotations to consult
  :init
  (marginalia-mode))

(use-package embark
  ;; provide actions on competion candidates, or text at point
  :bind
  (("M-." . embark-dwim)
   ("C-." . embark-act))
  :custom
  (embark-prompt-style 'completion)
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  ;; embark integration for consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package magit
  ;; emacs interface for git
  :preface
  (defun opt-out-of-consult-crm (&rest args)
    (if (advice-member-p #'consult-completing-read-multiple #'completing-read-multiple)
        (unwind-protect
            (progn
              (advice-remove #'completing-read-multiple #'consult-completing-read-multiple)
              (apply args))
          (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))
      (apply args)))

  (defun magit-status--around (orig-magit-status &rest args)
    "Set magit status to fullscreen."
    (window-configuration-to-register :my/magit-fullscreen)
    (apply orig-magit-status args)
    (delete-other-windows))

  (defun magit-log-buffer-file--before (orig-fun &rest args)
    "Store the window configuration before logging."
    (setq my-magit-log-buffer-file-registered t)
    (window-configuration-to-register :my/magit-fullscreen)
    (apply orig-fun args))

  (defun magit-mode-bury-buffer--around (orig-magit-mode-bury-buffer &rest args)
    "Restore previous window configuration if we are burying magit-status."
    (if (or
         (equal (symbol-name major-mode) "magit-status-mode")
         (and my-magit-log-buffer-file-registered
              (equal (symbol-name major-mode) "magit-log-mode")))
        (progn
          (setq my-magit-log-buffer-file-registered nil)
          (apply orig-magit-mode-bury-buffer args)
          (jump-to-register :my/magit-fullscreen))
      (apply orig-magit-mode-bury-buffer args)
      (delete-other-windows)))

  (defun magit-push-current-to-upstream--before (orig-fun &rest args)
    "Promt for confirmation before pushing to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
                                   branch
                                   (magit-get "branch" branch "remote")))
        (user-error "Pushed aborted"))))
  :bind
  (("C-c l" . magit-log-buffer-file)
   ("C-c v" . magit))
  :hook
  ((magit-credential . magit-process-buffer)
   (git-commit-post-finish-hook . magit-process-buffer))
  :config
  (setq my-magit-log-buffer-file-registered nil)
  (advice-add #'magit-completing-read-multiple* :around #'opt-out-of-consult-crm)
  (advice-add 'magit-status :around #'magit-status--around)
  (advice-add 'magit-log-buffer-file :around #'magit-log-buffer-file--before)
  (advice-add 'magit-mode-bury-buffer :around #'magit-mode-bury-buffer--around)
  (advice-add 'magit-push-current-to-upstream
              :before #'magit-push-current-to-upstream--before))

(use-package apheleia
  ;; run code formatters, saving point
  :config
  (apheleia-global-mode +1))

(use-package json-mode) ; major mode for json

(use-package eglot
  :preface
  (defun me/eglot-shutdown-project ()
    "Kill the LSP server for the current project if it exists."
    (when-let ((server (eglot-current-server)))
      (ignore-errors (eglot-shutdown server))))  
  :hook
  (typescript-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :custom
  (eglot-autoshutdown t)
  :config
  ;; TODO: add this to typescript mode specific hook
  (eglot--code-action eglot-code-action-organize-imports "source.organizeImports.ts")
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (advice-add 'project-kill-buffers :before #'me/eglot-shutdown-project))

(use-package puni
  ;; balanced editing mode
  :defer t
  :hook ((prog-mode
          sgml-mode
          nxml-mode
          tex-mode
          eval-expression-minibuffer-setup) . puni-mode))

(use-package undo-tree
  ;; make undo a tree rather than line
  :custom
  (undo-tree-auto-save-history nil)
  :config (global-undo-tree-mode))

(use-package gruvbox-theme
  ;; Groovy
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package yaml-mode
  ;; formatting for yml files
  :mode "\\.yml\\'")

(use-package org
  ;; notes and todos
  :preface
  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
  :bind
  (("C-c a" . org-agenda)
   ("C-c q" . org-capture-inbox))
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/inbox.org")
  (org-agenda-files (list "~/notes/"))
  (org-capture-templates
   `(("i" "Inbox" entry  (file "inbox.org")
      ,(concat "* TODO %?\n"
               "/Entered on/ %U"))))
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " ")
     (tags   . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-confirm-babel-evaluate nil) ; disable confirm exec
  :config
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-mode-hook 'visual-line-mode))

(use-package ob-http
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t))))

(use-package mixed-pitch
  ;; use both pitch types in one buffer
  :hook (text-mode . mixed-pitch-mode)
  :config
  (when (eq system-type 'darwin)
    (set-face-attribute 'default (selected-frame) :family "Iosevka" :height 200)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 200)
    (set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 200))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Hack" :height 150)
    (set-face-attribute 'fixed-pitch nil :family "Hack" :height 150)
    (set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :height 150)))

(use-package org-superstar
  ;; Nice bullets
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))

(use-package visual-fill-column
  ;; Line wrap at the fill column, not buffer end
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package yasnippet
  ;; template system for emacs
  :hook (prog-mode . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/src/guix/etc/snippets")
  (yas-reload-all))

(use-package password-store
  ;; front end for `pass'
  :if (eq system-type 'gnu/linux))

(use-package pass
  ;; use the unix `pass' store
  :if (eq system-type 'gnu/linux)
  :preface
  (defun pass-status--around (orig-pass &rest args)
    "Set pass status to fullscreen."
    (window-configuration-to-register :my/pass-fullscreen)
    (apply orig-pass args)
    (delete-other-windows))

  (defun pass-quit--around (orig-pass-quit &rest args)
    "Restore previous window configuration."
    (if (equal (symbol-name major-mode) "pass-mode")
        (progn
          (apply orig-pass-quit args)
          (jump-to-register :my/pass-fullscreen))
      (apply orig-pass-quit args)))
  :custom (pass-username-field "user")
  :config
  (advice-add 'pass :around #'pass-status--around)
  (advice-add 'pass-quit :around #'pass-quit--around))

(use-package diredfl
  ;; dired font-lock
  :custom (diredfl-global-mode t))

(use-package dired-git-info
  ;; show latest commit message in dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package dired-narrow
  ;; filter dired buffers
  :ensure t
  :bind ((:map dired-mode-map
               ("/" . dired-narrow))))

(use-package async
  ;; use dired functions async
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package tree-sitter
  ;; fast syntax highlighting
  :demand
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  ;; helper package for tree-sitter
  :after tree-sitter)

(use-package typescript-mode
  ;; major mode for ts/js
  ;;:mode (rx "." (or "j" "t") "s" (zero-or-one "x"))
  :demand
  :after tree-sitter
  :preface
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  :custom
  (typescript-indent-level 2)
  :config
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package tsi
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :after tree-sitter
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tree-sitter-mode 1)))
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package highlight-parentheses
  ;; highlight all parens surrounding point
  :hook (prog-mode .  highlight-parentheses-mode))

(use-package helpful
  ;; better formatting for help buffers
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :straight (:type built-in)
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-case-fold-search nil))

(use-package wgrep
  :custom
  (wdired-allow-to-change-permissions t))

(use-package which-key
  ;; Display keybindings in popup
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 1)
  :bind
  ("C-c ?" . which-key-show-top-level))

(use-package project
  :custom
  (vc-directory-exclusion-list (append vc-directory-exclusion-list '("node_modules" "build"))))

(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :custom
  (project-x-local-identifier '("package.json" ".project"))
  :config
  (project-x-mode 1)
  (remove-hook 'project-find-functions 'project-x-try-local)
  (add-hook 'project-find-functions 'project-x-try-local -1))

(use-package hl-todo
  ;; Highlight keywords such as TODO, FIXME, NOTE, etc.
  ;; NOTE: Face values defined in `hl-todo-keyword-faces'.
  :config
  (global-hl-todo-mode))

;;;; Built-in Package Config

(use-package isearch
  :straight (:type built-in)
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-buffer t)
  (lazy-highlight-initial-delay 0)
  :hook
  (isearch-update-post . my/isearch-aim-beginning)
  :preface
  (defun my/isearch-aim-beginning ()
    "Move cursor back to the beginning of the current match."
    (when (and isearch-forward (number-or-marker-p isearch-other-end))
      (goto-char isearch-other-end))))

(use-package help-mode
  :straight (:type built-in)
  :bind
  (:map help-mode-map
        ("<" . help-go-back)
        (">" . help-go-forward)))

(use-package xref
  ;; find identifier in prog modes
  :straight (:type built-in)
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
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t))

(use-package dired-x
  ;; extension for dired
  :straight (:type built-in)
  :preface
  (defun my/dired-open()
    (interactive)
    (cond ;; use dired-find-file if it is a directory
     ((file-directory-p (dired-get-file-for-visit)) (dired-find-file))
     ;; If there is no default defined, open in dired
     ((null (dired-guess-default (cons (dired-get-filename) '()))) (dired-find-file))
     ;; use xdg-open for everything else
     ;; start-process quote the arguments so you do not need the sell-quote-argument function
     (t (start-process "dired-open" nil "xdg-open" (dired-get-file-for-visit)))))
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("<return>" . my/dired-open))
  :custom
  (dired-omit-verbose nil)
  (dired-guess-shell-alist-user
   '(("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\|webm\\|mov\\)\\(?:\\.part\\)?\\'"
      "! (mpv ? &>/dev/null &)")))
  :config
  ;; setting this in custom throws a dired-omit-files is undefined
  (setq dired-omit-files (concat dired-omit-files "\\|^.DS_STORE$")))

(eval-after-load  'dired
  '(defun dired-clean-up-after-deletion (fn)
     "My clean up after a deleted file or directory FN.
  Remove expanded subdir of deleted dir, if any."
     (save-excursion (and (cdr dired-subdir-alist)
                          (dired-goto-subdir fn)
                          (dired-kill-subdir)))
     ;; Offer to kill buffer of deleted file FN.
     (if dired-clean-up-buffers-too
         (progn
           (let ((buf (get-file-buffer fn)))
             (and buf
                  (save-excursion ; you never know where kill-buffer leaves you
                    (kill-buffer buf))))
           (let ((buf-list (dired-buffers-for-dir (expand-file-name fn)))
                 (buf nil))
             (and buf-list
                  (while buf-list
                    (save-excursion (kill-buffer (car buf-list)))
                    (setq buf-list (cdr buf-list)))))))))

(use-package wdired
  ;; editable dired buffers
  :straight (:type built-in)
  :bind ((:map wdired-mode-map
               ("<return>" . my/dired-open)
               ("C-x C-s" . wdired-finish-edit))))

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

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

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
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (vc-follow-symlinks t)
  (create-lockfiles nil))

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
  :preface
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
  
  (defun my/reb-replace-regexp (&optional delimited)
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
        (goto-char pnt)
        (setq my/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end))))
  :bind
  (("C-M-%" . re-builder)
   :map reb-mode-map
   ("RET" . my/reb-replace-regexp)
   :map reb-lisp-mode-map
   ("RET" . my/reb-replace-regexp))
  :custom
  (reb-re-syntax 'rx))

(use-package ediff
  ;; better merge conflict management
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w"))

(use-package erc
  ;; emacs irc interface
  :straight nil
  :hook
  ((erc-insert-post . erc-save-buffer-in-logs)
   (erc-mode . (lambda () (variable-pitch-mode 1))))
  :custom
  (erc-hide-timestamps t)
  (erc-generate-log-file-name-function (quote erc-generate-log-file-name-with-date))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (erc-log-mode))

(use-package em-smart
  ;; Plan-9 interface for eshell
  :straight (:type built-in)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))

(use-package eshell
  ;; elisp shell and repl
  :straight (:type built-in)
  :preface
  (defun my/eshell-here ()
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)
      (eshell-smart-initialize)))
  (defun eshell/x ()
    "Fast close and delete."
    ;; can't use my/ here, for eshell to pick it up
    (delete-window)
    (eshell/exit))
  (defun my/eshell-lint ()
    "Opens up a new shell and runs lint."
    (interactive)
    (let* ((height (/ (window-total-height) 4)))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: lint*"))
      (insert (concat "npm run lint"))
      (eshell-send-input)))
  :bind
  (([f8] . my/eshell-here)
   ([f9] . my/eshell-lint)))

(use-package proced
  :straight nil
  :preface
  (defun proced-status--around (orig-proced &rest args)
    "Set proced status to fullscreen."
    (window-configuration-to-register :my/proced-fullscreen)
    (apply orig-proced args)
    (delete-other-windows))

  (defun proced-quit--around (orig-proced-quit &rest args)
    "Restore previous window configuration."
    (if (equal (symbol-name major-mode) "proced-mode")
        (progn
          (apply orig-proced-quit args)
          (jump-to-register :my/proced-fullscreen))
      (apply orig-proced-quit args)))
  :custom (proced-auto-update-flat t)
  :config
  (advice-add 'proced :around #'proced-status--around)
  (advice-add 'quit-window :around #'proced-quit--around))

(use-package proced-narrow
  :ensure t
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))

(use-package tab-bar
  :straight nil
  :custom
  (tab-bar-mode 1)
  :config
  (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar))

(use-package flymake
  :straight nil
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package emacs
  ;; Stuff that doesn't seem to belong anywhere else
  :straight nil
  :preface
  (defun my/back-to-indentation-or-beginning () (interactive)
         (if (= (point) (progn (back-to-indentation) (point)))
             (beginning-of-line)))

  (defun my/visiting-buffer-rename (file newname &optional _ok-if-already-exists)
    "Rename buffer visiting FILE to NEWNAME.
  Intended as :after advice for `rename-file'."
    (when (called-interactively-p 'any)
      (when-let ((buffer (get-file-buffer file)))
        (with-current-buffer buffer
          (set-visited-file-name newname nil t)
          (when (derived-mode-p 'emacs-lisp-mode)
            (save-excursion
              (let* ((base (file-name-nondirectory file))
                     (sans (file-name-sans-extension base))
                     (newbase (file-name-nondirectory newname))
                     (newsans (file-name-sans-extension newbase)))
                (goto-char (point-min))
                (while (search-forward-regexp (format "^;;; %s" base) nil t)
                  (replace-match (concat ";;; " newbase)))
                (goto-char (point-max))
                (when
                    (search-backward-regexp (format "^(provide '%s)" sans) nil t)
                  (replace-match (format "(provide '%s)" newsans))))))))))

  (defun my/visiting-buffer-kill (file &optional _trash)
    "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
    (when (called-interactively-p 'any)
      (when-let ((buffer (get-file-buffer file)))
        (kill-buffer buffer))))

  (defun my/switch-to-last-buffer ()
    "Flip between two buffers."
    (interactive)
    (switch-to-buffer nil))

  (defun my/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line."
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end)
      (next-logical-line)
      (back-to-indentation)))

  (defun my/three-column-layout ()
    "Set the frame to three columns."
    (interactive)
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-horizontally)
    (balance-windows))
  :init
  (advice-add
   #'completing-read-multiple
   :override #'consult-completing-read-multiple)
  :bind
  (("C-a" . my/back-to-indentation-or-beginning)
   ("C-," . my/comment-or-uncomment-region-or-line)
   ("C-c b" . my/switch-to-last-buffer)
   ("C-c C-c" . server-edit)
   ("C-c C-k" . server-edit-abort)
   ([f1] . project-find-file)
   ([f3] . start-kbd-macro)
   ([f4] . end-kbd-macro)
   ([f5] . kmacro-call-macro))
  :hook
  ((text-mode . visual-line-mode)
   (minibuffer-setup . cursor-intangible-mode))
  :custom
  (inhibit-startup-message t)           ; no splash screen
  (visible-bell t)                      ; be quiet
  (indicate-empty-lines t)              ; show lines at the end of buffer
  (sentence-end-double-space nil)       ; single space after a sentence
  (indent-tabs-mode nil)                ; use spaces instead of tabs
  (tab-width 2)
  (cursor-type '(bar . 2))              ; no fat cursor
  (js-indent-level 2)                   ; js settings needed for rjsx
  (js-switch-indent-offset 2)           ; more js settings
  (css-indent-offset 2)
  (fill-column 80)                      ; default fill column
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ; no cursor in minibuffer
  (enable-recursive-minibuffer t)
  (help-window-select t)                ; move cursor to popup help windows
  (ad-redefinition-action 'accept)      ; silence warnings for redefinition
  (cursor-in-non-selected-windows nil)  ; Hide cursor in inactive windows
  (warning-suppress-types '((comp)))
  (use-short-answers t)                 ; y on n to confirm
  (sh-basic-offset 2)                   ; indentation 2 spaces
  :config
  (advice-add 'rename-file :after 'my/visiting-buffer-rename)
  (advice-add 'delete-file :after 'my/visiting-buffer-kill)
  (delete-selection-mode)
  (put 'upcase-region 'disabled nil)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode 1)                   ; Show matching parens
  (set-face-attribute 'default nil :family "Iosevka" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 160)
  (global-hl-line-mode 1)
  (set-face-background 'cursor "red")
  (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local outline-regexp (rx ";;;" (* not-newline)))))
  ;;(menu-bar-mode)
  (repeat-mode)
  (windmove-default-keybindings)
  (desktop-read "~/.config/emacs/"))

(when (eq system-type 'gnu/linux)
  (require 'vterm))

(when (eq system-type 'darwin)
  (use-package vterm)
  (add-to-list 'completion-ignored-extensions ".DS_STORE")
  (set-face-attribute 'default (selected-frame) :family "Iosevka" :height 200)
  (set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 200)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq auto-save-default nil)
  (setq frame-resize-pixelwise t)
  (setq magit-git-executable "/usr/local/bin/git")
  (set-frame-size (selected-frame) 2542 1412 t)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (setq ns-use-native-fullscreen nil))

(provide 'init)
;;; init.el ends here
