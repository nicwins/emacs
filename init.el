;; init --- Initial setup -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 Nicolas Winslow

;; Author: Nicolas Winslow

;;; Commentary:

;;; Code:

;;;; Pre-Package Initialization

;;;; Initialize Package

;; This is only needed once, near the top of the file
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t)

;; Bootstrap `use-package'
(setq-default use-package-verbose nil            ; Don't report loading details
              use-package-enable-imenu-support t ; Let imenu find use-package defs
              use-package-expand-minimally t)    ; minimize expanded code

;;;; OS Defaults
(when (eq system-type 'gnu/linux)
  (require 'vterm)
  (set-face-attribute 'default nil :family "Iosevka" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 160)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 160))

(when (eq system-type 'darwin)
  (use-package vterm)
  (add-to-list 'completion-ignored-extensions ".DS_STORE")
  (set-face-attribute 'default (selected-frame) :family "Iosevka" :height 200)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 200)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq auto-save-default nil)
  (setq frame-resize-pixelwise t)
  (setq magit-git-executable "/usr/local/bin/git")
  (set-frame-size (selected-frame) 2542 1412 t)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (setq ns-use-native-fullscreen nil))

;;;; Package Configuration
;; early to catch litter before it is created
(use-package no-littering
  ;; cleanup all the clutter from varios modes
  ;; places configs in /etc and data in /var
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package exec-path-from-shell
  ;; Load path from user shell
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package gcmh
  ;; Minimizes GC interference with user activity.
  :config (gcmh-mode 1))

;; needs to be early for straight.el
(use-package org
  ;; notes and todos, executable blocks
  :custom
  ;; Edit settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-startup-indented t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")

  ;; files
  (org-agenda-files '("~/notes/todo.org"))
  :config
  ;; Activate SQL source code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)))
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package tex
  :straight auctex
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-newline-function #'newline-and-indent))

(use-package bug-hunter) ;; Automatically bisects init file

(use-package ws-butler
  ;; Automatic whitespace trimming
  :hook
  (prog-mode . ws-butler-mode))

(use-package aggressive-indent
  ;; Indent as you type
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-mode))

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
        ("?" . minibuffer-completion-help))
  :config
  (savehist-mode 1))

(use-package casual)

;; base mode for scheme
(use-package geiser-mit
  :config
  (setf geiser-active-implementations '(guile)))

;; guile scheme addition
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(use-package paredit
  :hook
  ((scheme-mode emacs-lisp-mode) . paredit-mode))

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

(use-package vertico-multiform
  ;; alternate display modes for vertico
  :straight nil
  :after (vertico)
  :preface
  (defun vertico/sort-directories-first (files)
    ;; Sort directories before files
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  :init
  (require 'vertico-buffer nil nil)
  (require 'vertico-indexed nil nil)
  (require 'vertico-flat nil nil)
  (require 'vertico-grid nil nil)
  :custom
  (vertico-multiform-commands
   '((consult-line buffer)
     (consult-imenu buffer indexed)))
  (vertico-multiform-categories
   '((file (vertico-sort-function . vertico/sort-directories-first) grid)
     (consult-grep buffer)
     (symbol (vertico-sort-function . vertico-sort-alpha))))
  :config
  (vertico-multiform-mode))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :custom
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)

  (corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (corfu-popupinfo-mode 1)   ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package orderless
  :after corfu
  ;; narrowing and filtering for selections
  :custom
  ;; from docs on orderless
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  ;; TAB-and-Go customizations
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  :config
  ;; enable sort by recent history
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  ;; add a popup with documentation next to suggestions
  (corfu-popupinfo-mode 1))

(use-package cape
  ;; add and merge completion at point functions
  :after corfu
  :init
  ;; put dabbrev first in list of completions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package rg) ; ripgrep for consult

(use-package consult
  ;; enhanced selection ui
  :commands consult--directory-prompt
  :preface
  (defun consult--fd-builder (input)
    (let ((fd-command
           (if (eq 0 (process-file-shell-command "fdfind"))
               "fdfind"
             "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
        (when re
          (cons (append
                 (list fd-command
                       "--color=never" "--full-path"
                       (consult--join-regexps re 'extended))
                 opts)
                hl)))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))
  :bind
  (("M-s" . consult-line)
   ("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g m" . consult-mark)
   ("M-'" . consult-register-store)
   ("M-#" . consult-register-load)
   ;; C-x bindings
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ;; C-c bindings (user-map)
   ("C-c i" . consult-imenu)
   ("C-c I" . consult-project-imenu)
   ("C-c z" . consult-flycheck)
   ;;("C-c F" . consult-lsp-diagnostics)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ([f1] . consult-fd)
   ([f2] . consult-ripgrep)
   :map isearch-mode-map
   ("M-e" . consult-isearch)
   ("M-s l" . consult-line-symbol-at-point))
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

  ;; these three functions set magit to fullscreen on a per-tab basis, and restore when exited.
  (defun magit-status--around (orig-magit-status &rest args)
    "Set magit status to fullscreen."
    (window-configuration-to-register
     (or
      (intern-soft (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))
      (intern (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))))
    (apply orig-magit-status args)
    (delete-other-windows))

  (defun magit-log-buffer-file--before (orig-fun &rest args)
    "Store the window configuration before logging."
    (window-configuration-to-register
     (or
      (intern-soft (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))
      (intern (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))))
    (apply orig-fun args))

  (defun magit-mode-bury-buffer--around (orig-magit-mode-bury-buffer &rest args)
    "Restore previous window configuration if we are burying magit-status."
    (if (or
         (equal (symbol-name major-mode) "magit-status-mode")
         (and (intern-soft (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))
              (equal (symbol-name major-mode) "magit-log-mode")))
        (progn
          (apply orig-magit-mode-bury-buffer args)
          (jump-to-register (intern-soft (concat ":my/magit-fullscreen-tab-" (number-to-string (tab-bar--current-tab-index))))))
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
  (setf (alist-get 'latexindent apheleia-formatters)
        '("/gnu/store/jp88s51bk25vz217f10xqbasc3jhp50a-texlive-texmf-20230313/share/texmf-dist/scripts/latexindent/latexindent.pl" "--logfile\=/dev/null"))
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier" "--print-width" "80" file))
  (apheleia-global-mode +1))

(use-package json-mode) ; major mode for json

(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (html-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/src/emacs-combobulate/combobulate")))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(use-package org-modern
  ;; theme/styling for org
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-list
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  :config
  (global-org-modern-mode))

(use-package org-modern-indent
  :straight
  (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t))))

(use-package mixed-pitch
  ;; use both pitch types in one buffer
  :hook (text-mode . mixed-pitch-mode))

(use-package visual-fill-column
  ;; Line wrap at the fill column, not buffer end
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package auth-source-pass
  :straight (:type built-in)
  :config
  (auth-source-pass-enable))

(use-package password-store
  ;; front end for `pass'
  :if (eq system-type 'gnu/linux))

(use-package pass
  ;; use the unix `pass' store
  ;;:if (eq system-type 'gnu/linux)
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

(use-package speedrect
  :straight (speedrect :type git :host github :repo "jdtsmith/speedrect"))

(use-package buffer-move
  :bind
  (("<M-left>" . buf-move-left)
   ("<M-right>" . buf-move-right)
   ("<M-up>" . buf-move-up)
   ("<M-down>" . buf-move-down)))

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
               ("/" . dired-narrow))))/

(use-package dired-filter
  :ensure t
  :custom
  (dired-filter-prefix "f"))

(use-package async
  ;; use dired functions async
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

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
  (wgrep-enable-key "\C-c\C-c"))

(use-package which-key
  ;; Display keybindings in popup
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 1)
  :bind
  ("C-c ?" . which-key-show-top-level))

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
  :preface
  (defun my/isearch-aim-beginning ()
    "Move cursor back to the beginning of the current match."
    (when (and isearch-forward (number-or-marker-p isearch-other-end))
      (goto-char isearch-other-end)))
  :hook
  (isearch-update-post . my/isearch-aim-beginning)
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-buffer t)
  (lazy-highlight-initial-delay 0))

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

(use-package calc
  ;; calculator
  :straight (:type built-in)
  :bind
  (:map calc-mode-map
        ("?" . casual-calc-tmenu)))

(use-package dired
  ;; directory management
  :straight (:type built-in)
  :preface
  (defun my/dired-quickfind (find-string)
    "Fuzzy find `find-string' recursively in current dir."
    (interactive (list (read-string "Search:")))
    (find-dired
     (file-name-directory (or buffer-file-name default-directory))
     (concat "-iname " "*\"" find-string "\"*")))
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
        ;;("f" . my/dired-quickfind)
        ("C-c C-c" . dired-toggle-read-only)
        ("?" . casual-dired-tmenu))
  :custom
  (dired-dwim-target t)
  ;; Dired listing switches - see man ls
  (dired-listing-switches "-alhF --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (find-name-arg "-iname"))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  (add-hook 'dired-subtree-after-insert-hook #'dired-omit-mode)
  (add-hook 'dired-subtree-before-insert-hook #'dired-omit-mode))

(use-package dired-x
  ;; extension for dired
  :straight (:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-guess-shell-alist-user
   (list
    (list "\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\|webm\\|mov\\)\\(?:\\.part\\)?\\'"
          "! (mpv ? &>/dev/null &)")
    (list "\\.pdf$" "zathura")))
  :config
  ;; setting this in custom throws a dired-omit-files is undefined
  (setq dired-omit-extensions (list ".DS_STORE")))

(eval-after-load 'dired
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

(use-package dired-du)

(use-package wdired
  ;; editable dired buffers
  :straight (:type built-in)
  :bind ((:map wdired-mode-map
               ("C-x C-s" . wdired-finish-edit)))
  :custom
  (wdired-allow-to-change-permissions t))

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
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil))

;; (use-package eldoc-box
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package free-keys)

(use-package tabspaces
  ;; buffer isolated workspaces per tab
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  :config
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package simple
  :straight nil
  :preface
  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
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
  :bind
  (("C-`" . push-mark-no-activate)
   ("M-`" . jump-to-mark)
   ("C-x C-x" . exchange-point-and-mark-no-activate)))

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

(use-package savehist
  :straight nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package files
  ;; General file handling
  :straight nil
  :custom
  (backup-by-copying t)
  (make-backup-files nil)
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
    (let* ((re (reb-target-value 'reb-regexp))
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
  :preface
  (defvar ct/circle-numbers-alist
    '((1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")

  (defun ct/tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (concat
       ;; First, add the tab number with a custom face
       (propertize
        (when (and tab-bar-tab-hints (< i 10)) (alist-get i ct/circle-numbers-alist)))
       ;; Add a space (unstyled)
       " "
       ;; Add tab name with the face returned by tab-bar-tab-face-function
       (propertize
        (concat (alist-get 'name tab)
	              (or (and tab-bar-close-button-show
			                   (not (eq tab-bar-close-button-show
				                          (if current-p 'non-selected 'selected)))
			                   tab-bar-close-button)
		                ""))
        'face (funcall tab-bar-tab-face-function tab)))))
  :bind
  ("C-M-s-x" . (lambda () (interactive) (tab-bar-select-tab 1)))
  ("C-M-s-c" . (lambda () (interactive) (tab-bar-select-tab 2)))
  ("C-M-s-d" . (lambda () (interactive) (tab-bar-select-tab 3)))
  ("C-M-s-r" . (lambda () (interactive) (tab-bar-select-tab 4)))
  ("C-M-s-s" . (lambda () (interactive) (tab-bar-select-tab 5)))
  ("C-M-s-t" . (lambda () (interactive) (tab-bar-select-tab 6)))
  ("C-M-s-w" . (lambda () (interactive) (tab-bar-select-tab 7)))
  ("C-M-s-f" . (lambda () (interactive) (tab-bar-select-tab 8)))
  ("C-M-s-p" . (lambda () (interactive) (tab-bar-select-tab 9)))
  :custom
  (tab-bar-mode 1)
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)
  (tab-bar-close-button nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-format '(tab-bar-format-menu-bar
                    tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package lsp-mode
  :ensure t
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  ;; temp for hangs
  (lsp-response-timeout 1)
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;; completion from corfu
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; (use-package lsp-completion
;;   :straight nil
;;   :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :straight nil
  :demand t
  :after lsp-mode)

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package delsel
  :straight nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package emacs
  ;; Stuff that doesn't seem to belong anywhere else
  :straight nil
  :preface
  (defun my/back-to-indentation-or-beginning () (interactive)
         (if (= (point) (progn (back-to-indentation) (point)))
             (beginning-of-line)))

  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

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
  (defun vertico/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :bind
  (("C-a" . my/back-to-indentation-or-beginning)
   ("C-," . my/comment-or-uncomment-region-or-line)
   ("C-c b" . my/switch-to-last-buffer)
   ("C-c C-c" . server-edit)
   ("C-c C-k" . server-edit-abort)
   ([f3] . start-kbd-macro)
   ([f4] . end-kbd-macro)
   ([f5] . kmacro-call-macro)
   (:map global-map
         ("C-g" . prot/keyboard-quit-dwim)))
  :hook
  ((text-mode . visual-line-mode)
   (minibuffer-setup . cursor-intangible-mode))
  :custom
  (use-package-compute-statistics t)
  (inhibit-startup-message t)           ; no splash screen
  (visible-bell t)                      ; be quiet
  (indicate-empty-lines t)              ; show lines at the end of buffer
  (sentence-end-double-space nil)       ; single space after a sentence
  (indent-tabs-mode nil)                ; use spaces instead of tabs
  (tab-width 2)
  (cursor-type '(bar . 2))              ; no fat cursor
  (css-indent-offset 2)
  (fill-column 80)                      ; default fill column
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)) ; no cursor in minibuffer
  (enable-recursive-minibuffer t)
  (help-window-select t)                ; move cursor to popup help windows
  (ad-redefinition-action 'accept)      ; silence warnings for redefinition
  (cursor-in-non-selected-windows nil)  ; Hide cursor in inactive windows
  (warning-suppress-types '((comp)))
  (use-short-answers t)               ; y on n to confirm
  (sh-basic-offset 2)                 ; indentation 2 spaces
  (js-indent-level 2)                 ; indentation 2 spaces in js derived modes
  (image-dired-thumb-size 256)        ; dired thumbnail size
  (desktop-load-locked-desktop t)     ; load desktop even if starting from crash
  (compilation-scroll-output 'first-error)
  (kill-whole-line t)           ; if on col 0, kills line instead of emptying it
  (tramp-connection-timeout 5)
  (proced-filter 'all)                        ; show processes from all users
  (backward-delete-char-untabify-method 'all) ; delete whole line when only whitespace
  (next-line-add-newlines t)  ; add newline on C-n if point is at end of buffer.
  (enable-local-variables :safe)
  :config
  (pixel-scroll-precision-mode)
  (advice-add 'rename-file :after 'my/visiting-buffer-rename)
  (advice-add 'delete-file :after 'my/visiting-buffer-kill)
  (delete-selection-mode)
  (put 'upcase-region 'disabled nil)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode 1)                   ; Show matching parens
  (global-hl-line-mode 1)
  (set-face-background 'cursor "red")
  (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
  (set-face-attribute 'italic nil :slant 'italic :underline 'unspecified)
  (set-frame-parameter nil 'alpha-background 95)
  (windmove-default-keybindings)
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local outline-regexp (rx ";;;" (* not-newline)))))
  (desktop-read "~/.config/emacs/"))

(provide 'init)
;;; init.el ends here
