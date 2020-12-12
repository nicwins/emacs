;;; init --- Initial setup

;;; Commentary:

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

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
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Add variable to user-lisp-directory
(defvar user-lisp-directory)
(setq user-lisp-directory (expand-file-name "lisp" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-lisp-directory))
(load custom-file)
(setq make-backup-files nil)

;; Setup package -- MELPA
(require 'setup-package)

;; Add helpers
(require 'helpers)

;; Install packages if they are missing
;; (defun init--install-packages ()
;;   "Install below packages if they are missing.  Will not delete unlisted packages."
;;   (packages-install
;;    '(company
;;      dash
;;      delight
;;      evil
;;      evil-leader
;;      evil-surround
;;      f
;;      flycheck
;;      gruvbox-theme
;;      helm
;;      helm-ag
;;      helm-descbinds
;;      helm-projectile
;;      key-chord
;;      linum-relative
;;      magit
;;      markdown-mode
;;      prettier
;;      projectile
;;      rainbow-delimiters
;;      rjsx-mode
;;      s
;;      smart-mode-line
;;      smartparens
;;      smooth-scrolling
;;      tide
;;      undo-tree
;;      which-key)))

;; Company Mode
(use-package company
             :delight
             :config (global-company-mode t))

;; Dash
(use-package dash)

;; Setup Dired
(use-package dired)

;; Setup Key Chord, used to map evil escape
(use-package key-chord
             :init (setq key-chord-two-keys-delay 0.3)
             :config (key-chord-mode 1))

;; evil-mode
(use-package evil)
(require 'setup-evil)

;; evil-surround
(use-package evil-surround
             :ensure t
             :config (global-evil-surround-mode 1))

;; flycheck
(use-package flycheck
             :init (setq flycheck-emacs-lisp-load-path 'inherit)
             :config (global-flycheck-mode))

;; Helm
(use-package helm)
(require 'setup-helm)

;; relative linum
(use-package linum-relative)
(after 'linum-relative
  (defun bw/disable-linum-mode ()
    "Disables linum-mode"
    (linum-mode -1))

  (defun bw/linum-non-relative (line-number)
    "Linum formatter that copies the format"
    (propertize (format linum-relative-format line-number)
                'face 'linum))

  (defun bw/linum-relative-formatting ()
    "Turn on relative formatting"
    (setq-local linum-format 'linum-relative))

  (defun bw/linum-normal-formatting ()
    "Turn on non-relative formatting"
    (setq-local linum-format 'bw/linum-non-relative))

  ;; I never use linum-mode except for this, so it's okay to
  ;; clobber it
  (setq linum-format 'bw/linum-non-relative
        ;; show >> on line where cursor is
        linum-relative-current-symbol ">>")
  ;; in Normal mode, use relative numbering
  (add-hook 'evil-normal-state-entry-hook 'bw/linum-relative-formatting)
  ;; in Insert mode, use normal line numbering
  (add-hook 'evil-insert-state-entry-hook 'bw/linum-normal-formatting)
  ;; turn off linum mode automatically when entering Emacs mode
  (add-hook 'evil-emacs-state-entry-hook 'bw/disable-linum-mode)
  ;; turn off linum mode when entering Emacs
  (add-hook 'evil-emacs-state-entry-hook 'bw/linum-normal-formatting)

  ;; copy linum face so it doesn't look weird
  (set-face-attribute 'linum-relative-current-face nil :foreground
                      (face-attribute 'font-lock-keyword-face :foreground)
                      :background nil :inherit 'linum :bold t))

;; magit
(use-package magit)
(require 'setup-magit)

;; markdown Mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; prettier
;; (require 'prettier-js)
;; (add-hook 'after-init-hook #'global-prettier-mode)
;; (setq prettier-js-args '(
;;   "--bracket-spacing" "true"
;;   "--single-quote" "true"
;;   "--no-semi" "true"
;;   "--jsx-single-quote" "true"
;;   "--jsx-bracket-same-line" "true"))


;; projectile
(use-package projectile
             :init
             (setq projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
             (setq projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
             (setq projectile-cache-file "~/.emacs.d/backups/projectile.cache")
             (setq projectile-switch-project-action 'helm-projectile)
             (setq projectile-enable-caching nil)
             (setq projectile-remember-window-configs t)
             :config (projectile-global-mode))

(use-package helm-projectile
             :config (helm-projectile-on))

;; rainbow delimiters
(require 'rainbow-delimiters)
(--each '(css-mode-hook
          rjsx-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)

  (add-hook it 'rainbow-delimiters-mode))

;; rjsx mode
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; ruby mode
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))

(defun my-open-block-sexp ()
  "Insert a new line in a newly opened and newlined block."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; smooth-scrolling
(require 'smooth-scrolling)

;; tide-mode
(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-hook 'rjsx-mode-hook #'setup-tide-mode)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-mode-lighter "")

;; setup Delight, reduce modeline clutter
(require 'delight)
(delight '((eldoc-mode nil "eldoc")
           (helm-mode)
           (which-key)
           (company-mode)))

;; Set up appearance
(require 'appearance)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Mode Mapping
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; Setup key bindings
;; General.el and which-key are setup here
(require 'keybindings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Shell-mode
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(setq multi-term-program-switches "--login")

(put 'erase-buffer 'disabled nil)

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(toggle-frame-fullscreen)

(provide 'init)
;;; init.el ends here
