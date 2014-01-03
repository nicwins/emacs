;;; init --- Initial setup

;;; Commentary:
;;
;; Most of this has been cribbed from github/magnars/.emacs.d

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
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Setup packages
(require 'setup-package)

;; Install packages if they are missing
(defun init--install-packages ()
  "Install below packages if they are missing.
Will not delete unlisted packages."
  (packages-install
   '(ag
     angular-snippets
     auto-indent-mode
     auto-complete
     css-eldoc
     dash
     dired-details+
     emmet-mode
     f
     fill-column-indicator
     flycheck
     god-mode
     highlight-escape-sequences
     ido-at-point
     ido-ubiquitous
     inf-ruby
     js2-mode
     magit
     powerline
     rainbow-delimiters
     s
     smartparens
     smex
     smooth-scrolling
     undo-tree
     yasnippet
     zenburn-theme)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; auto-complete
(autoload 'auto-complete-mode "auto-complete" nil t)
(require 'auto-complete-config)
(setq ac-comphist-file  "~/.emacs.d/backups/ac-comphist.dat")
;;Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20131128.233/dict")
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
;; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; auto-indent
(require 'auto-indent-mode)
(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
(auto-indent-global-mode)

;; css-eldoc
(require 'css-eldoc)

;; Setup Dired
(eval-after-load 'dired '(require 'setup-dired))
(require 'dired)

;; dired-details+
(require 'dired-details+)
(setq-default dired-details-hidden-string "--- ")

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
;; indent 2 spaces.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load 'emmet-mode
  '(progn
     (define-key emmet-mode-keymap (kbd "C-j") nil)
     (define-key emmet-mode-keymap (kbd "<C-return>") nil)
     (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)))

;; Fill column indicator
(require 'fill-column-indicator)
(--each '(css-mode-hook
          js-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)
  (add-hook it 'fci-mode))

(--each '(css-mode-hook
          js-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)
  (add-hook it 'auto-fill-mode))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; god-mode command modes
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; highlight-escape-sequences
(hes-mode)

;; IDO
(eval-after-load 'ido '(require 'setup-ido))
(require 'ido)

;; inferior ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; js2-mode
(require 'js2-mode)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; rainbow delimiters
(require 'rainbow-delimiters)
(--each '(css-mode-hook
          js-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)

  (add-hook it 'rainbow-delimiters-mode))

;; ruby mode
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))

;; smartparens default setup
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          js-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)

  (add-hook it 'turn-on-smartparens-mode))

;; smex
(require 'smex)
(smex-initialize)

;; smooth-scrolling
(require 'smooth-scrolling)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-mode-lighter "")

;; Setup yasnippet
(require 'setup-yasnippet)

;; Set up appearance
(require 'appearance)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Map files to modes
(require 'mode-mappings)

;; Setup key bindings
(require 'key-bindings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Add helpers
(require 'helpers)

;; Go Fullscreen
(fullscreen)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(provide 'init)
;;; init.el ends here
