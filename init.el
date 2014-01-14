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
     diminish
     dired-details+
     emmet-mode
     f
     fill-column-indicator
     flycheck
     god-mode
     helm
     helm-projectile
     highlight-escape-sequences
     inf-ruby
     js2-mode
     magit
     popwin
     powerline
     projectile
     rainbow-delimiters
     s
     smartparens
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
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; auto-indent
(require 'auto-indent-mode)
;; If you want auto-indent on for files
(setq auto-indent-on-visit-file t)
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
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load 'emmet-mode
  '(progn
     (define-key emmet-mode-keymap (kbd "C-j") nil)
     (define-key emmet-mode-keymap (kbd "<C-return>") nil)
     (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)))

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; god-mode command modes
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(defun my-update-cursor ()
  "Set the cursor to a red bar if in god mode or read only.
Else set cursor to a white box."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box))
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        "#FF0000"
                      "#FFFFFF")))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Helm
(helm-mode t)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(define-key helm-map (kbd "C-;") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-mini)

;; highlight-escape-sequences
(hes-mode)

;; IDO
;;(eval-after-load 'ido '(require 'setup-ido))
;;(require 'ido)

;; inferior ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; js2-mode
(require 'js2-mode)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)
(eval-after-load 'magit '(require 'setup-magit))

;; popwin
(require 'popwin)
(popwin-mode 1)
(push '("*Help*" :height 30 :stick t) popwin:special-display-config)
(push '("*helm for files*" :height 20) popwin:special-display-config)
(push '("*helm projectile*" :height 20) popwin:special-display-config)
(push '("*helm mini*" :height 20) popwin:special-display-config)
(push '("*helm-mode-execute-extended-command*" :height 20) popwin:special-display-config)
(push '("*Warnings*" :height 20) popwin:special-display-config)
(push '("*helm-mode-magit-diff-working-tree*" :width 0.3 :position right)
      popwin:special-display-config)
(push '("*Procces List*" :height 20) popwin:special-display-config)
(push '("*Messages*" :height 20) popwin:special-display-config)
(push '("*Backtrace*" :height 20) popwin:special-display-config)
(push '("*Compile-Log*" :height 20 :noselect t) popwin:special-display-config)
(push '("*Remember*" :height 20) popwin:special-display-config)
(push '(" *undo-tree*" :width 0.1 :position right) popwin:special-display-config)
(push '("*All*" :height 20) popwin:special-display-config)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; projectile
(require 'projectile)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-known-projects-file "~/.emacs.d/backups/projectile-bookmarks.eld")
(setq projectile-cache-file "/home/winsln/.emacs.d/backups/projectile.cache")
(setq projectile-switch-project-action 'helm-projectile)
;;(setq projectile-remember-window-configs t)
(projectile-global-mode t)
(global-set-key '[f1] 'helm-projectile)
(global-set-key '[f2] 'projectile-ag)

(defun projectile-update-mode-line ()
  "Report project in mode-line."
  (let* ((project-name (projectile-project-name))
         (message (format " [%s]" project-name)))
    (setq projectile-mode-line message))
  (force-mode-line-update))

;; rainbow delimiters
(require 'rainbow-delimiters)
(--each '(css-mode-hook
          js2-mode-hook
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
          js2-mode-hook
          js-mode-hook
          sgml-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)

  (add-hook it 'turn-on-smartparens-mode))

(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
(sp-local-pair 'js-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
(sp-local-pair 'ruby-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))

(defun my-open-block-sexp (&rest _ignored)
  "Insert a new line in a newly opened and newlined block."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


;; smex
;;(require 'smex)
;;(smex-initialize)

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
;;(fullscreen)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(provide 'init)
;;; init.el ends here
