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
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Add variable to user-lisp-directory
(defvar user-lisp-directory)
(setq user-lisp-directory (expand-file-name "lisp" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-lisp-directory))
(load custom-file)

;; Setup package -- MELPA
(require 'setup-package)

;; Install packages if they are missing
(defun init--install-packages ()
  "Install below packages if they are missing.
Will not delete unlisted packages."
  (packages-install
   '(ag
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
     helm
     helm-projectile
     highlight-escape-sequences
     js2-mode
     magit
     markdown-mode
     powerline
     projectile
     rainbow-delimiters
     ruby-block
     ruby-end
     s
     smartparens
     smooth-scrolling
     undo-tree
     web-mode
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
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load 'emmet-mode
  '(progn
     (define-key emmet-mode-keymap (kbd "C-j") nil)
     (define-key emmet-mode-keymap (kbd "<C-return>") nil)
     (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)))

;; flycheck
;; NOTE: requires npm install -g jshint for js2-mode
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Helm
(helm-mode t)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "C-x b") 'helm-mini)
(helm-autoresize-mode 1)

;; highlight-escape-sequences
(hes-mode)

;; js2-mode
(require 'js2-mode)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)
(eval-after-load 'magit '(require 'setup-magit))

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; projectile
(require 'projectile)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-known-projects-file "~/.emacs.d/backups/projectile-bookmarks.eld")
(setq projectile-cache-file "/home/winsln/.emacs.d/backups/projectile.cache")
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)
(setq projectile-remember-window-configs t)
(projectile-global-mode t)
(global-set-key '[f1] 'helm-projectile)
(global-set-key '[f2] 'projectile-ag)

(require 'helm-projectile)
(helm-projectile-on)

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

;; ruby-end


;; ruby mode
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))

;; smartparens default setup
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          js2-mode-hook
          js-mode-hook
          sgml-mode-hook
          ruby-mode-hook
          markdown-mode-hook
          emacs-lisp-mode-hook)

  (add-hook it 'turn-on-smartparens-mode))

(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
(sp-local-pair 'js-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
(sp-local-pair 'ruby-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))

(defun my-open-block-sexp (&rest _ignored)
  "Insert a new line in a newly opened and newlined block. _IGNORED params."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

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
(toggle-frame-fullscreen)

;; Shell-mode
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(put 'erase-buffer 'disabled nil)

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init)
;;; init.el ends here
