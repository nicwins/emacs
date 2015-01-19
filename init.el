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

;; Install packages if they are missing
(defun init--install-packages ()
  "Install below packages if they are missing.
Will not delete unlisted packages."
  (packages-install
   '(ag
     auto-indent-mode
     css-eldoc
     company
     company-tern
     dash
     diminish
     dired-details+
     emmet-mode
     evil
     evil-leader
     evil-surround
     f
     fill-column-indicator
     flycheck
     helm
     helm-descbinds
     helm-projectile
     highlight-escape-sequences
     js2-mode
     linum-relative
     magit
     markdown-mode
     powerline
     projectile
     rainbow-delimiters
     ruby-block
     ruby-end
     robe
     rvm
     s
     skewer-mode
     smartparens
     smooth-scrolling
     tern
     undo-tree
     web-mode
     yasnippet
     zenburn-theme)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Add helpers
(require 'helpers)

;; auto-indent
(require 'auto-indent-mode)
;; If you want auto-indent on for files
(setq auto-indent-on-visit-file t)
(auto-indent-global-mode)

;; Company Mode
(global-company-mode t)

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

;; evil-leader-mode
(require 'setup-evil-leader)

;; evil-mode
(require 'setup-evil)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; flycheck
;; NOTE: requires npm install -g jshint for js2-mode
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Helm
(require 'setup-helm)

;; highlight-escape-sequences
(hes-mode)

;; js2-mode
(require 'js2-mode)

;; relative linum
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
  (set-face-attribute 'linum-relative-current-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground) :background nil :inherit 'linum :bold t))

(require 'linum-relative)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)
(eval-after-load 'magit '(require 'setup-magit))

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; powerline
(require 'setup-powerline)

;; projectile
(require 'projectile)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
(setq projectile-cache-file "~/.emacs.d/backups/projectile.cache")
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching nil)
(setq projectile-remember-window-configs t)
(projectile-global-mode)
(global-set-key '[f1] 'helm-projectile)
(global-set-key '[f2] 'projectile-ag)
(setq projectile-mode-line '(:eval (with-timeout (0.2 " Projectile[NOOO]")
                                     (format " Projectile[%s]" (projectile-project-name)))))

(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))

;; rainbow delimiters
(require 'rainbow-delimiters)
(--each '(css-mode-hook
          js2-mode-hook
          ruby-mode
          markdown-mode
          emacs-lisp-mode-hook)

  (add-hook it 'rainbow-delimiters-mode))

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

;; ruby-end

;; ruby mode
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))

;; smartparens default setup
;; (require 'smartparens-config)
;; (setq sp-autoescape-string-quote nil)
;; (--each '(css-mode-hook
;;           js2-mode-hook
;;           js-mode-hook
;;           sgml-mode-hook
;;           ruby-mode-hook
;;           markdown-mode-hook
;;           emacs-lisp-mode-hook)

;;   (add-hook it 'turn-on-smartparens-mode))

;; (sp-local-pair 'js2-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
;; (sp-local-pair 'js-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))
;; (sp-local-pair 'ruby-mode "{" nil :post-handlers '((my-open-block-sexp "RET")))

;; Electric
(electric-pair-mode 1)
(electric-indent-mode nil)

(defun my-open-block-sexp (&rest _ignored)
  "Insert a new line in a newly opened and newlined block."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; RVM Integration
(require 'rvm)
(rvm-use-default)

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

;; Shell-mode
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(put 'erase-buffer 'disabled nil)

;; Skewer
(add-hook 'js2-mode-hook 'skewer-mode)

;; tern-mode
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-to-list 'company-backends 'company-tern)

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

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'init)
;;; init.el ends here
