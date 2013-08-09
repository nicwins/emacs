;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup packages
(require 'setup-package)

;; Setup Dired
(eval-after-load 'dired '(require 'setup-dired))
(require 'dired)

;; Setup IDO
(eval-after-load 'ido '(require 'setup-ido))
(require 'ido)

;; Slime setup
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'slime)
(slime-setup)
(add-hook 'after-init-hook
          #'(lambda ()
              (when (locate-library "slime-js")
                (require 'setup-slime-js))))

;; Map files to modes
(require 'mode-mappings)

;; Setup key bindings
(require 'key-bindings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Cua Mode
(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

;; Store auto save in temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set up el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; el-get recipies
(setq
 el-get-sources
 '((:name smex    ;; a better (ido-like M-x )
          :type git
          :url "git://github.com/nonsequitur/smex.git"
          :features smex
          :after (progn
                   (setq smex-save-file "~/.emacs.d/backups/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                   (smex-initialize))
          )

   (:name buffer-move     ; have to add your own keys
          :after (progn
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right))
          )

   (:name magit       ; git meet emacs, and a binding
          :type git
          :url "git://github.com/magit/magit.git"
          :features magit
          :after (progn
                   (global-set-key (kbd "C-x C-z") 'magit-status))
          )

   (:name goto-last-change    ; move pointer back to last change
          :after (progn
                   (global-set-key (kbd "C-x C-/") 'goto-last-change))
          )

   (:name undo-tree    ;; Represent undo-history as an actual tree
          :type git
          :url "http://www.dr-qubit.org/git/undo-tree.git"
          :features undo-tree
          :after (progn
                   (global-undo-tree-mode)
                   (setq undo-tree-mode-lighter ""))
          )

   (:name autopair    ;; Autopair automagically
          :type git
          :url "git://github.com/capitaomorte/autopair.git"
          :features autopair
          :after (progn
                   (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))
                   (autopair-global-mode))
          )

   (:name auto-indent  ;; Indent files and lines automatically
          :type git
          :url "https://github.com/mlf176f2/auto-indent-mode.el.git"
          :features auto-indent-mode
          :after (progn
                   (setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
                   (auto-indent-global-mode))
          )

   (:name smooth-scrolling ;; Keep cursor away from edges when scrolling up/down
          :type git
          :url "git://github.com/aspiers/smooth-scrolling.git"
          :features smooth-scrolling
          )

   (:name slime-js ;; slime part for swank-js
          :type git
          :url "git://github.com/Gozala/slime-js.git"
          :features slime-js
          )

   (:name emmet-mode ;; new Zencoding mode
          :type git
          :url "git://github.com/smihica/emmet-mode.git"
          :features emmet-mode
          :after (progn
                   (add-hook 'sgml-mode-hook 'emmet-mode)
                   (add-hook 'css-mode-hook 'emmet-mode)
                   (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;indent 2 spaces.
                   (eval-after-load 'emmet-mode
                     '(progn
                        (define-key emmet-mode-keymap (kbd "C-j") nil)
                        (define-key emmet-mode-keymap (kbd "<C-return>") nil)
                        (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)
                        (diminish 'emmet-mode))))
          )
   ))

(setq
 my:el-get-packages
 '(el-get
   auto-complete
   ido-ubiquitous
   diminish))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)
