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

;; Keep emacs Custom-settings in separate file
;;(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
;;(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; When running in Windows, we want to use an alternate shell
;;(setq shell-file-name "C:/Program Files/git/bin/bash")
;;(setq explicit-shell-file-name shell-file-name)

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(packages-install
 (cons 'paredit melpa))

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(require 'ido)
(eval-after-load 'dired '(require 'setup-dired))
(require 'dired)
(require 'js2-highlight-vars)
(require 'setup-zencoding)
                                        ;(require 'paredit)
(require 'setup-ffip)
(require 'setup-wrap-region)
(require 'setup-perspective)
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Slime setup
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "C:/home/nic/.emacs.d/site-lisp/slime/")
(require 'slime)
(slime-setup)
(add-hook 'after-init-hook
          #'(lambda ()
              (when (locate-library "slime-js")
                (require 'setup-slime-js))))


;; Map files to modes
(require 'mode-mappings)
(add-to-list 'load-path "C:/home/nic/.emacs.d/site-lisp/bang/")
(add-to-list 'load-path "C:/home/nic/.emacs.d/site-lisp/mark-multiple/")
(add-to-list 'load-path "C:/home/nic/.emacs.d/site-lisp/js2-refactor/")
(require 'mark-multiple)
(require 'js2-refactor)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
                                        ;(diminish 'yas/minor-mode)

;; Misc
(require 'appearance)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval (quote ((slime-repl-mode 1.5) (css-mode 0.0013374828338623048) (html-mode 1.5) (emacs-lisp-mode 0.006116674084133572) (js2-mode 1.5) (default 0.0005)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
