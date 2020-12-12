;;; keybindings.el --- Keybindings related to built-in emacs functions

;;; Commentary:

;; Bindings related to specific modes should go with the code for that mode.
;; bindings for built in Emacs functions should go here, or this file should be
;; merged in with init.el

;;; Code:

;; I don't need to kill Emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; common searches on function keys
(global-set-key '[f1] 'helm-projectile) ;; find file in project
(global-set-key '[f2] 'projectile-ag)   ;; find text in projet
(global-set-key '[f5] 'call-last-kbd-macro)

;; which-key
(use-package which-key
             :delight
             :commands (which-key-mode)
             :init (which-key-mode)
             :defer 0.2
             :config
             (progn
               (setq ;; which-key-idle-delay 0.2 ;; Time before which-key pops up
                which-key-allow-evil-operators t ;; Show evil keybindings
                which-key-sort-order 'which-key-key-order-alpha)))

;; General.el, for setting keybindings like vim-leader
(defun save-all () "Save all open buffers." (interactive) (save-some-buffers t))

(use-package general
             :config
             (progn
               (general-create-definer my-leader-def
                                       :prefix "SPC")
               (my-leader-def 'normal
                              "w" 'save-buffer
                              "W" 'save-all
                              "q" 'kill-buffer-and-window
                              "h" 'dired-jump
                              "v" 'split-window-right
                              "e" 'pp-eval-last-sexp
                              "SPC" 'other-window
                              "b" 'ibuffer
                              "x" 'helm-M-x
                              "g" 'magit-status
                              "G" 'magit-blame-mode
                              "k" 'kill-this-buffer
                              "K" 'kill-buffer
                              "o" 'helm-occur
                              "T" 'eshell)))



               (provide 'keybindings)
;;; keybindings.el ends here
