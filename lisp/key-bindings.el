;;; key-bindings.el --- Keybindings related to built-in emacs functions

;;; Commentary:

;; Bindings related to specific modes should go with the code for that mode.
;; bindings for built in Emacs functions should go here, or this file should be
;; merged in with init.el

;;; Code:

;; I don't need to kill Emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; call macro with f5
(global-set-key [f5] 'call-last-kbd-macro)

(provide 'key-bindings)
;;; key-bindings.el ends here
