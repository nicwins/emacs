;;; key-bindings.el --- Keybindings related to built-in emacs functions

;;; Commentary:

;; Bindings related to specific modes should go with the code for that mode.
;; bindings for built in Emacs functions should go here, or this file should be
;; merged in with init.el

;;; Code:

;; I don't need to kill Emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; call macro with f5
(global-set-key [f5] 'call-last-kbd-macro)

;; backword kill word easily
(global-set-key "\C-w" 'backward-kill-word)

;; reset kill region to c-x c-l
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; swap C-f and C-s
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s") 'forward-char)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

;; duplicate line
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'delete-char)
(provide 'key-bindings)

;;; key-bindings.el ends here
