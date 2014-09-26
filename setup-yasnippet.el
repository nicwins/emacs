
;;; setup-yasnippet --- Inits and defuns for yasnippet.

;;; Commentary:
;; Sets dir and enables global mode.

;;; Code:

(require 'yasnippet)

;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-c C-y") 'yas-insert-snippet)

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here
