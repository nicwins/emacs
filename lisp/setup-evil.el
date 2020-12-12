;;; setup-evil --- Evil Mode Customizations

;;; Commentary:
;;; Adds bindings and overrides for evil mode

;;; Code:

(require 'evil)

(evil-mode 1)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-move-cursor-back nil)

(defun set-key (map spec cmd)
  "Set in `MAP' `SPEC' to `CMD'.
`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (case map
                      (global #'global-set-key)
                      (local  #'local-set-key)
                      (t      (lambda (key def) (define-key map key def)))))
        (key (typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "Wrong argument")))))
    (funcall setter-fun key cmd)))

;; Map escape to jk
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)


(define-key evil-insert-state-map [left] 'undefined)
(define-key evil-insert-state-map [right] 'undefined)
(define-key evil-insert-state-map [up] 'undefined)
(define-key evil-insert-state-map [down] 'undefined)

(define-key evil-motion-state-map [left] 'undefined)
(define-key evil-motion-state-map [right] 'undefined)
(define-key evil-motion-state-map [up] 'undefined)
(define-key evil-motion-state-map [down] 'undefined)

(define-key evil-normal-state-map (kbd "C-x C-s") 'undefined)
(define-key evil-normal-state-map (kbd "C-s") 'undefined)
(define-key evil-insert-state-map (kbd "C-s") 'undefined)
(define-key evil-normal-state-map (kbd "C-x s") 'undefined)
;;(define-key evil-normal-state-map (kbd "M-x") 'undefined)
;;(define-key evil-insert-state-map (kbd "M-x") 'undefined)

(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; State-mode overrides
(add-to-list 'evil-emacs-state-modes 'bookmark-bmenu-mode)
(add-to-list 'evil-emacs-state-modes 'ag-mode)
(add-to-list 'evil-emacs-state-modes 'comint-mode)
(add-to-list 'evil-emacs-state-modes 'eshell-mode)
(add-to-list 'evil-emacs-state-modes 'fundamental-mode)
(add-to-list 'evil-emacs-state-modes 'help-mode)
(add-to-list 'evil-emacs-state-modes 'term-mode)
(add-to-list 'evil-emacs-state-modes 'magit-mode)
(add-to-list 'evil-emacs-state-modes 'magit-status-mode)
(add-to-list 'evil-emacs-state-modes 'magit-log-mode)
(add-to-list 'evil-emacs-state-modes 'magit-diff-mode)
(add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
(add-to-list 'evil-emacs-state-modes 'magit-popup-sequence-mode)
(add-to-list 'evil-emacs-state-modes 'magit-log-select-mode)
(add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
(add-to-list 'evil-emacs-state-modes 'magit-revision-mode)
(add-to-list 'evil-emacs-state-modes 'epa-key-list-mode)

(defun get-major-mode-name ()
  "Message the major mode associated with current buffer."
  (interactive)
  (message "%s" major-mode))


(provide 'setup-evil)
;;; setup-evil ends here
