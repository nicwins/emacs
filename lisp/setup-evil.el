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

(defun take (n lst)
  "Return at most the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq  lst (cdr lst)))
    (nreverse acc)))

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun pour-mappings-to (map mappings)
  "Call `set-key' with `MAP' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (set-key map (car mapping) (cadr mapping)))
  map)

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (declare (indent defun))
  (pour-mappings-to keymap mappings))

(evil-define-command cofi/evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?j)
        (exit-key ?k))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(fill-keymap evil-insert-state-map
  ;;"SPC" 'cofi/yas-expand-or-spc
  "j"   'cofi/evil-maybe-exit
  "C-h" 'backward-delete-char
  "C-k" 'kill-line
  "C-y" 'yank
  "C-e" 'end-of-line
  "C-a" 'beginning-of-line)

(fill-keymap evil-normal-state-map
  "C-k" 'kill-line
  "C-e" 'end-of-line
  "C-a" 'beginning-of-line)

(define-key evil-insert-state-map [left] 'undefined)
(define-key evil-insert-state-map [right] 'undefined)
(define-key evil-insert-state-map [up] 'undefined)
(define-key evil-insert-state-map [down] 'undefined)

(define-key evil-motion-state-map [left] 'undefined)
(define-key evil-motion-state-map [right] 'undefined)
(define-key evil-motion-state-map [up] 'undefined)
(define-key evil-motion-state-map [down] 'undefined)

(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-x C-s") 'undefined)
(define-key evil-normal-state-map (kbd "C-x s") 'undefined)

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

(defun get-major-mode-name ()
  "Message the major mode associated with current buffer."
  (interactive)
  (message "%s" major-mode))


(provide 'setup-evil)
;;; setup-evil ends here
