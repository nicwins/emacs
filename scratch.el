;;; scratch.el --- temp stuff
;;; Commentary:
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;;; Code:

(setq-local modebit (format "%s" projectile-mode-line))
(setq modebit (s-split "\\]" (pop (cdr (s-split "\\[" modebit)))))
(setq modebit (pop modebit))
(setq modebit (format "%s" (propertize modebit 'face '(:weight bold :foreground "Blue"))))
(setq projectile-mode-line (concat " [" modebit "]"))
(message modebit)
(force-mode-line-update)

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "projectile-mode" projectile-mode "[]")

;;; scratch.el ends here
