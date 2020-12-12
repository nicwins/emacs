;;; appearance --- Sets appearance defaults not associated with a particular mode.

;;; Commentary:
;; Any appearace customs particular to non-built-in modes should be placed with the set for that
;; mode, not here.

;;; Code:

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; force line word wrapping in text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme
(load-theme 'gruvbox-dark-hard t)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Unclutter the modeline
(sml/setup)

;; add fill column
(global-display-fill-column-indicator-mode)
(set-face-attribute 'fill-column-indicator nil :foreground "grey27")
(setq-default display-fill-column-indicator-column 99)

(provide 'appearance)
;;; appearance.el ends here
