
;;; appearance --- Sets appearance defaults not associated with a particular mode

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
(load-theme 'zenburn t)
(set-face-attribute 'default nil :font "Droid Sans Mono" :height 100)

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
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-mode))
(eval-after-load "whitespace" '(diminish 'whitespace-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(provide 'appearance)
;;; appearance.el ends here
