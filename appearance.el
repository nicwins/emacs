;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
                                        ;(turn-off-tool-bar)
  (tooltip-mode -1)
                                        ;(turn-off-tool-bar)
  (blink-cursor-mode -1))

;;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Make zooming affect frame instead of buffers
;;(require 'zoom-frm)

;; Zenburn please
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; These are the droids I'm looking for
(set-face-attribute 'default nil :font "Droid Sans Mono" :height 100)

(provide 'appearance)
