;;; setup-magit --- Magit inits and helpers

;;; Commentary:
;; Adds full-screen to magit status, magit-diff with commit, and colorizes the diff screen.

;;; Code:

(setq-default magit-stage-all-confirm nil)
(set-face-foreground 'diff-added "#fff")
(set-face-background 'diff-added "#398439")
(set-face-foreground 'diff-added "#fff")
(set-face-background 'diff-removed "#ac2925")

(setq-default magit-highlight-whitespace nil)
(setq-default magit-highlight-trailing-whitespace nil)
(setq-default magit-highlight-indentation nil)
(setq-default magit-diff-refine-hunk nil)

(add-hook 'server-switch-hook (lambda ()
                                (if (derived-mode-p 'git-commit-mode)
                                    (progn (delete-other-windows)
                                           (split-window-right)
                                           (switch-to-buffer-other-window "*magit-diff*")
                                           (other-window 1)))))

(add-hook 'server-done-hook (lambda ()
                              (kill-buffer "*magit-diff*")))

(defadvice magit-commit-internal (after magit-commit-diff activate)
  "Add the diff buffer to the commit view."
  (magit-diff-staged))

(defadvice magit-status (around magit-fullscreen activate)
  "Set magit status to full-screen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (kill-buffer "*magit-process*")
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'setup-magit)
;;; setup-magit ends here
