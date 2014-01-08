;;; setup-magit --- Magit Initialization and helpers
;;; Commentary:
;;; Code:

(set-default 'magit-stage-all-confirm nil)

(defadvice magit-status (around magit-fullscreen activate)
  "Set magit status to full-screen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'setup-magit)
;;; setup-magit ends here
