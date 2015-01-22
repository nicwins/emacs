;;; setup-magit --- Magit inits and helpers

;;; Commentary:
;; Adds full-screen to magit status, restores on exit.

;;; Code:

(defadvice magit-status (around magit-fullscreen activate)
  "Set magit status to full-screen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (around magit-quit-session activate)
  "Restore previous window configuration if we are burying magit-status."
  (if (equal (symbol-name major-mode) "magit-status-mode")
      (progn
        ad-do-it
        (jump-to-register :magit-fullscreen))
    ad-do-it
    (delete-other-windows)))

(provide 'setup-magit)
;;; setup-magit ends here
