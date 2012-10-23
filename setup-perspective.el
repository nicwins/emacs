;; Load Perspective
(require 'perspective)

;; Enable perspective mode
(persp-mode t)

;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

(provide 'setup-perspective)
