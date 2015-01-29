;;; setup-helm --- Helm inits and helpers

;;; Commentary:
;;; Enables fuzzy matching and key bindings for common helm modes

;;; Code:


;;; Enable Modes
;;
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-autoresize-mode 1)


(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x c o") 'helm-occur)

(setq helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*Compile-Log\\*" "\\*magit" "\\*Customize Group"))

(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(require 'helm-descbinds)
(helm-descbinds-mode)

(provide 'setup-helm)
;;; setup-helm ends here
