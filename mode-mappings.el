;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-hook 'html-mode-hook
          (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;;(autoload 'js2-mode "js2-mode" nil t)
(defun my-js2-mode-hook ()
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  )
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(add-hook 'js2-mode-hook (lambda ()
                           (require 'setup-js2-mode)
                           ))

(provide 'mode-mappings)
