(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval
   (quote
    ((sh-mode 1.5)
     (lisp-interaction-mode 1.5)
     (fundamental-mode 1.5)
     (shell-mode 1.5)
     (js-mode 1.5)
     (emacs-lisp-mode 1.5)
     (ruby-mode 1.5)
     (css-mode 1.5)
     (html-mode 1.5)
     (js2-mode 1.5)
     (nxml-mode 0.0012033417256673178)
     (default 0.0005))))
 '(delete-active-region (quote kill))
 '(js2-global-externs
   (quote
    ("angular" "define" "describe" "expect" "it" "require" "$" "_" "Backbone" "JSON" "setTimeout" "jasmine" "beforeEach" "afterEach" "spyOn" "module" "inject")))
 '(js2-strict-missing-semi-warning nil)
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-warning ((t (:foreground "orange" :underline nil :weight bold))))
 )
