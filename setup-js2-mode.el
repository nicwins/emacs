(setq-default js2-global-externs '("define" "describe" "expect" "it" "require" "$" "_" "Backbone" "JSON" "setTimeout" "jasmine" "beforeEach" "afterEach" "spyOn"))

(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-basic-offset 2)

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
;;(font-lock-add-keywords
;; 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
;;              (0 (progn (compose-region (match-beginning 1)
;;                                        (match-end 1) "\u2190")
;;                        nil)))))


(provide 'setup-js2-mode)
