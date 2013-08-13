;; Set up slime-js
;;
;; To install, see https://github.com/swank-js/swank-js/wiki/Installation
;;
;; This is what I did:
;;
;;     npm install swank-js -g
;;     M-x package-install slime-js
;;
;; The slime-js version in marmalade requires swank 2010.04.04, or at least
;; one prior to the breaking change in 2011.
;;
;; It also requires js2-mode, which is a good choice in either case. I highly
;; recommend this fork:
;;
;;     https://github.com/mooz/js2-mode
;;
;; My settings found in this file also requires js2-refactor:
;;
;;     https://github.com/magnars/js2-refactor.el
;;
;; I have included this file in init.el like so:
;;
;;     (add-hook 'after-init-hook
;;               #'(lambda ()
;;                   (when (locate-library "slime-js")
;;                     (require 'setup-slime-js))))
;;

(require 'comint)

;; target-url is the output from local server
(set-default 'slime-js-target-url "http://localhost:9000")
;; connect-url is where you direct your browser
(set-default 'slime-js-connect-url "http://localhost:8009")
;; show server where to find index
(set-default 'slime-js-starting-url "/")
(set-default 'slime-js-swank-command "swank-js")
(set-default 'slime-js-swank-args '())
(set-default 'slime-js-browser-command "google-chrome")
(set-default 'slime-js-browser-jacked-in-p nil)

;; (setq start-local-cmd "cd /home/winsln/projects/; node ./scripts/web-server.js")
(setq start-local-cmd "cd /home/winsln/projects/dashboard; grunt server")

(add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))


(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (apply #'make-comint "swank-js"  slime-js-swank-command nil slime-js-swank-args))

(defun slime-js-jack-in-node ()
  "Start a swank-js server and connect to it, opening a repl."
  (interactive)
  (slime-js-run-swank)
  (sleep-for 1)
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))



(defun start-local-server ()
  "Start local static-node server"
  (interactive)
  ;;(shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
  ;;                       " | sudo -S your_command_here"))
  (shell "local-server")
  (comint-send-string "local-server" start-local-cmd)
  (comint-send-input)
  (switch-to-buffer "*scratch*"))

(defun slime-js-jack-in-browser ()
  "Start a swank-js server, connect to it, open a repl, open a browser, connect to that."
  (interactive)
  (start-local-server)
  (sleep-for 5)
  (slime-js-jack-in-node)
  (sleep-for 2)
  (slime-js-set-target-url slime-js-target-url)
  (shell-command (concat slime-js-browser-command " " slime-js-connect-url slime-js-starting-url))
  (sleep-for 5)
  (setq slime-remote-history nil)
  (slime-js-sticky-select-remote (caadr (slime-eval '(js:list-remotes))))
  (setq slime-js-browser-jacked-in-p t)
  (global-set-key [f5] 'slime-js-reload))

(defadvice save-buffer (after save-css-buffer activate)
  (when (and slime-js-browser-jacked-in-p (eq major-mode 'css-mode))
    (slime-js-refresh-css)))

(defadvice save-buffer (after save-js-buffer activate)
  (when (and slime-js-browser-jacked-in-p (eq major-mode 'js2-mode))
    (slime-js-reload)))

(defun js2-eval-friendly-node-p (n)
  (or (and (js2-stmt-node-p n) (not (js2-block-node-p n)))
      (and (js2-function-node-p n) (js2-function-node-name n))))

(defun slime-js--echo-result (result &rest _)
  (message result))

(defun slime-js--replace-with-result (replacement beg end)
  (save-excursion
    (goto-char beg)
    (delete-char (- end beg))
    (insert replacement)))

(defun slime-js-eval-region (beg end &optional func)
  (lexical-let ((func (or func 'slime-js--echo-result))
                (beg beg)
                (end end))
    (slime-flash-region beg end)
    (slime-js-eval
     (buffer-substring-no-properties beg end)
     #'(lambda (s) (funcall func (cadr s) beg end)))))

(defun slime-js-eval-statement (&optional func)
  (let ((node (js2r--closest 'js2-eval-friendly-node-p)))
    (slime-js-eval-region (js2-node-abs-pos node)
                          (js2-node-abs-end node)
                          func)))

(defun slime-js-eval-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark))
    (slime-js-eval-statement)))

(defun slime-js-eval-and-replace-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark) 'slime-js--replace-with-result)
    (slime-js-eval-statement 'slime-js--replace-with-result)))

(define-key slime-js-minor-mode-map (kbd "C-x C-e") 'slime-js-eval-current)
(define-key slime-js-minor-mode-map (kbd "C-c C-e") 'slime-js-eval-and-replace-current)

;; Remove slime-minor-mode from mode line if diminish.el is installed
(when (boundp 'diminish)
  (diminish 'slime-js-minor-mode))

(provide 'setup-slime-js)
