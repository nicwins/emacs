(defun rails-start-local ()
  "Start local rails server"
  (interactive)
  (shell "local-server")
  (comint-send-string "local-server" "/etc/init.d/httpd restart")
  (comint-send-input)
  (comint-send-string "local-server" "/etc/init.d/mysqld start")
  (comint-send-input)
  (comint-send-string "local-server" "cd /var/www/rails/")
  (comint-send-input)
  (comint-send-string "local-server" "rails server")
  (comint-send-input)
  (switch-to-buffer "*scratch*"))

;; go to localhost no port
;; go to localhost 3000 for rails backend
;; Repos need to be pulled to var/www/html/
