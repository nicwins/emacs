(require 'comint)

(defun grunt-init()
  "Starts the grunt server and grunt test watcher"
  (interactive)
  (shell "**local-server**")
  (comint-send-string "**local-server**" "cd /home/winsln/projects/dashboard; grunt server")
  (comint-send-input)
  (shell "**TEST**")
  (comint-send-string "**TEST**" "cd /home/winsln/projects/dashboard; grunt test")
  (comint-send-input)
  (switch-to-buffer "**TEST**"))
                                        ;(defun clean-winsight-xml ()
                                        ;  "Remove illegal characters from winsight xml"
                                        ;  (interactive)
                                        ;  (revert-buffer t t)
                                        ;  (goto-char (point-min))
                                        ;  (while (search-forward "&nbsp;" nil t)
                                        ;    (replace-match "" nil t))
                                        ;  (goto-char (point-min))
                                        ;  (while (search-forward "&" nil t)
                                        ;    (replace-match "&amp;"))
                                        ;  (goto-char (point-min))
                                        ;  (save-buffer))

(defun startup-rails-env ()
  "Start apache, mysqld, and rails"
  (interactive)
  ; (save-window-excursion
  (let ((buf (generate-new-buffer "mysql")))
    (set-buffer buf)
    (cd "/sudo::/")
    (async-shell-command "service mysqld start" nil buf)
    )
  (let ((buf (generate-new-buffer "apache")))
    (set-buffer buf)
    (cd "/sudo::/")
    (async-shell-command "service httpd restart" buf)
    )
  (let ((buf (generate-new-buffer "solr")))
    (set-buffer buf)
    (cd "/var/www/html/rails")
    (async-shell-command "bundle exec rake sunspot:solr:start" buf)
    )
  (shell "**RAILS**")
  (comint-send-string "**RAILS**" "cd /var/www/html/rails; bundle exec rails s" )
  (comint-send-input)
  (switch-to-buffer "**RAILS**")
  )
  ;;  (message "hello")
  ;;  (cd "/var/www/html/rails")
  ;;  (call-process "bundle" nil buf "exec" "rake" "sunspot:solr:start")
  ;;  (shell buf)
  ;;  (comint-send-string buf "cd /var/www/html/rails; bundle exec rails s" )
  ;;  (comint-send-input)
  ;;  (switch-to-buffer buf)
  ;;  ))

                                        ;
                                        ; (let ((buf (generate-new-buffer "async2")))
                                        ;   (cd "/sudo::/")
                                        ;   (async-shell-command "service httpd restart" buf)
                                        ;   ))
;;(shell-command "cd /var/www/html/rails && bundle exec rake sunspot:solr:start")

;;  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
;;                       " | sudo -S service httpd restart"))
;;

;;


(provide 'helpers)
