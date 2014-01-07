(require 'comint)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))


(defun grunt-init()
  "Starts the grunt server and grunt test watcher"
  (interactive)
  (shell "**local-server**")
  (comint-send-string "**local-server**" "cd ~/projects/dashboard; grunt server")
  (comint-send-input)
  (shell "**TEST**")
  (comint-send-string "**TEST**" "cd ~/projects/dashboard; grunt test")
  (comint-send-input)
  (switch-to-buffer "**TEST**"))

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

;; tar --exclude='dashboard/node_modules' --exclude='dashboard/app/bower_components' --exclude='dashboard/.git' -zcvf dashboard.tgz dashboard


(provide 'helpers)
