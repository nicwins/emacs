;;; helpers --- My defuns and associated keybindings

;;; Commentary:
;; This contains my custom defuns and their associated key-bindings. Might want to separate this
;; into those needed for specific projects vs those that are nice for Emacs itself.

;;; Code:

(require 'comint)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun grunt-server()
  "Starts the grunt server"
  (interactive)
  (shell "**local-server**")
  (comint-send-string "**local-server**" "cd ~/projects/dashboard; grunt server")
  (comint-send-input))

(defun grunt-test()
  "Starts just the grunt test process"
  (interactive)
  (shell "**TEST**")
  (comint-send-string "**TEST**" "cd ~/projects/dashboard; grunt test")
  (comint-send-input)
  (switch-to-buffer "**TEST**"))

(defun grunt-init ()
  "Starts the grunt server and test watcher"
  (interactive)
  (grunt-server)
  (grunt-test))

(defun tar-compress-dashboard ()
  "Tars up the dashboard project"
  (interactive)
  (shell "*tar*")
  (switch-to-buffer "*tar*")
  (comint-send-string "*tar*" "cd ~/projects/; tar --exclude='dashboard/node_modules' --exclude='dashboard/app/bower_components' --exclude='dashboard/coverage' --exclude='dashboard/.git' -zcvf dashboard.tgz dashboard")
  (comint-send-input))

;; tar --exclude='dashboard/node_modules' --exclude='dashboard/app/bower_components' --exclude="dashboard/coverage" --exclude="dashboard/.git" -zcvf dashboard.tgz dashboard

(defun startup-rails-env ()
  "Start apache, mysqld, and rails"
  (interactive)
                                        ; (save-window-excursion
  (let ((buf (generate-new-buffer "mysql")))
    (set-buffer buf)
    (cd "/sudo::/")
    (async-shell-command "service mysqld start" nil buf))
  (let ((buf (generate-new-buffer "apache")))
    (set-buffer buf)
    (cd "/sudo::/")
    (async-shell-command "service httpd restart" buf))
  (let ((buf (generate-new-buffer "solr")))
    (set-buffer buf)
    (cd "/var/www/html/rails")
    (async-shell-command "bundle exec rake sunspot:solr:start" buf))
  (shell "**RAILS**")
  (comint-send-string "**RAILS**" "cd /var/www/html/rails; bundle exec rails s thin" )
  (comint-send-input)
  (switch-to-buffer "**RAILS**"))

;; Prod -- RAILS_ENV='production' bundle exec unicorn -D -c ./config/unicorn.rb

;; Dev sudo /etc/init.d/avahi-daemon stop

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)

(provide 'helpers)
;;; helpers.el ends here
