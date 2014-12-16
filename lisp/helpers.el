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

(defun tar-compress-dashboard ()
  "Tars up the dashboard project."
  (interactive)
  (shell "*tar*")
  (switch-to-buffer "*tar*")
  (comint-send-string "*tar*" "tar --exclude='/var/www/evms-dashboard/log' --exclude='/var/www/evms-dashboard/solr/data' --exclude='/var/www/evms-dashboard/tmp' --exclude='/var/www/evms-dashboard/uploads' --exclude='/var/www/evms-dashboard/.git'  --exclude='/var/www/evms-dashboard/public/client/app/bower_components' --exclude='/var/www/evms-dashboard/public/client/node_modules' -zcvf rails.tgz /var/www/evms-dashboard")
  (comint-send-input))

(defun grunt-server()
  "Starts the grunt server."
  (interactive)
  (shell "**GULP**")
  (comint-send-string "**GULP**" "cd ~/projects/evms-dashboard/public/client; gulp")
  (comint-send-input))

(defun rails-server()
  "Starts rails"
  (interactive)
  (shell "**RAILS**")
  (comint-send-string "**RAILS**" "cd ~/projects/evms-dashboard/; rails s")
  (comint-send-input))

(defun startup-evms-dashboard ()
  "Start rails server and grunt watcher."
  (interactive)
  (grunt-server)
  (rails-server))

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
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)

(provide 'helpers)
;;; helpers.el ends here
