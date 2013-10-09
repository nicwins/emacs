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
  (switch-to-buffer "**TEST**")
  )

(provide 'setup-grunt)
