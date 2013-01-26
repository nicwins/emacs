;; Inserts the path to a buffer
(defun my-process-file (fPath)
  "Process the file at fullpath FPATH."

  (setq directory (substring (file-name-sans-extension fPath) (length webroot)))
  (setq directory (car (last (split-string directory "/"))))

  (insert (concat "'" directory "' : '/js" (substring (file-name-sans-extension fPath) (length webroot)) "',\n")) )


(defun update-main-js ()
  "Updates main.js pathing"
  (interactive)
  (save-excursion
    (let (filePath pathingBuf webroot pathingFileName)
      ;; full path to to project root
      (setq webroot "C:/home/nic/projects/Dashboard/js")

      ;; file name of pathing file, relative to webroot, no extension
      (setq pathingFileName "/main")

      ;; filePath is the full path the sitemap file
      ;; pathingBuf is the buffer of the sitemap file


      (setq filePath (concat webroot pathingFileName ".js"))

      ;; open file and save a handle to the buffer
      (setq pathingBuf (find-file-noselect filePath))
      (set-buffer pathingBuf)

      ;; delete old contents

      (goto-char (point-min))
      (search-forward "Begin-hook-auto-generation" nil t)
      (forward-line 1)
      (delete-region (point)
                     (progn
                       (search-forward "End-hook-auto-generation" nil t)
                       (forward-line -1)
                       (point)) )

      ;; for each file in my site, insert its url
      (require 'find-lisp)
      (mapc
       (lambda (x) (my-process-file x))
       (find-lisp-find-files webroot ".*"))

      ;; remove trailing comma
      (search-backward ",")
      (delete-char 1)
      (save-buffer)

      )))
