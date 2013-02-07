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

(defun jsnow-do-script (filename)
  "Quickly jump into development of a new js script.
Prompts the user for the FILENAME. It's expected that the user will not usually run this directly.
See the wrapper function: \\[jsnow-script]"
  (interactive
   (jsnow-prompt-user-for-file-to-create
    "Name for the new js script? " jsnow-script-location))
  (require 'template)
  (jsnow-create-with-template filename jsnow-js-script-template)
  (jsnow-change-mode-to-executable))

(defun jsnow-prompt-user-for-file-to-create (ask-mess default-location)
  "Ask for the name of the file to create.
Check to see if one exists already, and if so, ask for another name.
Asks the question ASK-MESS, and defaults to using the location DEFAULT-LOCATION.
Returns a list of single string, full file name with path."
  (let ( filename )
    (setq default-location (file-name-as-directory default-location))
    (while (progn
             (setq filename
                   (expand-file-name
                    (read-file-name ask-mess default-location)))
             (setq ask-mess
                   "That name is already in use, please use another name: ")
             (file-exists-p filename)))
    (list filename)))

(defun jsnow-widget-two-questions-stub (inc-spot package-name)
  "Quickly jump into development of a new js widget.
Asks the user two questions to get the INC-SPOT and the PACKAGE-NAME"
  (interactive
   (let ((default-directory jsnow-widget-location))
     (call-interactively 'jsnow-prompt-for-widget-to-create)))
  )
