;;; setup-ruby-mode --- General Ruby Mode Settings
;;; Commentary:
;;    Helpful ruby defuns and settings

;;; Code:

;; avoid ridiculous ruby indentation
(setq ruby-deep-indent-paren nil)

(defun ruby--jump-to-test ()
  "Jump from file to test file."
  (find-file
   (replace-regexp-in-string
    "/lib/" "/test/"
    (replace-regexp-in-string
     "/\\([^/]+\\).rb$" "/test_\\1.rb"
     (buffer-file-name)))))

(defun ruby--jump-to-lib ()
  "Jump from test file to file."
  (find-file
   (replace-regexp-in-string
    "/test/" "/lib/"
    (replace-regexp-in-string
     "/test_\\([^/]+\\).rb$" "/\\1.rb"
     (buffer-file-name)))))

(defun ruby-jump-to-other ()
  "Toggle test and lib."
  (interactive)
  (if (string-match-p "/test/" (buffer-file-name))
      (ruby--jump-to-lib)
    (ruby--jump-to-test)))

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)

(define-key ruby-mode-map (kbd "C-c t") 'ruby-jump-to-other)

(provide 'setup-ruby-mode)
;;; setup-ruby-mode ends here
