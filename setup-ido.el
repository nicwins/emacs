;; Interactively Do Things
(require 'ido)
(ido-mode 'both)
(setq ido-enable-pdrefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/backups/ido.last"
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#"
        "\\`\\.\\./" "\\`\\./")
      ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
        "_region_" " output\\*$" "^TAGS$" "^\*Ido"))

;(defun ido-ignore-crap-buffers (name)
;  "Ignore the crap buffers"
;  (string= name "*Messages*"))

;(setq ido-ignore-buffers '(ido-ignore-crap-buffers))


(add-hook
 'ido-setup-hook
 #'(lambda ()

     ;; Use C-w to go back up a dir to better match normal usage of C-w
     ;; - insert current file name with C-x C-w instead.
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
     (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Enable ido-ubiquitous
;cond
;;; New version
;((fboundp 'ido-ubiquitous)
; (ido-ubiquitous 1))
;;; Old version
;((boundp 'ido-ubiquitous-enabled)
; ;; Probably not required, since it is enabled by default
; (setq ido-ubiquitous-enabled t)))
;
;defvar ido-cur-item nil)
;defvar ido-default-item nil)
;defvar ido-cur-list nil)

(provide 'setup-ido)
