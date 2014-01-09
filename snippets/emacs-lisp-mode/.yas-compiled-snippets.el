;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
		     '(("neb" "\n;;; ${1:Package Name} --- ${2:Summary}\n\n;;; Commentary:\n;; $3\n\n;;; Code:\n\n$0\n\n(provide '$1)\n;;; $1.el ends here" "new-el-boilerplate" nil nil nil nil "C-c C-n" nil)))


;;; Do not edit! File generated at Wed Jan  8 19:09:52 2014
