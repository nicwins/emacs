;;; setup-package --- provides an automated packing install with MELPA

;;; Commentary:
;;
;; Provides helpers to enable automated package install from a list of packages
;; with melpa.

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package delight)
(provide 'setup-package)
;;; setup-package.el ends here
