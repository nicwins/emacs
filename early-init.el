;;; early-init --- Early Load Setups -*- lexical-binding t -*-

;;; Commentary:

;;; Code:

;; Defer Garbage Collection
(setq gc-cons-threshold 100000000) ;; 100mb

;; Increase size of data Emacs reads from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; We will handle our own package init
(setq package-enable-at-startup nil)

;; Disable file handling during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable site-wide configs
(setq site-run-file nil)

(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
	        (lambda ()
	          (setq gc-cons-threshold better-gc-cons-threshold)
	          (setq file-name-handler-alist file-name-handler-alist-original)
	          (makunbound 'file-name-handler-alist-original)))

(setenv "LSP_USE_PLISTS" "true")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
