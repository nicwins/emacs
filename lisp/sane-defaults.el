;;; sane-defaults --- Sets various emacs default settings

;;; Commentary:
;; cribbed from magnars/.emacs.d

;;; Code:

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" (concat dotfiles-dir "backups")))


;; Show lambda please
(global-prettify-symbols-mode 1)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Cua Mode
(cua-mode t)

;; Emacs Autosave Settings
(setq auto-save-list-file-prefix "~/.emacs.d/backups/.saves/")
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/.saves/" t)))

;; Emacs Backup Settings
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 `((".*" . "~/.emacs.d/backups/.saves/")) ; don't litter fs

 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

;; Tramp litter
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory "~/.emacs.d/backups/.saves/")

;; Eshell litter
(setq eshell-directory-name "~/.emacs.d/backups/eshell/")

;; Emacs Interlock
(setq create-lockfiles nil)

;; Shift left, up, down, right to swap buffers
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about buffers with processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(global-linum-mode 1)
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(setq recentf-exclude '("\.recentf"))
(setq recentf-save-file (expand-file-name "~/.emacs.d/backups/.recentf" user-emacs-directory))
(setq recentf-max-saved-items 50) ;; just 20 is too recent
(setq recentf-auto-cleanup 300)
(setq recentf-auto-save-timer
      (run-with-idle-timer 300 t 'recentf-save-list))
(recentf-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(setq tab-width 2)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to 0 (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
"When popping the mark, continue popping until the cursor moves.
Also, if the last command was a copy - skip past all the
expand-region cruft.:"
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Seed the random-number generator
(random t)

;; Whitespace
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Whitespace-helpers
(defun untabify-buffer ()
"Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
"Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook', and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Newline after inserting closing tag in html-mode
(defadvice sgml-close-tag (after close-tag-then-newline activate)
"Add a newline after inserting a closing tag in 'html-mode'."
  (newline-and-indent))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; full screen
(defun fullscreen ()
"Set Emacs to fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; Set js-mode indent to 2 for json files
(setq js-indent-level 2)

;; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

(provide 'sane-defaults)
;;; sane-defaults.el ends here
