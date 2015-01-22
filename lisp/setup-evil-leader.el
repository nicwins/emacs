(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(defun save-all () (interactive) (save-some-buffers t))

;; leader shortcuts
(evil-leader/set-key
  "w" 'save-buffer
  "W" 'save-all
  "q" 'kill-buffer-and-window
  "h" 'dired-jump
  "v" 'split-window-right
  "e" 'pp-eval-last-sexp
  "," 'other-window
  "b" 'ibuffer
  "x" 'helm-M-x
  "g" 'magit-status
  "G" 'magit-blame-mode
  "k" 'kill-this-buffer
  "K" 'kill-buffer
  "o" 'helm-occur
  "T" 'eshell)
  (provide 'setup-evil-leader)
