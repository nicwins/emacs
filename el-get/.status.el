((auto-complete status "installed" recipe
								(:name auto-complete :website "http://auto-complete.org/" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
											 (popup fuzzy)))
 (auto-indent status "installed" recipe
							(:name auto-indent :type git :url "https://github.com/mlf176f2/auto-indent-mode.el.git" :features auto-indent-mode :after
										 (progn
											 (setq auto-indent-on-visit-file t)
											 (auto-indent-global-mode))))
 (autopair status "installed" recipe
					 (:name autopair :type git :url "git://github.com/capitaomorte/autopair.git" :features autopair :after
									(progn
										(add-hook 'sldb-mode-hook
															#'(lambda nil
																	(setq autopair-dont-activate t)))
										(autopair-global-mode))))
 (buffer-move status "installed" recipe
							(:name buffer-move :description "Swap buffers without typing C-x b on each window" :type emacswiki :features buffer-move :after
										 (progn
											 (global-set-key
												(kbd "<C-S-up>")
												'buf-move-up)
											 (global-set-key
												(kbd "<C-S-down>")
												'buf-move-down)
											 (global-set-key
												(kbd "<C-S-left>")
												'buf-move-left)
											 (global-set-key
												(kbd "<C-S-right>")
												'buf-move-right))))
 (diminish status "installed" recipe
					 (:name diminish :description "An Emacs package that diminishes the amount of space taken on the mode line by the names of minor modes." :type http :url "http://www.eskimo.com/~seldon/diminish.el" :features diminish))
 (dired-details status "installed" recipe
								(:name dired-details :description "Make file details hide-able in dired" :type emacswiki :features dired-details))
 (dired-details+ status "removed" recipe nil)
 (el-get status "installed" recipe
				 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (emmet-mode status "installed" recipe
						 (:name emmet-mode :type git :url "git://github.com/smihica/emmet-mode.git" :features emmet-mode :after
										(progn
											(add-hook 'sgml-mode-hook 'emmet-mode)
											(add-hook 'css-mode-hook 'emmet-mode)
											(add-hook 'emmet-mode-hook
																(lambda nil
																	(setq emmet-indentation 2)))
											(eval-after-load 'emmet-mode
												'(progn
													 (define-key emmet-mode-keymap
														 (kbd "C-j")
														 nil)
													 (define-key emmet-mode-keymap
														 (kbd "<C-return>")
														 nil)
													 (define-key emmet-mode-keymap
														 (kbd "C-c C-j")
														 'emmet-expand-line)
													 (diminish 'emmet-mode))))))
 (fuzzy status "installed" recipe
				(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (goto-last-change status "installed" recipe
									 (:name goto-last-change :description "Move point through buffer-undo-list positions" :type emacswiki :load "goto-last-change.el" :after
													(progn
														(global-set-key
														 (kbd "C-x C-/")
														 'goto-last-change))))
 (ido-ubiquitous status "installed" recipe
								 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :type elpa))
 (magit status "installed" recipe
				(:name magit :type git :url "git://github.com/magit/magit.git" :features magit :after
							 (progn
								 (global-set-key
									(kbd "C-x C-z")
									'magit-status))))
 (package status "installed" recipe
					(:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin 24 :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
								 (progn
									 (setq package-user-dir
												 (expand-file-name
													(convert-standard-filename
													 (concat
														(file-name-as-directory default-directory)
														"elpa")))
												 package-directory-list
												 (list
													(file-name-as-directory package-user-dir)
													"/usr/share/emacs/site-lisp/elpa/"))
									 (make-directory package-user-dir t)
									 (unless
											 (boundp 'package-subdirectory-regexp)
										 (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
									 (setq package-archives
												 '(("ELPA" . "http://tromey.com/elpa/")
													 ("gnu" . "http://elpa.gnu.org/packages/")
													 ("marmalade" . "http://marmalade-repo.org/packages/")
													 ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (popup status "installed" recipe
				(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (slime-js status "installed" recipe
					 (:name slime-js :type git :url "git://github.com/Gozala/slime-js.git" :features slime-js))
 (smex status "installed" recipe
			 (:name smex :type git :url "git://github.com/nonsequitur/smex.git" :features smex :after
							(progn
								(setq smex-save-file "~/.emacs.d/backups/.smex-items")
								(global-set-key
								 (kbd "M-x")
								 'smex)
								(global-set-key
								 (kbd "M-X")
								 'smex-major-mode-commands)
								(smex-initialize))))
 (smooth-scrolling status "installed" recipe
									 (:name smooth-scrolling :type git :url "git://github.com/aspiers/smooth-scrolling.git" :features smooth-scrolling))
 (undo-tree status "installed" recipe
						(:name undo-tree :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :features undo-tree :after
									 (progn
										 (global-undo-tree-mode)
										 (setq undo-tree-mode-lighter "")))))
