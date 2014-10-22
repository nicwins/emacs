;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
		     '(("add" "t.$2 :$1\n$0" "add-column-migration" nil nil nil nil "direct-keybinding" nil)
		       ("create" "create_table :$1 do |t|\n  $0\n\n  t.timestamps\nend\n" "create table migrate snippet" nil
			("custom")
			nil nil "direct-keybinding" nil)
		       ("vars" "rename_column :vars, :$1, :$2\nrename_column :vars, :$3, :$4\nrename_column :vars, :$5, :$6\nrename_column :vars, :$7, :$8\nrename_column :vars, :$9, :$10\nrename_column :vars, :$11, :$12\nrename_column :vars, :$13, :$14\nrename_column :vars, :$15, :$16\nrename_column :vars, :$17, :$18\nrename_column :vars, :$19, :$20\nrename_column :vars, :$21, :$22\n" "vars temp migration snippet" nil
			("custom")
			nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Thu Oct  2 14:07:54 2014
