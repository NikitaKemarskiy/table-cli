;;; Require modules
(load "files.lisp")
(load "table.lisp")
(load "queries.lisp")
(load "helpers.lisp")
(load "select/select.lisp")
(load "select/join.lisp")

;;; Function that parses command name
;;; (takes substring before open parenthesis)
(defun parse-command (query)
	(setq index (search " " query))
	(cond
		((string-equal (substring query 0 index) "select") "select")
		((string-equal (substring query 0 index) "exit") "exit")
		(t (error "Command is not supported: ~S" query))))

;;; Function that executes a command
(defun execute-command (command query)
	(cond
		((string-equal command "select")
			(setq
				words
					(split-str query)
				columns
					(get-select-columns words)
				from-clause
					(get-select-from-clause words)
				where-clause
					(get-select-where-clause words)
				order-by-clause
					(get-select-order-by-clause words))
			(cond
				((= (length from-clause) 1)
					(setq
						table-parsed
							(get-table from-clause)
						table-selected
							(select-table
								table-parsed
								columns
								where-clause
								order-by-clause)))
				(t (setq
					t1 (nth 0 from-clause)
					t2 (nth 3 from-clause)
					join-clause (nth 1 from-clause)
					column-name-joined (nth 5 from-clause))
					(setq
						table-parsed-1
							(get-table (cons t1 nil))
						table-parsed-2
							(get-table (cons t2 nil))
						table-selected-1
							(select-table
								table-parsed-1
								columns
								where-clause
								order-by-clause)
						table-selected-2
							(select-table
								table-parsed-2
								columns
								where-clause
								order-by-clause)
						table-selected
							(join-tables
								table-selected-1
								table-selected-2
								column-name-joined
								join-clause))))
			(pretty-table-print table-selected))
		((string-equal command "exit") (exit))
		(t (error "Command is not supported: ~S" query))))