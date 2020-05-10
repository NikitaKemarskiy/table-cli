;;; Require modules
(load "files.lisp")
(load "table.lisp")
(load "queries.lisp")
(load "helpers.lisp")
(load "select/select.lisp")

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
				table-name
					(get-select-table-name words)
				where-clause
					(get-select-where-clause words)
				order-by-clause
					(get-select-order-by-clause words)
				table-parsed
					(get-table table-name)
				table-selected
					(select-table
						table-parsed
						columns
						where-clause
						order-by-clause))
			(pretty-table-print table-selected))
		((string-equal command "exit") (exit))
		(t (error "Command is not supported: ~S" query))))