;;; ASDF build system
(require 'asdf)

;;; Third-party lib for parsing .csv .tsv tables
(load "libs/cl-simple-table/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

;;; Require modules
(load "files.lisp")
(load "table.lisp")
(load "helpers.lisp")

;;; Tables pathes
(setq table-base-path "tables/")

;;; Function that parses command name
;;; (takes substring before open parenthesis)
(defun parse-command (query)
	(setq index (search "(" query))
	(cond
		((string= (substring query 0 index) "load") "load")
		((string= (substring query 0 index) "exit") "exit")
		(t (error "Command is not supported: ~S" query))))

;;; Function that parses parameter string
;;; (it doesn't parses separate parameters,
;;; only returns the whole string)
(defun get-params-string (query)
	(setq
		paramStartIndex (+ (search "(" query) 2)
		paramEndIndex (- (search ")" query) 1)
		param (substring query paramStartIndex paramEndIndex))
	param)

;;; Function that executes a command
(defun execute-command (command query)
	(cond
		((string= command "load")
			(setq
				table-name (get-params-string query)
				table-path (concatenate 'string table-base-path table-name)
				;table (simple-table:read-csv table-path)
				table (simple-table:read-csv "map_zal-skl9.csv" t)
				#| input-stream (open table-path)
				table-data (read-file-by-lines input-stream)
				table (parse-table table-data) |#)
			(print table)
			#| (pretty-table-print table) |#)
		((string= command "exit") (exit))
		(t (error "Command is not supported: ~S" query))))