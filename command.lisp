;;; Require modules
(load "files.lisp")
(load "table.lisp")
(load "queries.lisp")
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
				;; Table name
				table-name (get-params-string query)
				;; Table path
				table-path (concatenate 'string table-base-path table-name)
				;; Table input stream
				input-stream (open table-path)
				;; Table data (list of rows)
				table-data (read-file-by-lines input-stream)
				;; Table parsed (list of rows, rows are lists of cells)
				table-parsed (parse-table table-data table-name))
			(pretty-table-print table-parsed))
		((string= command "exit") (exit))
		(t (error "Command is not supported: ~S" query))))