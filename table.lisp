;;; Parse table
(defun parse-table (table-data table-name)
	(cond
		((ends-with-str table-name "tsv") (parse-table-separator table-data #\tab #\"))
		((ends-with-str table-name "csv") (parse-table-separator table-data #\, #\"))
		(t (error "Not supported file extension. File name: ~S" table-name))))

;;; Parse table with passed separator
(defun parse-table-separator (table-data separator quote-char)
	(mapcar
		#'(lambda (row) (split-row row separator quote-char))
		table-data))

;;; Pretty table output to stdout
(defun pretty-table-print (table)
	(mapcar
		#'(lambda (row)
			(mapcar
				#'(lambda (val)
					(format t "~15A " val))
				row)
			(format t "~C" #\linefeed))
		table))