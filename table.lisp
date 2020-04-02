;;; Parse table
(defun parse-table (table-data table-name)
"Parse table with passed data and name."
	(cond
		((ends-with-str table-name "tsv") (parse-table-separator table-data #\tab #\"))
		((ends-with-str table-name "csv") (parse-table-separator table-data #\, #\"))
		(t (error "Not supported file extension. File name: ~S" table-name))))

(defun parse-table-separator (table-data separator quote-char)
"Parse table with passed separator and qoute character."
	(setq
		table-parsed-by-rows
			(mapcar
				#'(lambda (row) (split-row row separator quote-char))
				table-data)
		table-parsed
			(calculate-columns-lengths
				(transpose-table
					table-parsed-by-rows)))
	table-parsed)

(defun pretty-table-print (table-parsed &optional (flag t))
"Pretty table output to stdout."
	(setq
		column-lengths
			(mapcar
				#'car
				table-parsed)
		table-parsed-recursive
			(mapcar
				#'(lambda(column)
					(setq
						column-length (car column)
						column-values (cdr column))
					(format t
						(concatenate 'string
							" ~"
							(write-to-string (+ column-length 1))
							"A|")
						(car column-values))
					(cond
						((cdr column-values)
							(cons
								column-length
								(cdr column-values)))
						(t nil)))
				table-parsed))
		(terpri)
		(cond (flag (print-separator-line column-lengths)))
		(cond
			((reduce
				#'(lambda (item1 item2)
					(or item1 item2))
				table-parsed-recursive)
				(pretty-table-print table-parsed-recursive nil))
			(t nil)))

(defun split-row (row separator quote-char &optional (result '()) (column-start-index 0))
"Splits row into list of columns values
(we pass separator and quote char as params)."
	(setq
		column-end-index
			(cond
				((= column-start-index (length row)) nil)
				(t (get-column-end-index row separator quote-char column-start-index)))
		column
			(parse-column
				(subseq row column-start-index column-end-index)
				quote-char)
		result-appended
			(append
				result
				(list column)))
	(cond
		(column-end-index
			(split-row
				row
				separator
				quote-char
				result-appended
				(+ column-end-index 1)))
		(t result-appended)))

(defun parse-column (column quote-char)
"If column isn't surrounded with quote chars - return column.
Otherwise quote chars will be removed, doubled quote chars inside
will be replaced with only one."
	(cond
		((and (> (length column) 1) (char= (char column 0) quote-char))
			(replace-all
				(subseq column 1 (- (length column) 1))
				(concatenate 'string (string quote-char) (string quote-char))
				(string quote-char)))
		(t column)))

(defun get-column-end-index (row separator quote-char column-start-index)
	(cond
		((char= (char row column-start-index) quote-char)
			(setq
				closing-quote-char-index
					(get-closing-quote-char-index
						row
						quote-char
						(+ column-start-index 1)))
			(+ closing-quote-char-index 1))
		(t (position separator row :test #'equal :start column-start-index))))

(defun get-closing-quote-char-index (row quote-char opening-quote-char-index)
	(setq
		quote-char-index
			(position quote-char row :test #'equal :start (+ opening-quote-char-index 1)))
	(cond
		((and
			(not (null quote-char-index))
			(< quote-char-index (- (length row) 1))
			(char= (char row (+ quote-char-index 1)) quote-char))
				(get-closing-quote-char-index
					row
					quote-char
					(+ quote-char-index 1)))
		(t quote-char-index)))

(defun calculate-columns-lengths (table-parsed)
	(setq
		max-column-lengths
			(get-max-column-lengths table-parsed))
	(mapcar
		#'cons
		max-column-lengths
		table-parsed))

(defun get-max-column-lengths (columns)
	(setq
		column (car columns)
		column-lengths
			(mapcar
				#'length
				column)
		max-column-length (apply #'max column-lengths))
	(cond
		((cdr columns)
			(cons max-column-length (get-max-column-lengths (cdr columns))))
		(t (cons max-column-length nil))))

(defun print-separator-line (column-lengths)
	(mapcar
		#'(lambda (column)
			(format t "~v@{~A~:*~}" (+ column 2) #\-)
			(format t "+"))
		column-lengths)
	(terpri))