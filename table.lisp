;;; Parse table
(defun parse-table (table-data table-name)
"Parse table with passed data and name."
	(cond
		((ends-with-str table-name "tsv") (parse-table-separator table-data #\tab #\"))
		((ends-with-str table-name "csv") (parse-table-separator table-data #\, #\"))
		(t (error "Not supported file extension. File name: ~S" table-name))))

(defun parse-table-separator (table-data separator quote-char)
"Parse table with passed separator and qoute character."
	(setq table-parsed (mapcar
		#'(lambda (row) (split-row row separator quote-char))
		table-data))
	(cons
		(get-first-row table-parsed)
		(cdr table-parsed)))

(defun pretty-table-print (table)
"Pretty table output to stdout."
	(mapcar
		#'(lambda (row)
			(mapcar
				#'(lambda (val)
					(format t "~15A " val))
				row)
			(format t "~C" #\linefeed))
		table))

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

(defun get-first-row (table-parsed)
	(setq
		max-column-lengths (get-max-column-lengths (cdr table-parsed)))
	(get-cons-list
		(car table-parsed)
		max-column-lengths))

(defun get-max-column-lengths (rows &optional (lengths '()))
	(setq
		row (car rows)
		column-lengths
			(mapcar
				#'(lambda (column)
					(length column))
				row)
		selected-max-lengths
			(get-selected-max-number-list
				column-lengths
				lengths))
	(cond
		((cdr rows)
			(get-max-column-lengths (cdr rows) selected-max-lengths))
		(t selected-max-lengths)))