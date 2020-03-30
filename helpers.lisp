(defun ends-with-str (str1 str2)
	(setq match (mismatch str2 str1 :from-end t))
	(or (not match) (= 0 match)))

(defun replace-all (str original desired &optional (start-index 0))
	(setq
		original-index (search original str :start2 start-index))
	(print original-index)
	(cond
		(original-index
			(setq
				replaced-str
					(concatenate
						'string
						(subseq str 0 original-index)
						desired
						(subseq str (+ original-index (length original)))))
			(replace-all
				replaced-str
				original
				desired
				(+ original-index (length desired))))
		(t str)))

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