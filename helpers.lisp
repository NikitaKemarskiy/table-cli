(defun ends-with-str (str1 str2)
	(setq match (mismatch str2 str1 :from-end t))
	(or (not match) (= 0 match)))

(defun split-row (row separator quote-char &optional (result '()) (index 0))
	(setq
		column-end-index
			(cond
				((= index (length row)) nil)
				(t (position separator row :test #'equal :start index)))
		column (subseq row index column-end-index))
	(cond
		(column-end-index
			(split-row
				row
				separator
				quote-char
				(append result (list column))
				(+ column-end-index 1)))
		(t result)))