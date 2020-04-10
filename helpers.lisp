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

(defun transpose-table (table)
	(apply
		#'mapcar
		(lambda (&rest lst) lst)
		table))

(defun get-selected-max-number-list (list1 list2)
	(cond
		((and list1 list2)
			(cons
				(max (car list1) (car list2))
				(get-selected-max-number-list
					(cdr list1)
					(cdr list2))))
		(list1 list1)
		(list2 list2)
		(t nil)))

(defun get-cons-list (list1 list2)
	(cond
		((and list1 list2)
			(cons
				(cons
					(car list1)
					(car list2))
				(get-cons-list
					(cdr list1)
					(cdr list2))))
		(t nil)))

(defun remove-empty-strings (lst)
	(setq first-string (car lst))
	(cond
		((null first-string)
			nil)
		((string= "" first-string)
			(remove-empty-strings
				(cdr lst)))
		(t
			(cons
				first-string
				(remove-empty-strings (cdr lst))))))

(defun split-str (string &optional (separator " ") (r nil))
	(let ((n (position separator string
		:from-end t
		:test #'(lambda (x y)
		(find y x :test #'string=)))))
	(if n
		(split-str (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
		(cons string r))))