;;; Require modules
(load "helpers.lisp")

;;; Function that returns list of columns
;;; specified in SELECT statement
(defun get-select-columns (words &optional (started nil))
	(setq first-word (car words))
	(cond
		((null first-word)
			nil)
		((string-equal first-word "select")
			(get-select-columns (cdr words) T))
		((null started)
			(get-select-columns (cdr words)))
		((string-equal first-word "from")
			nil)
		(t
			(append
				(remove-empty-strings
					(split-str first-word ","))
				(get-select-columns
					(cdr words) T)))))

;;; Function that returns table name
;;; specified in SELECT statement
(defun get-select-table-name (words &optional (started nil))
	(setq first-word (car words))
	(cond
		((null first-word)
			nil)
		((string-equal first-word "from")
			(get-select-table-name (cdr words) T))
		((null started)
			(get-select-table-name (cdr words)))
		((string-equal first-word "where")
			nil)
		(t
			(append
				(remove-empty-strings
					(split-str first-word ","))
				(get-select-table-name
					(cdr words) T)))))