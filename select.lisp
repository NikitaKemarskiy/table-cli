;;; Require modules
(load "helpers.lisp")

;;; Function that returns list of columns
;;; specified in SELECT statement
(defun get-select-columns (words)
	(get-words-between words "select" "from"))

;;; Function that returns table name
;;; specified in SELECT statement
(defun get-select-table-name (words &optional (started nil))
	(get-words-between words "from"))

(defun select-table (table-parsed columns)
;replace-item-with-severa
	(setq
		table-columns-names
			(mapcar
				#'(lambda (column-parsed)
					(car (cdr column-parsed)))
				table-parsed)
		columns-star-replaced
			(replace-item-with-several
				columns
				"*"
				table-columns-names))
	(mapcar
		#'(lambda (column)
			(select-table-get-column
				table-parsed
				column))
		columns-star-replaced))

(defun select-table-get-column (table-parsed column)
	(setq
		column-parsed (car table-parsed)
		column-name (car (cdr column-parsed)))
	(cond
		((string-equal column-name column)
			column-parsed)
		((null (cdr table-parsed))
			(error "Column wasn't found: ~S" column))
		(t (select-table-get-column (cdr table-parsed) column))))