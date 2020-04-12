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
		is-distinct
			(string-equal
				(car columns)
				"distinct")
		columns-processed
			(replace-item-with-several
				(cond
					(is-distinct (cdr columns))
					(t columns))
				"*"
				table-columns-names)
		table-selected
			(mapcar
				#'(lambda (column)
					(select-table-get-column
						table-parsed
						column))
				columns-processed))
	(cond
		(is-distinct
			(distinct-table table-selected))
		(t table-selected)))

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

(defun distinct-table (table-selected)
	(setq
		table-selected-values
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected)
		table-selected-values-rows-sorted
			(sort
				(transpose-table table-selected-values)
				#'compare-two-lists)
		table-selected-values-distinct
			(remove-item-duplicates table-selected-values-rows-sorted)
		table-selected-values-result
			(transpose-table table-selected-values-distinct))
	(mapcar
		#'(lambda (column values)
			(append
				(list (car column) (car (cdr column)))
				values))
		table-selected
		table-selected-values-result))