;;; Require modules
(load "helpers.lisp")

;;; Function that returns list of columns
;;; specified in SELECT statement
(defun get-select-columns (words)
	(get-words-between words "select" "from"))

;;; Function that returns table name
;;; specified in SELECT statement
(defun get-select-table-name (words &optional (started nil))
	(get-words-between words "from" "where"))

;;; Function that returns where clause
;;; words
(defun get-select-where-clause (words &optional (started nil))
	(get-words-between words "where" "order"))

;;; Function that returns order by
;;; clause columns
(defun get-select-order-by-clause (words &optional (started nil))
	(cdr (get-words-between words "order")))

(defun select-table (table-parsed columns where-clause order-by-clause)
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
						column
						where-clause))
				columns-processed)
		table-distincted
			(cond
				(is-distinct
					(distinct-table table-selected))
				(t table-selected))
		table-ordered
			(cond
				((null order-by-clause)
					table-distincted)
				(t
					(order-table
						table-distincted
						order-by-clause))))
	table-ordered)

(defun select-table-get-column (table-parsed column &optional (where-clause nil))
	(setq
		column-parsed (car table-parsed)
		column-name (car (cdr column-parsed)))
	(cond
		((string-equal column-name column)
			(cond
				((null where-clause)
					column-parsed)
				((string-equal (car where-clause) column)
					(filter-column-where-clause
						column-parsed
						where-clause))
				(t column-parsed)))
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

(defun order-table (table-selected order-by-clause)
	(setq
		order-by-column
			(car order-by-clause)
		order-by-order
			(car (cdr order-by-clause)))
	(setf
		(symbol-function 'order-by-function)
			(cond
				((string= order-by-order "desc")
					#'compare>)
				(t #'compare<)))
	(setq
		order-by-column-index
			(get-column-index-by-name
				table-selected
				(car order-by-clause))
		table-selected-values
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected)
		table-selected-values-rows-sorted
			(sort
				(transpose-table table-selected-values)
				#'(lambda (list1 list2)
					(setq
						item1 (nth order-by-column-index list1)
						item2 (nth order-by-column-index list2))
					(order-by-function item1 item2)))
		table-selected-values-result
			(transpose-table table-selected-values-rows-sorted))
	(mapcar
		#'(lambda (column values)
			(append
				(list (car column) (car (cdr column)))
				values))
		table-selected
		table-selected-values-result))

(defun filter-column-where-clause (column-parsed where-clause)
	(setq
		column-values
			(cdr (cdr column-parsed))
		first-value
			(car column-values)
		operator
			(car (cdr where-clause))
		compare-value
			(car (cdr (cdr where-clause)))
		compare-value-converted
			(cond
				((numberp first-value)
					(parse-integer compare-value))
				(t compare-value))
		where-function
			(cond
				((string-equal operator "<=")
					#'compare<=)
				((string-equal operator "<>")
					#'compare/=))
		filtered-values
			(filter-by-predicate
				column-values
				#'(lambda (value)
					(funcall where-function value compare-value-converted))))
	(cons
		(car column-parsed)
		(cons
			(car (cdr column-parsed))
			filtered-values)))