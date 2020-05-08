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
						column))
				columns-processed)
		table-filtered
			(cond
				((null where-clause)
					table-selected)
				(t (filter-table table-selected where-clause)))
		table-distincted
			(cond
				(is-distinct
					(distinct-table table-filtered))
				(t table-filtered))
		table-ordered
			(cond
				((null order-by-clause)
					table-distincted)
				(t
					(order-table
						table-distincted
						order-by-clause))))
	table-ordered)

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

(defun filter-table (table-selected where-clause &optional (prev-table nil))
	(setq
		table-selected-values
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected)
		table-selected-values-rows
			(transpose-table table-selected-values)
		not-clause
			(string= (car where-clause) "not")
		where-clause-tokens
			(cond
				(not-clause (cdr where-clause))
				(t where-clause))
		column-name
			(car where-clause-tokens)
		operator
			(car (cdr where-clause-tokens))
		compare-value
			(car (cdr (cdr where-clause-tokens)))
		column-index
			(get-column-index-by-name table-selected column-name)
		first-value
			(nth column-index (car table-selected-values-rows))
		compare-value-converted
			(cond
				((numberp first-value)
					(parse-integer compare-value))
				(t compare-value))
		where-clause-remainder
			(cdr (cdr (cdr where-clause-tokens)))
		where-function
			(cond
				((string-equal operator "<=")
					#'(lambda (item1 item2)
						(cond
							(not-clause (not (compare<= item1 item2)))
							(t (compare<= item1 item2)))))
				((string-equal operator "<>")
					#'(lambda (item1 item2)
						(cond
							(not-clause (not (compare/= item1 item2)))
							(t (compare/= item1 item2)))))))
	(setq
		filtered-values-rows
			(filter-rows-by-predicate
				table-selected-values-rows
				column-index
				#'(lambda (value)
					(funcall where-function value compare-value-converted))))

	(setq
		filtered-values
			(transpose-table filtered-values-rows)
		table-filtered
			(mapcar
				#'(lambda (column values)
					(append
						(list (car column) (car (cdr column)))
						values))
				table-selected
				filtered-values))
	(cond
		((null where-clause-remainder) table-filtered)
		((string= (car where-clause-remainder) "and")
			(filter-table
				table-filtered
				(cdr where-clause-remainder)))
		((string= (car where-clause-remainder) "or")
			(filter-table
				table-filtered
				(cdr where-clause-remainder)
				table-selected))))