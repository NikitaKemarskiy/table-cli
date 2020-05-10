;;; Require modules
(load "helpers.lisp")
(load "select/order.lisp")
(load "select/where.lisp")

;;; Function that returns list of columns
;;; specified in SELECT statement
(defun get-select-columns (words)
	(get-words-between words "select" "from"))

;;; Function that returns table name
;;; specified in SELECT statement
(defun get-select-from-clause (words &optional (started nil))
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
		are-columns-aggregate (check-aggregate columns))
	(cond
		(are-columns-aggregate (select-table-aggregate table-parsed columns))
		(t (select-table-columns table-parsed columns where-clause order-by-clause))))

(defun select-table-aggregate (table-parsed columns)
	(setq
		table-columns-names
			(mapcar
				#'(lambda (column-parsed)
					(car (cdr column-parsed)))
				table-parsed))
	(setq
		aggregate-functions
			(mapcar
				#'(lambda (column)
					(setq
						open-paren-index (search "(" column)
						close-paren-index (search ")" column))
					(cond
						((or (null open-paren-index) (null close-paren-index))
							(error "You can't select columns with aggregate functions: ~S" query)))
					(setq
						function
							(subseq column 0 open-paren-index)
						column-arg
							(subseq
								column
								(+ open-paren-index 1)
								close-paren-index))
					(cons function column-arg))
				columns))
	(setq
		table-selected
			(mapcar
				#'(lambda (aggregate-function)
					(select-table-get-values-aggregate
						table-parsed
						aggregate-function))
				aggregate-functions))
	table-selected)

(defun select-table-columns (table-parsed columns where-clause order-by-clause)
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

(defun select-table-get-values-aggregate (table-parsed aggregate-function)
	(setq column-parsed (car table-parsed))
	(setq column-name (car (cdr column-parsed)))
	(setq column-arg (cdr aggregate-function))
	(cond
		((string-equal column-name column-arg)
			(calculate-value column-parsed aggregate-function))
		((null (cdr table-parsed))
			(error "Column wasn't found: ~S" column-arg))
		(t (select-table-get-values-aggregate (cdr table-parsed) aggregate-function))))

(defun calculate-value (column aggregate-function)
	(setq
		column-function (car aggregate-function)
		column-values (cdr (cdr column))
		value
			(cond
				((string= column-function "avg")
					(calculate-value-avg column-values))
				((string= column-function "max")
					(calculate-value-max column-values))))
	(cons
		(+ (length (write-to-string value)) 2)
		(cons
			(car (cdr column))
			(cons value nil))))

(defun calculate-value-avg (column-values)
	(/ (reduce #'+ column-values) (length column-values)))

(defun calculate-value-max (column &optional (max 0))
	(setq
		value (car column)
		remainder (cdr column)
		new-max
			(cond
				((> value max) value)
				(t max)))
	(cond
		((null remainder) new-max)
		(t (calculate-value-max remainder new-max))))

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

(defun check-aggregate (columns)
	(reduce
		#'(lambda (is-aggregate column)
			(setq column-lower (string-downcase column))
			(cond
				((< (length column) 4) is-aggregate)
				((or
					(string= (subseq column 0 4) "max(")
					(string= (subseq column 0 4) "avg("))
					T)
				(t is-aggregate)))
		columns
		:initial-value nil))