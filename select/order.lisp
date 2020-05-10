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