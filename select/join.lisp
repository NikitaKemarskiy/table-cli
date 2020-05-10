(defun join-tables (table-selected-1 table-selected-2 column-name join-clause)
	(cond
		((string= join-clause "inner")
			(join-tables-inner table-selected-1 table-selected-2 column-name))
		((or
			(string= join-clause "outer")
			(string= join-clause "left"))
				(join-tables-outer table-selected-1 table-selected-2 column-name))))

(defun join-tables-inner (table-selected-1 table-selected-2 column-name)
	(setq
		order-by-column-index-1
			(get-column-index-by-name
				table-selected-1
				column-name)
		order-by-column-index-2
			(get-column-index-by-name
				table-selected-2
				column-name))
	(setq
		table-selected-values-1
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected-1)
		table-selected-values-2
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected-2))
	(setq
		table-selected-rows-values-1
			(transpose-table table-selected-values-1)
		table-selected-rows-values-2
			(transpose-table table-selected-values-2)
		joined-rows-values
			(reduce
				#'(lambda (accum row-1)
					(setq value-1 (nth order-by-column-index-1 row-1))
					(append
						(reduce
							#'(lambda (accum row-2)
								(setq value-2 (nth order-by-column-index-2 row-2))
								(cond
									((compare= value-1 value-2)
										(cons (append row-1 row-2) accum))
									(t accum)))
							table-selected-rows-values-2
							:initial-value nil)
						accum))
				table-selected-rows-values-1
				:initial-value nil))
	(setq
		joined-values
			(transpose-table joined-rows-values)
		joined-table-headers
			(append
				(mapcar
					#'(lambda (column)
						(list
							(car column)
							(car (cdr column))))
					table-selected-1)
				(mapcar
					#'(lambda (column)
						(list
							(car column)
							(car (cdr column))))
					table-selected-2))
		joined-table
			(mapcar
				#'append
				joined-table-headers
				joined-values))
	joined-table)

(defun join-tables-outer (table-selected-1 table-selected-2 column-name)
	(setq
		order-by-column-index-1
			(get-column-index-by-name
				table-selected-1
				column-name)
		order-by-column-index-2
			(get-column-index-by-name
				table-selected-2
				column-name)
		table-selected-values-1
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected-1)
		table-selected-values-2
			(mapcar
				#'(lambda (column)
					(cdr (cdr column)))
				table-selected-2)
		table-selected-rows-values-1
			(transpose-table table-selected-values-1)
		table-selected-rows-values-2
			(transpose-table table-selected-values-2)
		joined-rows-values
			(reduce
				#'(lambda (accum row-1)
					(setq value-1 (nth order-by-column-index-1 row-1))
					(setq joined
						(reduce
							#'(lambda (accum row-2)
								(setq value-2 (nth order-by-column-index-2 row-2))
								(cond
									((compare= value-1 value-2)
										(cons (append row-1 row-2) accum))
									(t accum)))
							table-selected-rows-values-2
							:initial-value nil))
					(append
						(cond
							((null joined)
								(cons
									(append
										row-1
										(get-nil-list (length (car table-selected-rows-values-2))))
									nil))
							(t joined))
						accum))
				table-selected-rows-values-1
				:initial-value nil))
	(setq
		joined-values
			(transpose-table joined-rows-values)
		joined-table-headers
			(append
				(mapcar
					#'(lambda (column)
						(list
							(car column)
							(car (cdr column))))
					table-selected-1)
				(mapcar
					#'(lambda (column)
						(list
							(car column)
							(car (cdr column))))
					table-selected-2))
		joined-table
			(mapcar
				#'append
				joined-table-headers
				joined-values))
	joined-table)