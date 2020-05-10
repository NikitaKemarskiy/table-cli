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
							(t (compare/= item1 item2))))))
		filtered-values-rows
			(filter-rows-by-predicate
				table-selected-values-rows
				column-index
				#'(lambda (value)
					(funcall where-function value compare-value-converted)))
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