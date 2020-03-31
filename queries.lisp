;;; Select
(defun select (table-parsed)
	(setq
		table-header (get-table-header (car table-parsed)))
	table-header)

#| (defun get-table-header (table-parsed)
	(setq
		first-row (car table-parsed))
	(mapcar
		#'(lambda (column)
			(cons
				column
				(length column)))
		table-parsed)) |#