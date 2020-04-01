;;; Select
(defun select (table-parsed)
	(setq
		table-header (get-table-header (car table-parsed)))
	table-header)