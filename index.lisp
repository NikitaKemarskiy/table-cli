;;;; Variant 13

;;; Require modules
(load "command.lisp")

;;; Variables
(setq
	username (getenv "USERNAME")
	hostname (getenv "HOSTNAME"))

;;; Get table function
(defun main ()
	;; Print user greeting
	(terpri)
	(princ (concatenate 'string username "@" hostname "$: "))

	;; Read table index
	#| (setq query (read-line))
	(clear-input) |#

	;; FOR TEST
	(setq query "select row, row, col, col, pos_x from map_zal-skl9.csv where col <= 15 and row <= 10 order by row asc")
	;(setq query "load(\"mp-assistants.csv\")")
	;(setq query "load(\"mp-posts_full.csv\")")
	;(setq query "load(\"plenary_register_mps-skl9.tsv\")")

	;; Parse user command
	(setq command (parse-command query))
	;; Execute user's command
	(execute-command command query))

(main)