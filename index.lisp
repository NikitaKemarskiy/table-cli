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
	(setq query (read-line))
	(clear-input)

	;; FOR TEST
	(setq query "load(\"map_zal-skl9.csv\")")
	
	;; Parse user command
	(setq command (parse-command query))
	;; Execute user's command
	(execute-command command query))

(main)