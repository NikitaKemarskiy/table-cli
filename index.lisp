;;;; Variant 13

;;; Require modules
(load "libs/asdf/asdf.lisp")
(load "command.lisp")

;;; Variables
(setq
	username (getenv "USERNAME")
	hostname (getenv "HOSTNAME"))

;;; Get table function
(defun main ()
	;; Print user greeting
	(terpri)
	(princ (concatenate username "@" hostname "$: "))

	;; Read table index
	(setq query (read-line))
	(clear-input)
	;; Parse user command
	(setq command (parse-command query))
	;; Execute user's command
	(execute-command command query))

(main)

;; load("map_zal-skl9.csv")