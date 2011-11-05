;;;; -*- Mode: Lisp -*-

(defpackage #:org.mapcar.parse-number-tests
  (:use #:common-lisp #:org.mapcar.parse-number)
  (:export #:run-tests))

(in-package #:org.mapcar.parse-number-tests)

(defparameter *test-values*
  '("1" "-1" "1034" "-364" "80/335" "3.5333" "2.4E4" "6.8d3" "#xFF"
    "#b-1000" "#o-101/75" "13.09s3" "35.66l5" "21.4f2" "#C(1 2)"
    "#c ( #xF #o-1 ) " "#c(1d1 2s1)" "#16rFF" "#9r10" "#C(#9r44/61 4f4)"))

(defun run-tests ()
  (format t "~&~16@A (~16@A) = ~16A~%~%"
	  "String value" "READ value" "Parsed value")
  (dolist (value *test-values*)
    (format t "~&~16@A (~16@A) = ~16A~%"
	    value
	    (read-from-string value)
	    (parse-number value))))
