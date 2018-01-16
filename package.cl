;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-15 21:52:49>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI")
  (:export "*MAP-FILE*"
           "ENSURE-MAP"
           "IS-LAND"
           "INTERSECTS-LAND-P"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
