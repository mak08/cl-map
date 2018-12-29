;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 11:59:34>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI" "CL-GEOMATH")
  (:export "*MAP-FILE*"
           "ENSURE-MAP"
           "IS-LAND"
           "INTERSECTS-LAND-P"
           "LINE-INTERSECTS-LAND-P"
           "RECTANGLE-INTERSECTS-LAND-P"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
