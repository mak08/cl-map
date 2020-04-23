;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2020-04-17 00:38:27>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI" "CL-GEOMATH")
  (:export "*MAP-FILE*"
           "ENSURE-MAP"
           "POINT-ON-LAND-P"
           "INTERSECTS-LAND-P"
           "LINE-INTERSECTS-LAND-P"
           "RECTANGLE-INTERSECTS-LAND-P"
           "LINE-LAND-INTERSECTION"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
