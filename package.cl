;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2020-04-12 18:59:05>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI" "CL-GEOMATH")
  (:export "*MAP-FILE*"
           "ENSURE-MAP"
           "IS-LAND"
           "INTERSECTS-LAND-P"
           "LINE-INTERSECTS-LAND-P"
           "LINE-LAND-INTERSECTION"
           "RECTANGLE-INTERSECTS-LAND-P"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
