;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2022-02-02 20:50:08>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI" "CL-GEOMATH" "MACROS")
  (:export "*MAP-FILE*"
           "*BITMAP-FILE*"
           "*BITMAP-LATPOINTS*"
           "*BITMAP-LONPOINTS*"
           "ENSURE-MAP"
           "ENSURE-BITMAP"
           "POINT-ON-LAND-P"
           "INTERSECTS-LAND-P"
           "LINE-INTERSECTS-LAND-P"
           "RECTANGLE-INTERSECTS-LAND-P"
           "LINE-LAND-INTERSECTION"
           "BM-IS-LAND"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
