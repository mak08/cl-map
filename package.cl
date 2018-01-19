;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-18 19:39:40>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI")
  (:export "LATLNG"
           "MAKE-LATLNG"
           "COPY-LATLNG"
           "LATLNG-LATR"
           "LATLNG-LNGR"
           "LATLNG-LAT"
           "LATLNG-LNG"
           
           "RAD"
           "ANGLE"
           
           "*MAP-FILE*"
           "ENSURE-MAP"
           "IS-LAND"
           "INTERSECTS-LAND-P"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
