;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-22 01:53:37>

(defpackage "CL-MAP"
  (:use "COMMON-LISP" "CFFI")
  (:export "LATLNG"
           "MAKE-LATLNG"
           "COPY-LATLNG"
           "LATLNG-LATR"
           "LATLNG-LNGR"
           "LATLNG-LAT"
           "LATLNG-LNG"

           "DEG"
           "RAD"
           "ANGLE"
           
           "*MAP-FILE*"
           "ENSURE-MAP"
           "IS-LAND"
           "INTERSECTS-LAND-P"
           "LINE-INTERSECTS-LAND-P"
           "RECTANGLE-INTERSECTS-LAND-P"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
