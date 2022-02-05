;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2022-01-31 23:39:36>

(defsystem "cl-map"
  :description "Vector map queries using GDAL/OGR"
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "log2" "makros" "cl-geomath")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "gdal-api")
               (:file "map")
               (:file "vecmap")
               (:file "bitmap")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

