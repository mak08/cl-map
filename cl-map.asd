;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 11:48:50>

(defsystem "cl-map"
  :description "Vector map queries using GDAL/OGR"
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "log2" "cl-geomath")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "gdal-api")
               (:file "map")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

