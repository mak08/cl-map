;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-04-23 00:15:57>

(defsystem "cl-map"
  :description "Vector map queries using GDAL/OGR"
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "log2" "makros" "cl-geomath")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "gdal-api")
               (:file "map")
               (:file "bitmap")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

