;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-15 23:22:19>

(defsystem "cl-map"
  :description "Vector map queries using GDAL/OGR"
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "log2")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "gdal-api")
               (:file "map")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

