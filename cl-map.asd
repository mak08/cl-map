;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2022-01-31 23:39:36>

(defsystem "cl-map"
  :description "Vector map queries using GDAL/OGR"
  :in-order-to ((test-op (test-op "cl-map/test")))
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "log2" "makros" "cl-geomath" "bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "gdal-api")
               (:file "prepared-cache")
               (:file "map")
               (:file "vecmap")
               (:file "bitmap")
               (:file "test")))

(defsystem "cl-map/test"
  :depends-on ("cl-map")
  :default-component-class cl-source-file.cl
  :components ((:file "tests/prepared-cache-tests"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (let ((directory (merge-pathnames
                               (format nil "cl-map-cache-tests-~d-~d/" (get-universal-time) (random 1000000000))
                               (uiop:temporary-directory))))
               (unwind-protect
                    (progn
                      (uiop:run-program (list "python3"
                        (namestring (merge-pathnames "tests/prepare-cache-fixtures.py" (asdf:system-source-directory "cl-map")))
                        (namestring directory)) :output *standard-output* :error-output *error-output*)
                      (uiop:symbol-call :cl-map :test-prepared-cache directory))
                 (when (probe-file directory)
                   (uiop:delete-directory-tree directory :validate t))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

