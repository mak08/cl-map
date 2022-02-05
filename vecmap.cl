;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2022
;;; Last Modified <michael 2022-02-01 00:30:45>

(in-package :cl-map)

(defstruct vecmap name n s w e poly)

(defun land-tile-p (north west south east)
  "Determine if 1° tile with given NW corner is entirely contained in one of the polygons of *vecmaps*"
  (some (lambda (p)
          (tile-inside-polygon-p north west south east p))
        *vecmaps*))

(defun tile-inside-polygon-p (north west south east vecmap)
  (when
      ;; tile is inside enclosing rectangle
      (and
       (< north (vecmap-n vecmap))
       (> south (vecmap-s vecmap))
       (> west (vecmap-w vecmap))
       (< east (vecmap-e vecmap)))
    ;; do actual check
    (and
     (point-in-poly-p (make-latlng :lat north :lng west) (vecmap-poly vecmap))
     (point-in-poly-p (make-latlng :lat north :lng east) (vecmap-poly vecmap))
     (point-in-poly-p (make-latlng :lat south :lng east) (vecmap-poly vecmap))
     (point-in-poly-p (make-latlng :lat south :lng west) (vecmap-poly vecmap)))))

(defun load-vector-map (directory)
  (log2:info "Loading from ~s directory" directory)
  (let ((polygons
          (loop
            :for path :in (directory (merge-pathnames directory (make-pathname :type "json")))
            :collect (load-polygon path))))
    (make-array (length polygons) :initial-contents polygons))) 

(defun load-polygon (filename)
  (let* ((json (parse-json-file filename))
         (poly (joref json "waypointsRoute"))
         (result (make-array (1+ (length poly)) :initial-element (make-latlng)
                                                :element-type 'latlng)))
    (loop
      :with n = -90d0 :with s = 180d0 :with w = 360d0 :with e = -180d0
      :for p :across poly :for k :from 0
      :for lat = (coerce (joref (joref p "wp") "latitude") 'double-float)
      :for lng = (coerce (joref (joref p "wp") "longitude") 'double-float)
      :do (progn
            (setf (aref result k)
                  (make-latlng :lat lat :lng lng))
            (setf n (max n lat))
            (setf s (min s lat))
            (setf w (min w lng))
            (setf e (max e lng))
            (setf (aref result (length poly))
                  (aref result 0)))
      :finally
         (return
           (make-vecmap :name (pathname-name filename) :n n :s s :w w :e e :poly result)))))

(defparameter  *vecmaps*
  (load-vector-map
   (make-pathname :directory (append (pathname-directory #.*compile-file-truename*) '("vectormaps"))
                  :name :wild)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
