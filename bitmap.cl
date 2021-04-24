;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2020
;;; Last Modified <michael 2021-04-23 16:47:24>

(in-package :cl-map)

(setf (log2:log-destination "cl-map") "cl-map.log")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating bitmaps

(defvar *bitmap-file* "/home/michael/Maps/5400x10800.dat")
(defvar *bitmap* nil)
(defvar *bitmap-latpoints* 5400)
(defvar *bitmap-lonpoints* 10800)

(defun ensure-bitmap ()
  (setf *bitmap* (read-bitmap-file *bitmap-file*
                                   *bitmap-latpoints*
                                   *bitmap-lonpoints*))
  (values t))


(defun-t bm-is-land t ((latlng latlng))
  (check-bitmap *bitmap* latlng))

;;; Bitmap representing a geographical map
(defstruct (bitmap (:constructor %make-bitmap))
  data
  dlat%
  dlon%)

(defun make-bitmap (data)
  (destructuring-bind (latpoints lngpoints)
      (array-dimensions data)
    (%make-bitmap :data data
                  :dlat% (/ 180d0 latpoints)
                  :dlon% (/ 360d0 lngpoints))))

(declaim (inline check-bitmap))
(defun check-bitmap (bitmap latlng)
  "nil ==> land, T ==> no land"
  (let ((lat-index (truncate (+ (latlng-lat latlng) 90d0)
                             (bitmap-dlat% bitmap)))
        (lon-index (truncate (+ (latlng-lng latlng) 180d0)
                             (bitmap-dlon% bitmap))))
    (eql 1
         (aref (bitmap-data bitmap) lat-index lon-index))))

(defun write-bitmap-file (bitmap filename)
  (with-open-file (f filename
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type 'bit)
    (loop
      :for k :below (array-total-size bitmap)
      :do (write-byte (row-major-aref bitmap k)
                      f))))

(defun read-bitmap-file (filename x y)
  (let ((bitmap (make-array (list x y) :element-type 'bit)))
    (with-open-file (f filename
                       :direction :input
                       :element-type 'bit)
      (loop
        :for k :below (array-total-size bitmap)
        :do (setf  (row-major-aref bitmap k)
                   (read-byte f))))
    (make-bitmap bitmap)))

(defun test-rectangle (lat-nw lng-nw lat-se lng-se)
  (declare (double-float lat-nw lng-nw lat-se lng-se))
  (let ((segment (ogr-g-create-geometry wkbLinearRing))
        (poly (ogr-g-create-geometry wkbPolygon)))
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-geometry-directly poly segment)
    
    (ogr-l-set-spatial-filter *map* poly)
    (ogr-l-reset-reading *map*)
    ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
    ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
    (loop
      :for feature = (ogr-l-get-next-feature *map*)
      :while (not (null-pointer-p feature))
      :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                 (found (ogr-g-intersects polygon poly)))
            (ogr-g-destroy-geometry poly)
            ;; (ogr-g-destroy-geometry segment)
            (ogr-f-destroy feature)
            (when found
              (return-from test-rectangle t))))))

(defun probe-map (latpoints lonpoints &key (lat-north 90d0) (lat-south -90d0))
  (declare (integer latpoints lonpoints)
           (double-float lat-north lat-south))
  (let ((dlat (/ (- lat-north lat-south) latpoints))
        (dlon (/ 360d0 lonpoints))
        (result (make-array (list latpoints lonpoints) :element-type 'bit)))
    (loop
      :for lat :downfrom (- lat-north dlat) :to lat-south :by dlat
      :do  (progn
             (log2:info "Lat: ~,2,,f" lat)
             (loop
               :for lon :from -180d0 :to (- 180d0 dlon) :by dlon
               :do  (when
                        (test-rectangle lat lon  (+ lat dlat) (+ lon dlon))
                      (let ((lat-index (truncate (+ lat 90) dlat))
                            (lon-index (truncate (+ lon 180) dlon)))
                        (setf (aref result lat-index lon-index) 1))))))
    (values result)))

(defun generate-bitmap (filename latpoints lonpoints &key (lat-north 90d0) (lat-south -90d0))
  (log2:warning "Using ~a" *map-file*)
  (let ((bitmap (probe-map latpoints lonpoints :lat-north lat-north :lat-south lat-south)))
    (write-bitmap-file bitmap filename)))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
