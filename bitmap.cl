;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2020
;;; Last Modified <michael 2023-05-07 12:40:24>

(in-package :cl-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating bitmaps

(defparameter *bitmap-file* "/home/michael/Maps/bm-tiled-5400.dat")
(defvar *bitmap* nil)
(defvar *bitmap-latpoints* 5400)
(defvar *bitmap-lonpoints* 10800)

(defun ensure-bitmap ()
  (log2:info "Loading ~ax~a bitmap ~a"  *bitmap-latpoints* *bitmap-lonpoints* *bitmap-file*)
  (setf *bitmap* (read-bitmap-file *bitmap-file*
                                   *bitmap-latpoints*
                                   *bitmap-lonpoints*))
  (values t))


(declaim (inline check-bitmap))
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
  "T ==> land, nil ==> no land"
  (let ((lat-index (truncate (+ (latlng-lat latlng) 90d0)
                             (bitmap-dlat% bitmap)))
        (lon-index (truncate (+ (latlng-lng latlng) 180d0)
                             (bitmap-dlon% bitmap))))
    (eql 1
         (aref (bitmap-data bitmap) lat-index lon-index))))

(defun test-rectangle (lat-nw lng-nw lat-se lng-se)
  (declare (double-float lat-nw lng-nw lat-se lng-se))
  (let ((vectormap (mapp-data *map*))
        (segment (ogr-g-create-geometry wkbLinearRing))
        (poly (ogr-g-create-geometry wkbPolygon)))
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-geometry-directly poly segment)
    
    (ogr-l-set-spatial-filter vectormap poly)
    (ogr-l-reset-reading vectormap)
    ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
    ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
    (loop
      :for feature = (ogr-l-get-next-feature vectormap)
      :while (not (null-pointer-p feature))
      :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                 (found (ogr-g-intersects polygon poly)))
            (ogr-g-destroy-geometry poly)
            ;; (ogr-g-destroy-geometry segment)
            (ogr-f-destroy feature)
            (when found
              (return-from test-rectangle t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bitmap File I/O

(defun write-bitmap-file (bitmap filename)
  (with-open-file (f filename
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type 'bit)
    (loop
      :for k :below (array-total-size bitmap)
      :do (write-byte (row-major-aref bitmap k)
                      f))
    (log2:info "Wrote ~a" (truename f))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate Bitmap

(defun generate-bitmap (filename &key
                                   (latpoints 180)
                                   (lonpoints 360)
                                   (lat-north 90d0)
                                   (lat-south -90d0))
  (log2:warning "Using ~a" *map-file*)
  (let ((bitmap (probe-map latpoints lonpoints :lat-north lat-north :lat-south lat-south)))
    (write-bitmap-file bitmap filename)))

(defun probe-map (latpoints lonpoints &key (lat-north 90d0) (lat-south -90d0))
  (declare ((signed-byte 64) latpoints lonpoints)
           (double-float lat-north lat-south))
  (let ((dlat (/ (- lat-north lat-south) latpoints))
        (dlon (/ 360d0 lonpoints))
        (result (make-array (list latpoints lonpoints) :element-type 'bit)))
    (loop
      :for lat :of-type double-float :downfrom (- lat-north dlat) :to lat-south :by dlat
      :do  (progn
             (log2:info "Lat: ~,2,,f" lat)
             (loop
               :for lon :of-type double-float :from -180d0 :to (- 180d0 dlon) :by dlon
               :do  (when
                        (test-rectangle lat lon  (+ lat dlat) (+ lon dlon))
                      (let ((lat-index (truncate (+ lat 90) dlat))
                            (lon-index (truncate (+ lon 180) dlon)))
                        (setf (aref result lat-index lon-index) 1))))))
    (values result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate Bitmap Tiled (single file)

(defun generate-bitmap-tiled  (filename  &key
                                           (latpoints 180)
                                           (lonpoints 360)
                                           (lat-north 90d0)
                                           (lat-south -90d0)
                                           (tile-size 1d0))
  (let ((dlat (/ (- lat-north lat-south) latpoints))
        (dlon (/ 360d0 lonpoints))
        (bitmap (make-array (list latpoints lonpoints) :element-type 'bit)))
    (loop :for tlat :from -90d0 :to (- 90d0 tile-size) :by tile-size
          :do (progn
                (log2:info "Lat: ~,2,,f" tlat)
                (loop :for tlon :from -180d0 :to (- 180d0 tile-size) :by tile-size
                      :for north = (+ tlat tile-size)
                      :for west  = tlon
                      :for south = tlat
                      :for east  = (+ tlon tile-size)
                      :do (cond
                            ((land-tile-p north west south east)
                             (fill-map-tile bitmap :dlat dlat
                                                   :dlon dlon
                                                   :lat-north north
                                                   :lat-south south
                                                   :lon-east east
                                                   :lon-west west))
                            (t
                             (probe-map-tile bitmap :dlat dlat
                                                    :dlon dlon
                                                    :lat-north north
                                                    :lat-south south
                                                    :lon-east east
                                                    :lon-west west))))))
    (write-bitmap-file bitmap filename)))


(defun probe-map-tile (map &key
                             (dlat 1d0)
                             (dlon dlat)
                             (lat-north 90d0) (lat-south -90d0)
                             (lon-west -180d0) (lon-east 180d0))
  (log2:trace "dlat=~a dlon=~a" dlat dlon)
  (when
      (test-rectangle lat-north lon-west lat-south lon-east)
    (loop
      :for lat :from (- lat-north dlat) :downto lat-south :by dlat
      :do (progn
            (loop
              :for lon :from (- lon-east dlon) :downto lon-west :by dlon
              :do  (when
                       (test-rectangle lat lon  (+ lat dlat) (+ lon dlon))
                     (let ((lat-index (truncate (+ lat 90d0) dlat))
                           (lon-index (truncate (+ lon 180d0) dlon)))
                       (setf (aref map lat-index lon-index) 1)))))))
  (values t))

(defun fill-map-tile (map &key
                             (dlat 1d0)
                             (dlon dlat)
                             (lat-north 90d0) (lat-south -90d0)
                             (lon-west -180d0) (lon-east 180d0))
  (log2:trace "dlat=~a dlon=~a" dlat dlon)
  (loop
    :for lat :from (- lat-north dlat) :downto lat-south :by dlat
    :do (loop
          :for lon :from (- lon-east dlon) :downto lon-west :by dlon
          :do  (let ((lat-index (truncate (+ lat 90d0) dlat))
                     (lon-index (truncate (+ lon 180d0) dlon)))
                 (setf (aref map lat-index lon-index) 1))))
  (values t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate tile files

(defun generate-bitmap-tile (filename &key
                                        (latpoints 360)
                                        (lonpoints 360)
                                        (lat-north 90d0) (lat-south -90d0)
                                        (lon-west -180d0) (lon-east 180d0))
  (let ((dlat (/ (- lat-north lat-south) latpoints))
        (dlon (/ (- lon-east lon-west) lonpoints))
        (bitmap (make-array (list latpoints lonpoints) :element-type 'bit)))
    (log2:info "dlat=~a dlon=~a" dlat dlon)
    (probe-map-tile bitmap :dlat dlat
                           :dlon dlon 
                           :lat-north lat-north
                           :lat-south lat-south
                           :lon-west lon-west
                           :lon-east lon-east)
    (write-bitmap-file bitmap filename)))
  

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
