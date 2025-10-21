;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2020
;;; Last Modified <michael 2025-10-14 23:32:20>

(in-package :cl-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating bitmaps


(defvar *water-polygons*
  (load-map "/home/michael/Maps/water-polygons/water-polygons-split-4326/water_polygons.shx"))

(defparameter *bitmap-file* "/home/michael/Maps/bm-tiled-5400.dat")
(defvar *bitmap* nil)
(defvar *bitmap-latpoints* 5400)
(defvar *bitmap-lonpoints* 10800)

(defun ensure-bitmap ()
  (log2:info "Loading ~ax~a bitmap ~a"  *bitmap-latpoints* *bitmap-lonpoints* *bitmap-file*)
  (setf *bitmap* (read-bitmap-file *bitmap-file*))
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

(declaim (inline test-rectangle))
(defun test-rectangle (lat-nw lng-nw lat-se lng-se &key (map *map*))
  (declare (double-float lat-nw lng-nw lat-se lng-se))
  (let ((vectormap (mapp-polygons map))
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
                     :element-type '(unsigned-byte 8))
    (destructuring-bind (n-lat n-lon)
        (array-dimensions bitmap)
      (assert (zerop (mod n-lat 8)))
      (assert (zerop (mod n-lon 8)))
      (log2:info "Writing lat points: ~a, lon points: ~a" n-lat n-lon)
      (write-u16 f n-lat)
      (write-u16 f n-lon)
      (loop
        :for n :below (/ (array-total-size bitmap) 8)
        :do (let ((b 0))
              (dotimes (k 8)
                (setf b
                      (dpb (row-major-aref bitmap (+ (* n 8) k)) (byte 1 k) b)))
              (write-byte b f)))
      (log2:info "Wrote ~a" (truename f)))))
  
(defun read-bitmap-file (filename)
  (log2:info "Reading ~a" (pathname filename))
  (with-open-file (f filename
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (let* ((n-lat (read-u16 f))
           (n-lon (read-u16 f))
           (bitmap (make-array (list n-lat n-lon) :element-type 'bit)))
      (log2:info "Reading lat points: ~a, lon points: ~a" n-lat n-lon)
      (loop
        :for n :below (/ (* n-lat n-lon) 8)
        :do (let ((b (read-byte f nil nil)))
              (when (null b)
                (log2:info "--> ~a" n))
              (dotimes (k 8)
                (setf (row-major-aref bitmap (+ (* n 8) k))
                      (ldb (byte 1 k) b)))))
      (values bitmap))))

(defun write-u16 (f b)
  (write-byte (ldb (byte 8 0) b) f)
  (write-byte (ldb (byte 8 8) b) f)
  (values))

(defun read-u16 (f)
  (let ((b 0))
    (setf b (dpb (read-byte f nil nil) (byte 8 0) b))
    (setf b (dpb (read-byte f nil nil) (byte 8 8) b))
    b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate tile

(defun probe-tile (&key
                     (latpoints 180)
                     (lonpoints 360)
                     (lat-north 90d0) (lat-south -90d0)
                     (lon-west -180d0) (lon-east 180d0))

  (let ((dlat (/ (- lat-north lat-south) latpoints))
        (dlon (/ (- lon-east lon-west) lonpoints))
        (bitmap (make-array (list latpoints lonpoints) :element-type 'bit)))
    (log2:info "Probing ~a ~a ~a ~a" lat-north lat-south lon-west lon-east)

    (log2:trace "dlat=~a dlon=~a" dlat dlon)
    ;; (loop
    ;;   :for lat :from (- lat-north dlat) :downto lat-south :by dlat
    ;;   :for lat-index :from 0
    ;;   :do (loop
    ;;         :for lon :from (- lon-east dlon) :downto lon-west :by dlon
    ;;         :for lon-index :from 0
    ;;         :do  (when
    ;;                  (test-rectangle lat lon  (+ lat dlat) (+ lon dlon))
    ;;                (setf (aref bitmap lat-index lon-index) 1))))
    (values bitmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate tile files

(defun generate-tiles (&key (height 1) (width 1) (resolution 120))
  (let* ((directory (format nil "~a_~a_~a" height width resolution))
         (latpoints (floor (* height resolution)))
         (lonpoints (floor (* width resolution)))
         (lat-north 90d0)
         (lat-south -90d0)
         (lon-west -180d0)
         (lon-east 180d0)
         (land-tiles 0)
         (water-tiles 0)
         (probed-tiles 0))
    (ensure-directories-exist (format nil "~a/" directory))
    (log2:info "Writing ~a*~a tiles" (/ 180 height) (/ 360 width))
    (loop
      :for lat :from lat-north :downto lat-south :by height
      :for lat1 = (- lat height)
      :do (loop
            :for lon :from lon-east :downto lon-west :by width
            :for lon1 = (- lon width)
            :do (cond
                  ((null (test-rectangle lat lon lat1 lon1))
                   (incf water-tiles)
                   (write-water-tile lat lon lat1 lon1))
                  ((null (test-rectangle lat lon lat1 lon1 :map *water-polygons*))
                   (incf land-tiles)
                   (write-land-tile lat lon lat1 lon1))
                  (t
                   (incf probed-tiles)
                   (let ((tile (probe-tile :latpoints latpoints
                                           :lonpoints lonpoints
                                           :lat-north lat
                                           :lat-south lat1
                                           :lon-east lon
                                           :lon-west lon1)))
                     (write-bitmap-file tile (tile-path directory lat lon)))))))
    (format t "Water  tiles: ~a~%Land   tiles: ~a~%Probed tiles: ~a~%Total: ~a~%"
            water-tiles land-tiles probed-tiles
            (+ water-tiles land-tiles probed-tiles))))

(defun tile-path (directory north east)
  (make-pathname :directory `(:relative ,directory) :name (format nil "~f_~f" north east) :type "dat"))

(defun write-land-tile (lat0 lon0 lat1 lon1)
  (log2:info "Land tile: ~a ~a ~a ~a" lat0 lon0 lat1 lon1))

(defun write-water-tile (lat0 lon0 lat1 lon1)
  (log2:info "Water tile: ~a ~a ~a ~a" lat0 lon0 lat1 lon1))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
