;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-23 13:10:47>

(in-package :cl-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(defstruct dms (degrees 0) (minutes 0) (seconds 0))

(defun dms2decimal (dms)
  (+ (dms-degrees dms)
     (/ (dms-minutes dms) 60)
     (/ (dms-seconds dms) 3600)))

(defun decimal2dms (dec)
  (multiple-value-bind (d r1)
      (truncate dec)
    (multiple-value-bind (m r2)
        (truncate (* r1 60d0))
      (make-dms :degrees d :minutes m :seconds (* r2 60d0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read and print coordinates in the fixed format dddÂ°mm'ss".
;;; Reading fails (and probably produces strange errors) if the coordinate
;;; is not in this format.

(defmethod print-object ((thing dms) stream)
  (format stream  "~3,'0d°~2,'0d'~2,'0d\""
          (dms-degrees thing)
          (dms-minutes thing)
          (round (dms-seconds thing))))

(defun read-dms (stream)
  (flet ((decode (c0 c1 c2)
           (+ (* (- (char-code c0) 48) 100d0)
              (* (- (char-code c1) 48) 10d0)
              (- (char-code c2) 48))))
    (let ((chars
           (loop :for k :below 10 :collect (read-char stream t nil nil))))
      (make-dms :degrees (decode (nth 0 chars) (nth 1 chars) (nth 2 chars)) 
                :minutes (decode #\0 (nth 4 chars) (nth 5 chars))
                :seconds (decode #\0 (nth 7 chars) (nth 8 chars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lat&Lng
;; TODO: A latlng should only be used to represent Google Maps coordinates.

(defstruct latlng
  (lat% nil)
  (lng% nil)
  (latr% 0d0 :type double-float)
  (lngr% 0d0 :type double-float))


(defun latlng-lat (latlng)
  (or (latlng-lat% latlng)
      (setf (latlng-lat% latlng)
            (deg (latlng-latr latlng)))))

(defun latlng-lng (latlng)
  (or (latlng-lng% latlng)
      (setf (latlng-lng% latlng)
            (deg (latlng-lngr latlng)))))

(declaim (inline latlng-lng latlng-lngr))

(defun latlng-latr (latlng)
  (declare (inline rad))
  (cond ((eql (latlng-latr% latlng) 0d0)
         (setf (latlng-latr% latlng)
               (rad (latlng-lat latlng))))
        (t
         (latlng-latr% latlng))))

(defun latlng-lngr (latlng)
  (declare (inline rad))
  (cond ((eql (latlng-lngr% latlng) 0d0)
         (setf (latlng-lngr% latlng)
               (rad (latlng-lng latlng))))
        (t
         (latlng-lngr% latlng))))
(declaim (notinline latlng-latr latlng-lngr))


(defmethod print-object ((thing latlng) stream)
  (let ((lat-value
         (decimal2dms (abs (latlng-lat thing))))
        (lng-value
         (decimal2dms (abs (latlng-lng thing))))
        (lat-marker
         (if (< (latlng-lat thing) 0) "S" "N"))
        (lng-marker
         (if (< (latlng-lng thing) 0) "W" "E")))
    (format stream "#p[~a~a, ~a~a]" lat-value lat-marker lng-value lng-marker)))

(set-dispatch-macro-character #\# #\p 'read-latlng)

(defun read-latlng (stream sub-char arg)
  (declare (ignore sub-char arg))
  (let ((open-bracket (read-char stream))
        (latitude (read-dms stream))
        (lat-marker (read-char stream))
        (comma (read-char stream))
        (space (read-char stream))
        (longitude (read-dms stream))
        (lng-marker (read-char stream))
        (close-bracket (read-char stream)))
    (let ((lat-sign (ecase lat-marker (#\N 1) (#\S -1)))
          (lng-sign (ecase lng-marker (#\E 1) (#\W -1))))
      (make-latlng :lat% (* lat-sign (dms2decimal latitude))
                   :lng% (* lng-sign (dms2decimal longitude))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units Conversion

(declaim (inline rad))
(defun rad (x)
  (declare (double-float x))
  (* (* 2d0 pi) (/ x 360d0)))
(declaim (notinline rad))

(declaim (inline deg))
(defun deg (x)
  (declare (double-float x))
  (* 360 (/ x (* 2 pi))))
(declaim (notinline deg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(defconstant 180/pi (/ 180d0 pi))

(declaim (inline angle))
(defun angle (u v)
  (declare (double-float u v))
  (let ((angle
         (+ 180d0 (* 180/pi (atan u v)))))
    angle))
(declaim (notinline angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map file and dataset

;; Vector map describing Land areas. It must contain a layer "Land_Polygons".
;; It must have an accompanying index file land_polygons.shx."
;;
;;   There is a smaller file, simplified-land-polygons-complete-3857,
;;   but it is NOT faster, most likely because it does not have split polygons
;;   (and therefore, indexing does not help as much).
;;   Moreover, land detection is too coarse with the simplified polygons.
(defvar *map-file* "/home/michael/Maps/land-polygons-split-4326/land_polygons.shp"
  "Map data filename")

(defvar *map* nil
  "The OGR Layer containing land polygons")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENSURE-MAP
;;   Initialize GDAL and load map data.
;;   Does NOT clean-up the dataset 

(defvar +map-lock+
  (bordeaux-threads:make-lock "MAP-LOCK"))

(defun ensure-map (&key (filename *map-file*))
  (unless *map*
    (bordeaux-threads:with-lock-held (+map-lock+)
      (log2:info "Loading shapefile ~a" filename)
      (gdal-all-register)
      (let* ((ds (gdal-open-ex filename
                               4
                               (cffi:null-pointer)
                               (cffi:null-pointer)
                               (cffi:null-pointer)))
             (layer-count (gdal-dataset-get-layer-count ds))
             (land-polygons (gdal-dataset-get-layer ds 0)))
        (unless (eql layer-count 1)
          (error "Unexpected layer count ~a" layer-count))
        (cond
          ((< 0 (ogr-l-test-capability land-polygons OLCFastSpatialFilter))
           ;; Succeed only if we have an index
           (setf *map* land-polygons))
          (t
           (error "Layer not found or no index")))))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS-LAND
;;  Return TRUE iff (lat. lon) is on land

(let ((point (ogr-g-create-geometry wkbPoint)))
  (defun is-land (lat lon)
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d point 0 lon lat)
      (ogr-l-set-spatial-filter *map* point)
      (ogr-l-reset-reading *map*)
      (loop
         :for feature = (ogr-l-get-next-feature *map*)
         :while (not (null-pointer-p feature))
         :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                    (found (ogr-g-contains polygon point)))
               (ogr-f-destroy feature)
               (when found
                 (return-from is-land t)))))))


(let ((segment (ogr-g-create-geometry wkbLineString)))
  (ogr-g-add-point-2d segment 0d0 0d0)
  (ogr-g-add-point-2d segment 0d0 0d0)
  (defun line-intersects-land-p (start end)
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
      (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter *map* segment)
      (ogr-l-reset-reading *map*)
      ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
      ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
      (loop
         :for feature = (ogr-l-get-next-feature *map*)
         :while (not (null-pointer-p feature))
         :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                    (found (ogr-g-intersects polygon segment)))
               (ogr-f-destroy feature)
               (when found
                 (return-from line-intersects-land-p t)))))))

;;; Same as above, but without scanning the result.
;;; In practive this looked correct but is only slightly faster.
(let ((segment (ogr-g-create-geometry wkbLineString)))
  (ogr-g-add-point-2d segment 0d0 0d0)
  (ogr-g-add-point-2d segment 0d0 0d0)
  (defun intersects-land-noscan-p (start end)
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
      (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter *map* segment)
      (ogr-l-reset-reading *map*)
      (let ((feature (ogr-l-get-next-feature *map*)))
        (not (null-pointer-p feature))))))

(defun rectangle-intersects-land-p (nw se)
  (let ((segment (ogr-g-create-geometry wkbLinearRing))
        (poly (ogr-g-create-geometry wkbPolygon)))
    (ogr-g-add-point-2d segment (latlng-lng nw) (latlng-lat nw))
    (ogr-g-add-point-2d segment (latlng-lng se) (latlng-lat nw))
    (ogr-g-add-point-2d segment (latlng-lng se) (latlng-lat se))
    (ogr-g-add-point-2d segment (latlng-lng nw) (latlng-lat se))
    (ogr-g-add-point-2d segment (latlng-lng nw) (latlng-lat nw))
    (ogr-g-add-geometry poly segment)
    (bordeaux-threads:with-lock-held (+map-lock+)
      
      (ogr-l-set-spatial-filter *map* poly)
      (ogr-l-reset-reading *map*)
      ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
      ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
      (loop
         :for feature = (ogr-l-get-next-feature *map*)
         :while (not (null-pointer-p feature))
         :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                    (found (ogr-g-intersects polygon poly)))
               (ogr-f-destroy feature)
               (when found
                 (return-from rectangle-intersects-land-p t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fast land detection

(defun intersects-land-p (start end)
  ;; Check if the line from $start to $end intersects land.
  ;; This implementation assumes that $start and $end are on the same tile.
  (when
      ;; When there is any land in the tile containing point...
      (tile-intersects-land-p end)
    ;; ... look closer.
    (line-intersects-land-p start end)))

(defconstant +tile-width+ 0.2d0)
(defconstant +tile-num+ 5)

(defvar *tile-array* (make-array (list (* 180 +tile-num+)
                                       (* 360 +tile-num+))
                                 :initial-element :unknown))

(defparameter *miss* 0)
(defparameter *hit* 0)

(defun tile-intersects-land-p (latlng)
  (let* ((maxnorth (array-dimension *tile-array* 0)) 
         (maxwest (array-dimension *tile-array* 1))
         (north (floor (latlng-lat latlng) +tile-width+))
         (west (floor (latlng-lng latlng) +tile-width+))
         (i-north (if (< north 0) (+ north maxnorth) north))
         (i-west (if (< west 0) (+ west maxwest) west)))
    (let ((land-p (aref *tile-array* i-north i-west)))
      (cond ((eq land-p :unknown)
             (incf *miss*)
             (let* ((south (ceiling (latlng-lat latlng) +tile-width+))
                    (east (ceiling (latlng-lng latlng) +tile-width+))
                    (nw (make-latlng :lat% (* north +tile-width+)
                                     :lng% (* west +tile-width+)))
                    (se (make-latlng :lat% (* south +tile-width+)
                                     :lng% (* east +tile-width+)))
                    (land-p (rectangle-intersects-land-p nw se)))
               (setf (aref *tile-array* i-north i-west) land-p)))
            (t
             (incf *hit*)
             land-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize map on load -
;;; DON'T - *map-file* needs to be customized first!

;;; (ensure-map)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
