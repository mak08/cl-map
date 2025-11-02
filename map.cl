;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2025-11-02 16:43:48>

(in-package :cl-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map 

(defstruct mapp
  polygons
  filename
  (land-inverted nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !! Coordinate ranges !!
;; GDAL:
;;    Lat: N = 90, S = -90
;;    Lng: W = -180, ZeroM = 0, E = 180
;; Tilings:
;;    Lat: N = 0, S = 180
;;    Lng: ZeroM = 0/360, increasing eastward

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

(defun ensure-map (&key (filename *map-file*) (inverted nil))
  (unless *map*
    (let ((map 
            (load-map filename)))
      (setf *map* map))))

(defun load-map (filename)
  (bordeaux-threads:with-lock-held (+map-lock+)
    (log2:info "Loading shapefile ~a" filename)
    (gdal-all-register)
    (let ((ds (gdal-open-ex filename
                            GDAL_OF_VECTOR
                            (cffi:null-pointer)
                            (cffi:null-pointer)
                            (cffi:null-pointer))))
      (when (null-pointer-p ds)
        (error "Failed to open ~a" filename))
      (let* ((layer-count (gdal-dataset-get-layer-count ds))
             (polygons (gdal-dataset-get-layer ds 0)))
        (unless (eql layer-count 1)
          (error "Unexpected layer count ~a" layer-count))
        (cond
          ((< 0 (ogr-l-test-capability polygons OLCFastSpatialFilter))
           ;; Succeed only if we have an index
           (log2:info "Loaded layer ~a with ~a features"
                      (ogr-l-get-name polygons)
                      (ogr-l-get-feature-count polygons))
           (values (make-mapp :polygons polygons
                             :filename filename)))
          (t
           (error "Layer not found or no index")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS-LAND
;;  Return TRUE iff point is on land


(let ((cached-ogr-point (ogr-g-create-geometry wkbPoint)))

  (defun is-land (point &key (map *map*))
    (let ((mapdata (mapp-polygons map))
          (lat (latlng-lat point))
          (lon (latlng-lng point))
          (ogr-point  (ogr-g-create-geometry wkbPoint)))
      (bordeaux-threads:with-lock-held (+map-lock+)
        (ogr-g-set-point-2d ogr-point 0 lon lat)
        (ogr-l-set-spatial-filter mapdata ogr-point)
        (ogr-l-reset-reading mapdata)
        (loop
           :for feature = (ogr-l-get-next-feature mapdata)
           :while (not (null-pointer-p feature))
           :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                      (found (ogr-g-contains polygon ogr-point)))
                 (ogr-f-destroy feature)
                 (ogr-f-destroy ogr-point)
                 (when found
                   (return-from is-land t))))))
    (return-from is-land nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINE-INTERSECTS-LAND-P

(defparameter +cached-segment+ (ogr-g-create-geometry wkbLineString))
(eval-when (:execute)
  (ogr-g-add-point-2d +cached-segment+ 0d0 0d0)
  (ogr-g-add-point-2d +cached-segment+ 0d0 0d0))

(declaim (inline line-intersects-land-p))
(defun line-intersects-land-p (start end &key (map *map*))
  (let ((mapdata (mapp-polygons map))
        (segment (ogr-g-create-geometry wkbLineString)))
    (bordeaux-threads:with-lock-held (+map-lock+)
        (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
        (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter mapdata segment)
      (ogr-l-reset-reading mapdata)
      ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
      ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
      (loop
        :for feature = (ogr-l-get-next-feature mapdata)
        :while (not (null-pointer-p feature))
        :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                   (found (ogr-g-intersects polygon segment)))
              (ogr-f-destroy segment)
              (ogr-f-destroy feature)
              (when found
                (return-from line-intersects-land-p t))))))
  (return-from line-intersects-land-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECTANGLE-INTERSECTS-LAND-P

(defun rectangle-intersects-land-p (lat-nw lng-nw lat-se lng-se &key (map *map*))
  (let ((segment (ogr-g-create-geometry wkbLinearRing))
        (poly (ogr-g-create-geometry wkbPolygon))
        (mapdata (mapp-polygons map)))
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-nw)
    (ogr-g-add-point-2d segment lng-se lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-se)
    (ogr-g-add-point-2d segment lng-nw lat-nw)
    (ogr-g-add-geometry poly segment)
    (bordeaux-threads:with-lock-held (+map-lock+)
      
      (ogr-l-set-spatial-filter mapdata poly)
      (ogr-l-reset-reading mapdata)
      ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
      ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
      (loop
         :for feature = (ogr-l-get-next-feature mapdata)
         :while (not (null-pointer-p feature))
         :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                    (found (ogr-g-intersects polygon poly)))
               (ogr-f-destroy feature)
               (when found
                 (ogr-f-destroy poly)
                 (return-from rectangle-intersects-land-p
                   (not (mapp-land-inverted map)))))))
    (ogr-f-destroy poly))
  (return-from rectangle-intersects-land-p (mapp-land-inverted map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intersection point

(defun line-land-intersection (start end &key (map *map*))
  (let ((segment (ogr-g-create-geometry wkbLineString))
        (mapdata (mapp-polygons map)))
    (ogr-g-add-point-2d segment 0d0 0d0)
    (ogr-g-add-point-2d segment 0d0 0d0)
    ;; Check if and where a line intersects land. The start point should be on
    ;; sea.
    ;; The result geometry is a LINESTRING if the line (start, end) enters
    ;; land one time, or a MULTILINESTRING if the line enters and leaves land
    ;; multiple times.
    ;; The first point of the (first) linestring is the point where the line
    ;; hits land for the first time.
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
      (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter mapdata segment)
      (ogr-l-reset-reading mapdata)
      (nearest-point segment (latlng-lat start) (latlng-lng start) mapdata))))

(defun nearest-point (segment lat0 lng0 mapdata)
  (with-foreign-objects ((ogr-lat :double)
                         (ogr-lon :double)
                         (ogr-z :double))
    (let ((min-dist nil)
          (min-lat)
          (min-lng)
          (lat)
          (lng))
      (flet ((nearest-point-linestring (linestring)
               (let ((count (ogr-g-get-point-count linestring)))
                 (loop
                    :for k :below count
                    :do (progn
                          (ogr-g-get-point linestring k ogr-lon ogr-lat ogr-z)
                          (setf lat (mem-ref ogr-lat :double))
                          (setf lng (mem-ref ogr-lon :double))
                          (let ((dist (+ (expt (- lat0 lat) 2)
                                         (expt (- lng0 lng) 2))))
                            (when (or (null min-dist)
                                      (< dist min-dist))
                              (log2:debug "NEAREST: ~,4f,~,4f for ~,4f,~,4f" lat lng lat0 lng0)
                              (setf min-dist dist
                                    min-lat lat
                                    min-lng lng)))))))
             (nearest-point-multipoint (multipoint)
               (loop
                 :for k :from 0
                 :for geom = (ogr-g-get-geometry-ref multipoint k)
                 :while (not (null-pointer-p geom))
                 :do (progn
                       (ogr-g-get-point geom 0 ogr-lon ogr-lat ogr-z)
                       (setf lat (mem-ref ogr-lat :double))
                       (setf lng (mem-ref ogr-lon :double))
                       (let ((dist (+ (expt (- lat0 lat) 2)
                                      (expt (- lng0 lng) 2))))
                         (when (or (null min-dist)
                                   (< dist min-dist))
                           (log2:debug "NEAREST: ~,4f,~,4f for ~,4f,~,4f" lat lng lat0 lng0)
                           (setf min-dist dist
                                 min-lat lat
                                 min-lng lng)))))))
        (loop
          :for feature = (ogr-l-get-next-feature mapdata)
          :while (not (null-pointer-p feature))
          :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                     (intersects (ogr-g-intersects segment polygon)))
                (when intersects
                  (let*  ((intersection (ogr-g-intersection segment polygon))
                          (count  (ogr-g-get-point-count intersection))
                          (intersection-name (ogr-g-get-geometry-name intersection)))
                    (cond
                      ((string= intersection-name "MULTILINESTRING")
                       (let ((count (ogr-g-get-geometry-count intersection)))
                         (loop
                           :for k :below count
                           :do (nearest-point-linestring (ogr-g-get-geometry-ref intersection k)))))
                      ((or (string= intersection-name "LINESTRING")
                           (string= intersection-name "POINT"))
                       (nearest-point-linestring intersection))
                      ((string= intersection-name "MULTIPOINT")
                       (nearest-point-multipoint intersection))
                      (T
                       (error "Unexpected geometry ~a" intersection-name)))
                    (ogr-f-destroy intersection)))
                (ogr-f-destroy feature)))
        (when min-lat
          (values t
                  (make-latlng :lat  min-lat :lng min-lng)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fast land detection

(defun point-on-land-p (point &key (map *map*))
  (when (tile-intersects-land-p (latlng-lat point) (latlng-lng point) :map map)
    (is-land point :map map)))

(declaim (inline intersects-land-p))
(defun intersects-land-p (start end &key (map *map*))
  ;; Check if the line from $start to $end intersects land.
  ;; This implementation assumes that $start and $end are on the same tile.
  (when
      ;; When there is any land in the tile containing point...
      (tile-intersects-land-p (latlng-lat end) (latlng-lng end) :map map)
    ;; ... look closer.
    (line-intersects-land-p start end :map map)))

;; Tiling used to speed up land check.
;; The smaller the tiles, the bigger the probability to skip over land!
(defvar +tile-width+ 0.1d0)
(defvar +tile-num+ 10)
(defvar +maxnorth+ (* 180 +tile-num+))
(defvar +maxwest+ (* 360 +tile-num+))

(defparameter *tile-array* (make-array (list +maxnorth+ +maxwest+)
                                       :initial-element :unknown))

(defparameter *miss* 0)
(defparameter *hit* 0)

(declaim (inline tile-intersects-land-p))
(defun tile-intersects-land-p (lat lng &key (map *map*))
  (let* ((north (floor lat +tile-width+))
         (west (floor lng +tile-width+))
         (i-north (if (< north 0) (+ north +maxnorth+) north))
         (i-west (if (< west 0) (+ west +maxwest+) west)))
    (let ((land-p (aref *tile-array* i-north i-west)))
      (cond ((eq land-p :unknown)
             (incf *miss*)
             (let* ((south (ceiling lat +tile-width+))
                    (east (ceiling lng +tile-width+))
                    (land-p (rectangle-intersects-land-p (* north +tile-width+)
                                                         (* west +tile-width+)
                                                         (* south +tile-width+)
                                                         (* east +tile-width+)
                                                         :map map)))
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
