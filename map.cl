;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2025-11-02 16:43:48>

(in-package :cl-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map 

(defstruct mapp
  polygons dataset filename
  (land-inverted nil) (closed nil) cache coarse-tiles coarse-width)

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

(defun check-open-map (map)
  "Reject closed/missing map handles before entering native code. Caller holds the map lock."
  (unless (and map (not (mapp-closed map)) (mapp-polygons map))
    (error "Map is not open")))

(defun %clear-map-cache (map)
  "Release a map's prepared and coarse indices under the shared map lock."
  (when map
    (release-vector-cache (mapp-cache map))
    (setf (mapp-cache map) nil (mapp-coarse-tiles map) nil)))

(defun clear-map-cache (&optional (map *map*))
  "Clear one map's cache without closing the dataset or disturbing other maps."
  (bordeaux-threads:with-lock-held (+map-lock+) (%clear-map-cache map))
  t)

(defun %close-map (map)
  "Destroy cache before dataset; repeated closes are harmless. Caller holds the map lock."
  (when (and map (not (mapp-closed map)))
    (%clear-map-cache map)
    (when (mapp-dataset map) (gdal-close (mapp-dataset map)))
    (setf (mapp-polygons map) nil (mapp-dataset map) nil (mapp-closed map) t)))

(defun close-map (&optional (map *map*))
  "Close an owned map and all its native cache resources, serialized with queries."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (%close-map map)
    (when (eq map *map*) (setf *map* nil)))
  t)

(defun %load-map (filename inverted)
  "Open an indexed dataset transactionally. Caller holds the map lock."
  (gdal-all-register)
  (let* ((filename (namestring (truename filename)))
         (ds (gdal-open-ex filename GDAL_OF_VECTOR (null-pointer) (null-pointer) (null-pointer)))
         (transferred nil))
    (when (null-pointer-p ds) (error "Failed to open ~a" filename))
    (unwind-protect
         (let ((layer (gdal-dataset-get-layer ds 0)))
           (unless (and (= (gdal-dataset-get-layer-count ds) 1) (not (null-pointer-p layer))
                        (plusp (ogr-l-test-capability layer OLCFastSpatialFilter)))
             (error "Map layer not found or no spatial index: ~a" filename))
           (log2:info "Loaded indexed land layer ~a from ~a" (ogr-l-get-name layer) filename)
           (prog1 (make-mapp :polygons layer :dataset ds :filename filename :land-inverted inverted)
             (setf transferred t)))
      (unless transferred (gdal-close ds)))))

(defun load-map (filename &key (inverted nil))
  "Open an independent indexed map; CLOSE-MAP owns its eventual cleanup."
  (bordeaux-threads:with-lock-held (+map-lock+) (%load-map filename inverted)))

(defun ensure-map (&key (filename *map-file*) (inverted nil) reload)
  "Open/reload the active map, retaining the old one if opening the replacement fails."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (when (or reload (null *map*) (mapp-closed *map*)
              (not (equal (mapp-filename *map*) (namestring (truename filename))))
              (not (eq inverted (mapp-land-inverted *map*))))
      (let ((new (%load-map filename inverted)) (old *map*))
        (setf *map* new)
        (%close-map old)))
    *map*))

(defun map-vector-cache (map)
  "Return the map-specific candidate index, invalidating it when cell size changes."
  (unless (and (plusp (ogr-has-prepared-geometry-support))
               (plusp (ogr-l-test-capability (mapp-polygons map) "RandomRead")))
    (return-from map-vector-cache nil))
  (unless (and (realp *prepared-cache-cell-size*) (> *prepared-cache-cell-size* 0))
    (error "Prepared-cache cell size must be positive"))
  (when (and (mapp-cache map) (/= (vector-cache-cell (mapp-cache map)) *prepared-cache-cell-size*))
    (release-vector-cache (mapp-cache map)) (setf (mapp-cache map) nil))
  (or (mapp-cache map)
      (setf (mapp-cache map) (make-vector-cache :layer (mapp-polygons map) :cell (coerce *prepared-cache-cell-size* 'double-float)))))

(defun indexed-vector-query (map geometry predicate)
  "Run the original exact indexed predicate, with one destruction per feature."
  (let ((layer (mapp-polygons map)))
    (cpl-error-reset)
    (ogr-l-set-spatial-filter layer geometry) (ogr-l-reset-reading layer)
    (loop for feature = (ogr-l-get-next-feature layer) until (null-pointer-p feature) do
      (unwind-protect
           (let ((found (if (eq predicate :contains)
                            (ogr-g-contains (ogr-f-get-geometry-ref feature) geometry)
                            (ogr-g-intersects (ogr-f-get-geometry-ref feature) geometry))))
             (check-map-error "testing indexed land geometry")
             (when found (return-from indexed-vector-query t)))
        (ogr-f-destroy feature)))
    (check-map-error "reading indexed land geometry")
    nil))

(defun map-vector-query (map geometry west south east north predicate)
  "Use prepared predicates when possible; otherwise retain exact indexed access."
  (trim-prepared-cache)
  (when (and *prepared-cache-enabled* (plusp *prepared-cache-max-bytes*))
    (let ((cache (map-vector-cache map)))
      (when cache
        (multiple-value-bind (found handled) (prepared-vector-query cache geometry west south east north predicate)
          (when handled (return-from map-vector-query found))))
      (unless cache (incf (cache-lru-fallbacks *prepared-cache-pool*)))))
  (indexed-vector-query map geometry predicate))

(defun is-land (point &key (map *map*))
  "Test strict point containment with the same exact prepared/indexed semantics."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map)
    (cpl-error-reset)
    (let ((geometry (ogr-g-create-geometry wkbPoint)) (x (latlng-lng point)) (y (latlng-lat point)))
      (unwind-protect
           (progn (ogr-g-set-point-2d geometry 0 x y)
                  (check-map-error "constructing land query point")
                  (map-vector-query map geometry x y x y :contains))
        (ogr-g-destroy-geometry geometry)))))

(defun %line-intersects-land-p (x0 y0 x1 y1 map)
  "Check one non-wrapping segment while holding the map lock."
  (cpl-error-reset)
  (let ((segment (ogr-g-create-geometry wkbLineString)))
    (unwind-protect
         (progn (ogr-g-set-point-2d segment 0 x0 y0) (ogr-g-set-point-2d segment 1 x1 y1)
                (check-map-error "constructing land query segment")
                (map-vector-query map segment (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1) :intersects))
      (ogr-g-destroy-geometry segment))))

(defun %line-query (start end map)
  "Split an antimeridian crossing into two short, physically adjacent segments."
  (let* ((x0 (latlng-lng start)) (x1 (latlng-lng end)) (y0 (latlng-lat start)) (y1 (latlng-lat end))
         (delta (- x1 x0)))
    (if (<= (abs delta) 180d0) (%line-intersects-land-p x0 y0 x1 y1 map)
        (let* ((boundary (if (plusp delta) -180d0 180d0))
               (unwrapped (+ x1 (if (plusp delta) -360d0 360d0)))
               (fraction (/ (- boundary x0) (- unwrapped x0)))
               (y (+ y0 (* fraction (- y1 y0)))))
          (or (%line-intersects-land-p x0 y0 boundary y map)
              (%line-intersects-land-p (- boundary) y x1 y1 map))))))

(declaim (notinline line-intersects-land-p intersects-land-p tile-intersects-land-p))
(defun line-intersects-land-p (start end &key (map *map*))
  "Test an entire sailing segment with exact, cache-backed intersection predicates."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map) (%line-query start end map)))

(defun %rectangle-intersects-land-p (west south east north map &optional (cached t))
  "Create and destroy one rectangle/ring; coarse presence checks use uncached access."
  (cpl-error-reset)
  (let ((ring (ogr-g-create-geometry wkbLinearRing)) (poly (ogr-g-create-geometry wkbPolygon)))
    (unwind-protect
         (progn
           (dolist (p (list (cons west north) (cons east north) (cons east south) (cons west south) (cons west north)))
             (ogr-g-add-point-2d ring (car p) (cdr p)))
           (unless (zerop (ogr-g-add-geometry poly ring)) (error "Cannot create land query rectangle"))
           (check-map-error "constructing land query rectangle")
           (let ((found (if cached (map-vector-query map poly west south east north :intersects)
                            (indexed-vector-query map poly :intersects))))
             (if (mapp-land-inverted map) (not found) found)))
      (ogr-g-destroy-geometry ring) (ogr-g-destroy-geometry poly))))

(defun rectangle-intersects-land-p (lat-nw lng-nw lat-se lng-se &key (map *map*))
  "Test exact rectangle intersection, preserving the map's inversion setting."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map)
    (%rectangle-intersects-land-p (min lng-nw lng-se) (min lat-nw lat-se)
                                (max lng-nw lng-se) (max lat-nw lat-se) map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intersection point

(defun line-land-intersection (start end &key (map *map*))
  "Return the nearest intersection vertex, releasing the query on all exits."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map)
    (let ((segment (ogr-g-create-geometry wkbLineString)) (layer (mapp-polygons map)))
      (unwind-protect
           (progn
             (cpl-error-reset)
             (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
             (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
             (ogr-l-set-spatial-filter layer segment) (ogr-l-reset-reading layer)
             (nearest-point segment (latlng-lat start) (latlng-lng start) layer))
        (ogr-g-destroy-geometry segment)))))

(defun nearest-point (segment lat0 lng0 layer)
  "Inspect owned intersection geometries while keeping borrowed child handles alive."
  (with-foreign-objects ((lat :double) (lon :double) (z :double))
    (let ((best nil) (best-lat nil) (best-lon nil))
      (labels ((visit (geometry)
                 (let ((name (ogr-g-get-geometry-name geometry)))
                   (cond
                     ((member name '("LINESTRING" "POINT") :test #'string=)
                      (dotimes (i (ogr-g-get-point-count geometry))
                        (ogr-g-get-point geometry i lon lat z)
                        (let* ((y (mem-ref lat :double)) (x (mem-ref lon :double))
                               (distance (+ (expt (- lat0 y) 2) (expt (- lng0 x) 2))))
                          (when (or (null best) (< distance best))
                            (setf best distance best-lat y best-lon x)))))
                     ((member name '("MULTILINESTRING" "MULTIPOINT" "GEOMETRYCOLLECTION") :test #'string=)
                      (dotimes (i (ogr-g-get-geometry-count geometry))
                        (visit (ogr-g-get-geometry-ref geometry i))))
                     (t (error "Unexpected land intersection geometry ~a" name))))))
        (loop for feature = (ogr-l-get-next-feature layer) until (null-pointer-p feature) do
          (unwind-protect
               (let* ((polygon (ogr-f-get-geometry-ref feature))
                      (intersection (ogr-g-intersection segment polygon)))
                 (when (null-pointer-p intersection) (error "Cannot calculate land intersection"))
                 (unwind-protect (visit intersection) (ogr-g-destroy-geometry intersection)))
            (ogr-f-destroy feature)))
        (check-map-error "calculating land intersection")
        (when best (values t (make-latlng :lat best-lat :lng best-lon)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fast land detection

(defvar +tile-width+ 0.1d0)
(defvar *miss* 0)
(defvar *hit* 0)

(defun %tile-intersects-land-p (lat lng map)
  "Cache complete coarse cells per map: 0 unknown, 1 water, 2 land. Caller holds lock."
  (let* ((width +tile-width+) (rows (ceiling 180d0 width)) (cols (ceiling 360d0 width))
         (row (max 0 (min (1- rows) (floor (+ lat 90d0) width))))
         (col (max 0 (min (1- cols) (floor (+ lng 180d0) width)))))
    (unless (and (mapp-coarse-tiles map) (eql width (mapp-coarse-width map)))
      (setf (mapp-coarse-width map) width
            (mapp-coarse-tiles map) (make-array (list rows cols) :element-type '(unsigned-byte 2) :initial-element 0)))
    (let ((value (aref (mapp-coarse-tiles map) row col)))
      (if (zerop value)
          (let* ((south (- (* row width) 90d0)) (west (- (* col width) 180d0))
                 (land (%rectangle-intersects-land-p west south (min 180d0 (+ west width))
                                                    (min 90d0 (+ south width)) map nil)))
            (incf *miss*)
            (setf (aref (mapp-coarse-tiles map) row col) (if land 2 1))
            land)
          (progn (incf *hit*) (= value 2))))))

(defun tile-intersects-land-p (lat lng &key (map *map*))
  "Read the synchronized, map-specific coarse presence cache."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map) (trim-prepared-cache) (%tile-intersects-land-p lat lng map)))

(defun point-on-land-p (point &key (map *map*))
  "Use coarse presence to skip open ocean before exact point containment."
  (when (tile-intersects-land-p (latlng-lat point) (latlng-lng point) :map map)
    (is-land point :map map)))

(defun intersects-land-p (start end &key (map *map*))
  "Preserve the endpoint-tile shortcut, using cached exact checks when coastal."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (check-open-map map)
    (trim-prepared-cache)
    (when (%tile-intersects-land-p (latlng-lat end) (latlng-lng end) map)
      (%line-query start end map))))

(defun prepared-cache-statistics (&optional (map *map*))
  "Return a cheap synchronized snapshot; native bytes are conservatively estimated."
  (bordeaux-threads:with-lock-held (+map-lock+)
    (let* ((cache (when map (mapp-cache map))) (tiles (when cache (vector-cache-tile-lru cache)))
           (pool *prepared-cache-pool*))
      (list :enabled *prepared-cache-enabled* :limit-bytes *prepared-cache-max-bytes*
            :estimated-bytes (cache-lru-bytes pool) :payload-bytes (cache-lru-payload pool)
            :entries (cache-lru-count pool) :hits (cache-lru-hits pool) :misses (cache-lru-misses pool)
            :evictions (cache-lru-evictions pool) :fallbacks (cache-lru-fallbacks pool)
            :tiles (if tiles (cache-lru-count tiles) 0)
            :tile-index-bytes (if tiles (cache-lru-bytes tiles) 0)
            :coarse-bytes (if (and map (mapp-coarse-tiles map))
                              (ceiling (array-total-size (mapp-coarse-tiles map)) 4) 0)))))
