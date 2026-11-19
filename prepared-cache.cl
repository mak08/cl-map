;;; Bounded exact-vector cache. All entry points below require +MAP-LOCK+.
(in-package :cl-map)

(defparameter *prepared-cache-enabled* t "Use cached exact prepared polygon predicates.")
(defparameter *prepared-cache-max-bytes* (* 2000 1024 1024)
  "Global conservative native-memory accounting budget across open maps, bytes.
Charged as 16 times WKB size plus 4096 bytes per feature; not a measured RSS cap.")
(defparameter *prepared-cache-max-entries* 50000 "Global prepared-feature/negative-entry limit.")
(defparameter *prepared-cache-cell-size* 0.25d0 "Spatial candidate-cache cell width, degrees.")
(defparameter *prepared-cache-tile-bytes* (* 32 1024 1024) "Per-map candidate-index accounting budget.")
(defparameter *prepared-cache-max-tiles* 4096 "Per-map spatial candidate-cell limit.")
(defparameter *prepared-cache-max-query-tiles* 256 "Use indexed fallback for larger query boxes.")

(defstruct cache-entry key owner geometry prepared (payload 0) (charge 0) ids prev next)
(defstruct cache-lru head tail (count 0) (bytes 0) (payload 0)
  (hits 0) (misses 0) (evictions 0) (fallbacks 0))
(defvar *prepared-cache-pool* (make-cache-lru))
(defstruct vector-cache layer (cell *prepared-cache-cell-size*)
  (features (make-hash-table)) (tiles (make-hash-table :test #'equal))
  (tile-lru (make-cache-lru)) (seen (make-hash-table)))

(defun cache-unlink (lru entry)
  "Detach a resident entry, updating list/accounting but not destroying handles."
  (let ((prev (cache-entry-prev entry)) (next (cache-entry-next entry)))
    (if prev (setf (cache-entry-next prev) next) (setf (cache-lru-head lru) next))
    (if next (setf (cache-entry-prev next) prev) (setf (cache-lru-tail lru) prev)))
  (decf (cache-lru-count lru)) (decf (cache-lru-bytes lru) (cache-entry-charge entry))
  (decf (cache-lru-payload lru) (cache-entry-payload entry))
  (setf (cache-entry-prev entry) nil (cache-entry-next entry) nil))

(defun cache-link (lru entry)
  "Insert an entry at the most-recent end of an LRU list."
  (setf (cache-entry-prev entry) nil (cache-entry-next entry) (cache-lru-head lru))
  (when (cache-lru-head lru) (setf (cache-entry-prev (cache-lru-head lru)) entry))
  (unless (cache-lru-tail lru) (setf (cache-lru-tail lru) entry))
  (setf (cache-lru-head lru) entry)
  (incf (cache-lru-count lru)) (incf (cache-lru-bytes lru) (cache-entry-charge entry))
  (incf (cache-lru-payload lru) (cache-entry-payload entry))
  entry)

(defun cache-touch (lru entry)
  "Keep an existing entry hot without changing its resource ownership."
  (unless (eq entry (cache-lru-head lru)) (cache-unlink lru entry) (cache-link lru entry))
  entry)

(defun release-prepared-entry (entry &optional eviction)
  "Unlink a feature globally, then destroy its prepared handle and OGR clone."
  (cache-unlink *prepared-cache-pool* entry)
  (remhash (cache-entry-key entry) (vector-cache-features (cache-entry-owner entry)))
  (when eviction (incf (cache-lru-evictions *prepared-cache-pool*)))
  (when (cache-entry-prepared entry) (ogr-destroy-prepared-geometry (cache-entry-prepared entry)))
  (when (cache-entry-geometry entry) (ogr-g-destroy-geometry (cache-entry-geometry entry))))

(defun trim-prepared-cache (&optional (reserve 0) (entries 0))
  "Evict before allocation, respecting updated limits and the shared map budget."
  (let ((limit (if *prepared-cache-enabled* (max 0 *prepared-cache-max-bytes*) 0)))
    (loop while (and (cache-lru-tail *prepared-cache-pool*)
                     (or (> (+ reserve (cache-lru-bytes *prepared-cache-pool*)) limit)
                         (> (+ entries (cache-lru-count *prepared-cache-pool*))
                            (max 0 *prepared-cache-max-entries*))))
          do (release-prepared-entry (cache-lru-tail *prepared-cache-pool*) t))))

(defun release-vector-cache (cache)
  "Release all native resources belonging to one map, leaving other maps intact."
  (when cache
    (let ((entries (loop for entry being the hash-values of (vector-cache-features cache) collect entry)))
      (dolist (entry entries) (release-prepared-entry entry)))
    (clrhash (vector-cache-tiles cache)) (clrhash (vector-cache-seen cache))))

(defun admit-prepared-feature (cache id geometry)
  "Cache a cloned/prepared feature; oversize or unsupported features get a fallback marker."
  (let* ((payload (ogr-g-wkb-size geometry))
         (charge (+ 4096 (* 16 payload))) (copy nil) (prepared nil) (entry nil) (linked nil) (transferred nil))
    (when (or (<= *prepared-cache-max-bytes* 0) (<= *prepared-cache-max-entries* 0))
      (return-from admit-prepared-feature nil))
    (when (or (<= payload 0) (> charge *prepared-cache-max-bytes*)) (setf charge 4096 payload 0))
    (when (> charge *prepared-cache-max-bytes*) (return-from admit-prepared-feature nil))
    (trim-prepared-cache charge 1)
    (unwind-protect
         (progn
           (when (plusp payload)
             (setf copy (ogr-g-clone geometry))
             (unless (null-pointer-p copy)
               (setf prepared (ogr-create-prepared-geometry copy)))
             (when (or (null-pointer-p copy) (null prepared) (null-pointer-p prepared))
               (unless (null-pointer-p copy) (ogr-g-destroy-geometry copy))
               (setf copy nil prepared nil payload 0 charge 4096)
               (cpl-error-reset)))
           (setf entry (make-cache-entry :key id :owner cache :geometry copy :prepared prepared
                                          :payload payload :charge charge))
           (setf (gethash id (vector-cache-features cache)) entry)
           (cache-link *prepared-cache-pool* entry)
           (setf linked t transferred t)
           entry)
      (unless transferred
        (remhash id (vector-cache-features cache))
        (when linked (cache-unlink *prepared-cache-pool* entry))
        (when (and prepared (not (null-pointer-p prepared))) (ogr-destroy-prepared-geometry prepared))
        (when (and copy (not (null-pointer-p copy))) (ogr-g-destroy-geometry copy))))))

(defun cached-prepared-feature (cache id)
  "Find a prepared feature, rereading an evicted ID without depending on layer iteration."
  (let ((entry (gethash id (vector-cache-features cache))))
    (if entry
        (progn (incf (cache-lru-hits *prepared-cache-pool*)) (cache-touch *prepared-cache-pool* entry))
        (let ((feature (ogr-l-get-feature (vector-cache-layer cache) id)))
          (incf (cache-lru-misses *prepared-cache-pool*))
          (when (null-pointer-p feature) (error "Cannot read land feature ~d" id))
          (unwind-protect (admit-prepared-feature cache id (ogr-f-get-geometry-ref feature))
            (ogr-f-destroy feature))))))

(defun evict-vector-tile (cache)
  "Drop the least-used cell index; feature ownership is independent of tile entries."
  (let* ((lru (vector-cache-tile-lru cache)) (entry (cache-lru-tail lru)))
    (cache-unlink lru entry) (remhash (cache-entry-key entry) (vector-cache-tiles cache))
    (incf (cache-lru-evictions lru))))

(defun vector-tile-ids (cache x y)
  "Index a complete cell through GDAL; NIL is cached as a valid empty ocean cell."
  (let* ((key (cons x y)) (lru (vector-cache-tile-lru cache))
         (existing (gethash key (vector-cache-tiles cache))))
    (when existing
      (incf (cache-lru-hits lru)) (cache-touch lru existing)
      (return-from vector-tile-ids (cache-entry-ids existing)))
    (incf (cache-lru-misses lru))
    (let ((layer (vector-cache-layer cache)) (cell (vector-cache-cell cache))
          (ids nil) (count 0) (oversize nil))
      (cpl-error-reset)
      (ogr-l-set-spatial-filter-rect layer (* x cell) (* y cell) (* (1+ x) cell) (* (1+ y) cell))
      (ogr-l-reset-reading layer)
      (loop for feature = (ogr-l-get-next-feature layer) until (null-pointer-p feature) do
        (unwind-protect
             (let ((id (ogr-f-get-fid feature)))
               (when (or (minusp id) (> (incf count) 16384)) (setf oversize t) (return))
               (push id ids)
               (unless (gethash id (vector-cache-features cache))
                 (incf (cache-lru-misses *prepared-cache-pool*))
                 (admit-prepared-feature cache id (ogr-f-get-geometry-ref feature))))
          (ogr-f-destroy feature)))
      (check-map-error "reading land candidate cell")
      (let* ((ids (if oversize :fallback (coerce ids 'vector)))
             (charge (+ 256 (if oversize 0 (* 16 count))))
             (entry (make-cache-entry :key key :owner cache :ids ids :charge charge)))
        (if (or (> charge *prepared-cache-tile-bytes*) (<= *prepared-cache-max-tiles* 0))
            :fallback
            (progn
              (loop while (and (cache-lru-tail lru)
                               (or (>= (cache-lru-count lru) *prepared-cache-max-tiles*)
                                   (> (+ charge (cache-lru-bytes lru)) *prepared-cache-tile-bytes*)))
                    do (evict-vector-tile cache))
              (setf (gethash key (vector-cache-tiles cache)) entry)
              (cache-link lru entry)
              ids))))))

(defun prepared-vector-query (cache geometry west south east north predicate)
  "Return result and handled-p. Large boxes/uncacheable features use the exact indexed fallback."
  (trim-prepared-cache)
  (let ((lru (vector-cache-tile-lru cache)))
    (loop while (and (cache-lru-tail lru)
                     (or (> (cache-lru-count lru) (max 0 *prepared-cache-max-tiles*))
                         (> (cache-lru-bytes lru) (max 0 *prepared-cache-tile-bytes*))))
          do (evict-vector-tile cache)))
  (unless (and *prepared-cache-enabled* (> *prepared-cache-max-bytes* 0)
               (> *prepared-cache-max-entries* 0))
    (return-from prepared-vector-query (values nil nil)))
  (let* ((cell (vector-cache-cell cache)) (x0 (floor west cell)) (x1 (floor east cell))
         (y0 (floor south cell)) (y1 (floor north cell)))
    (unless (<= (* (1+ (- x1 x0)) (1+ (- y1 y0))) *prepared-cache-max-query-tiles*)
      (incf (cache-lru-fallbacks *prepared-cache-pool*))
      (return-from prepared-vector-query (values nil nil)))
    (clrhash (vector-cache-seen cache))
    ;; Build all indices before looking up IDs: OGR_L_GetFeature interrupts iteration.
    (let ((ids nil))
      (loop for y from y0 to y1 do
        (loop for x from x0 to x1 do
          (let ((tile (vector-tile-ids cache x y)))
            (when (eq tile :fallback)
              (incf (cache-lru-fallbacks *prepared-cache-pool*))
              (return-from prepared-vector-query (values nil nil)))
            (loop for id across tile unless (gethash id (vector-cache-seen cache)) do
              (setf (gethash id (vector-cache-seen cache)) t) (push id ids)
              (when (> (hash-table-count (vector-cache-seen cache)) 65536)
                (incf (cache-lru-fallbacks *prepared-cache-pool*))
                (return-from prepared-vector-query (values nil nil)))))))
      (dolist (id ids)
        (let ((entry (cached-prepared-feature cache id)))
          (unless (and entry (cache-entry-prepared entry))
            (incf (cache-lru-fallbacks *prepared-cache-pool*))
            (return-from prepared-vector-query (values nil nil)))
          (cpl-error-reset)
          (let ((found (if (eq predicate :contains)
                           (ogr-prepared-geometry-contains (cache-entry-prepared entry) geometry)
                           (ogr-prepared-geometry-intersects (cache-entry-prepared entry) geometry))))
            (check-map-error "testing prepared land geometry")
            (when (plusp found) (return-from prepared-vector-query (values t t))))))
      (values nil t))))
