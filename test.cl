;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2025-10-16 21:25:02>
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-map)

(check-equal (not (null (land-tile-p 30d0 10d0 29d0 11d0)))
             t)

(check-equal (not (null (land-tile-p 39d0 -8d0 38d0 -7d0)))
             t)

(check-equal (not (null (land-tile-p 38d0 -9d0 37d0 -8d0)))
             nil)

(check-equal (not (null (land-tile-p 32d0 11d0 31d0 12d0)))
             nil)

(check-equal (not (null (land-tile-p 21d0 9d0 20d0 10d0)))
             t)

(check-equal (not (null (tile-inside-polygon-p 30d0 10d0 29d0 11d0
                                    (aref *vecmaps* 0))))
             t)

(check-equal (not (null (tile-inside-polygon-p 30d0 10d0 29d0 11d0
                                    (aref *vecmaps* 1))))
             nil)

(check-equal (not (null (tile-inside-polygon-p 39d0 -8d0 38d0 -7d0
                                    (find "eurasia" *vecmaps* :key #'vecmap-name :test #'string-equal))))
             t)

(check-equal (not (null (tile-inside-polygon-p 38d0 -9d0 37d0 -8d0
                                    (find "eurasia" *vecmaps* :key #'vecmap-name :test #'string-equal))))
             nil)

(check-equal (not (null (tile-inside-polygon-p 32d0 11d0 31d0 12d0
                                    (find "africa" *vecmaps* :key #'vecmap-name :test #'string-equal))))
             nil)

(check-equal (not (null (tile-inside-polygon-p 21d0 9d0 20d0 10d0
                                    (find "africa" *vecmaps* :key #'vecmap-name :test #'string-equal))))
             t)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
