;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2022-02-01 00:38:42>
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-map)

(check-equal (land-tile-p 30d0 10d0 29d0 11d0)
             t)

(check-equal (land-tile-p 39d0 -8d0 38d0 -7d0)
             t)

(check-equal (land-tile-p 38d0 -9d0 37d0 -8d0)
             nil)

(check-equal (land-tile-p 32d0 11d0 31d0 12d0)
             nil)

(check-equal (land-tile-p 21d0 9d0 20d0 10d0)
             t)

(check-equal (tile-inside-polygon-p 30d0 10d0 29d0 11d0
                                    (aref *vecmaps* 0))
             t)

(check-equal (tile-inside-polygon-p 30d0 10d0 29d0 11d0
                                    (aref *vecmaps* 1))
             nil)

(check-equal (tile-inside-polygon-p 39d0 -8d0 38d0 -7d0
                                    (find "eurasia" *vecmaps* :key #'vecmap-name :test #'string-equal))
             t)

(check-equal (tile-inside-polygon-p 38d0 -9d0 37d0 -8d0
                                    (find "eurasia" *vecmaps* :key #'vecmap-name :test #'string-equal))
             nil)

(check-equal (tile-inside-polygon-p 32d0 11d0 31d0 12d0
                                    (find "africa" *vecmaps* :key #'vecmap-name :test #'string-equal))
             nil)

(check-equal (tile-inside-polygon-p 21d0 9d0 20d0 10d0
                                    (find "africa" *vecmaps* :key #'vecmap-name :test #'string-equal))
             t)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
