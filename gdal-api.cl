;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-04-22 00:43:01>

(in-package :cl-map)

(defvar wkbPoint 1)
(defvar wkbLineString 2)
(defvar wkbPolygon 3)
(defvar wkbLinearRing 101)
(defvar GDAL_OF_VECTOR 4)
(defvar OLCFastSpatialFilter "FastSpatialFilter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization & Files

(defcfun ("GDALAllRegister" gdal-all-register) :void)

(defcfun ("GDALOpenEx" gdal-open-ex) :pointer
  (filename :string)
  (open-flags :uint)
  (allowed-drivers :string)
  (open-options :string)
  (sibling-files :string))

(defcfun ("GDALClose" gdal-close) :void
  (datasource :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dataset/Datasource

(defcfun ("GDALDatasetGetLayerCount" gdal-dataset-get-layer-count) :int
  (datasource :pointer))

(defcfun ("GDALDatasetGetLayer" gdal-dataset-get-layer) :pointer
  (dataset :pointer)
  (layer :int))

(defcfun ("GDALDatasetGetLayerByName"  gdal-dataset-get-layer-by-name) :pointer
  (dataset :pointer)
  (layer-name :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layer

(defcfun ("OGR_L_SetSpatialFilter" ogr-l-set-spatial-filter) :void
  (layer :pointer)
  (geom :pointer))

(defcfun ("OGR_L_ResetReading" ogr-l-reset-reading) :void
  (layer :pointer))

(defcfun ("OGR_L_TestCapability" ogr-l-test-capability) :int
  (layer :pointer)
  (capability :string))

(defcfun ("OGR_L_GetNextFeature" ogr-l-get-next-feature) :pointer
  (layer :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Feature

(defcfun ("OGR_F_GetGeometryRef" ogr-f-get-geometry-ref) :pointer
  (feature :pointer))

(defcfun ("OGR_F_Destroy" ogr-f-destroy) :void
  (feature :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry

(defcfun ("OGR_G_CreateGeometry" ogr-g-create-geometry) :pointer
  (type :uint))

(defcfun ("OGR_G_DestroyGeometry"  ogr-g-destroy-geometry) :void
  (geom :pointer))

(defcfun ("OGR_G_GetGeometryType" ogr-g-get-geometry-type) :pointer
  (geom :pointer))

(defcfun ("OGR_G_GetGeometryName" ogr-g-get-geometry-name) :string
  (geom :pointer))

(defcfun ("OGR_G_Contains"  OGR-G-Contains) :boolean
  (container :pointer)
  (geom :pointer))

(defcfun ("OGR_G_Intersects"  ogr-g-intersects) :boolean
  (this-geom :pointer)
  (other-geom :pointer))

(defcfun ("OGR_G_Intersection"  ogr-g-intersection) :pointer
  (this-geom :pointer)
  (other-geom :pointer))

(defcfun ("OGR_G_AddPoint_2D" ogr-g-add-point-2d) :void
  (geom :pointer)
  (lon :double)
  (lat :double))

(defcfun ("OGR_G_AddGeometry" ogr-g-add-geometry) :int
  (geom :pointer)
  (newsubgeom :pointer))

(defcfun ("OGR_G_AddGeometryDirectly" ogr-g-add-geometry-directly) :int
  (geom :pointer)
  (newsubgeom :pointer))

(defcfun ("OGR_G_SetPoint_2D" ogr-g-set-point-2d) :void
  (geom :pointer)
  (index :int)
  (lon :double)
  (lat :double))

(defcfun ("OGR_G_GetPoint" ogr-g-get-point) :void
  (geom :pointer)
  (index :int)
  (lon :pointer)
  (lat :pointer)
  (z :pointer))

(defcfun ("OGR_G_GetPointCount" ogr-g-get-point-count) :int
  (geom :pointer))


(defcfun ("OGR_G_ForceToLineString" ogr-g-force-to-line-string) :pointer
  (geom :pointer))

(defcfun ("OGR_G_GetGeometryCount" ogr-g-get-geometry-count) :int
  (geom :pointer))

(defcfun ("OGR_G_GetGeometryRef" ogr-g-get-geometry-ref) :pointer
  (geom :pointer)
  (index :int))

(defcfun ("OGR_G_ExportToWkt" ogr-g-export-to-wkt) :pointer
  (geom :pointer)
  (result :string))


;;; Geometry type

(defcfun ("OGR_GT_Flatten" ogr-gt-flatten) :pointer
  (type :uint))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
