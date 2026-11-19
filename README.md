# cl-map

Common Lisp land predicates backed by an indexed GDAL vector map. Prepared
geometry caching accelerates exact coastal checks without raster approximation.

## Runtime requirements

- GDAL 3.3 or newer with GEOS prepared-geometry support (tested: GDAL 3.12.4,
  GEOS 3.14.1 on Fedora 44).
- An indexed geographic (longitude/latitude) land-polygon shapefile. Opening
  requires `FastSpatialFilter`; prepared caching additionally requires
  `RandomRead`. Unsupported prepared access uses exact indexed predicates.
- CFFI, log2, makros, cl-geomath and Bordeaux Threads through the existing
  application environment. No software is installed by this project.

## Public API

```lisp
(setf cl-map:*map-file* "/path/to/land_polygons.shp")
(cl-map:ensure-map)
(cl-map:point-on-land-p position)
(cl-map:intersects-land-p start end)
(cl-map:line-intersects-land-p start end) ; entire segment, no coarse shortcut
(cl-map:prepared-cache-statistics)
(cl-map:clear-map-cache)
(cl-map:ensure-map :reload t)
(cl-map:close-map)
```

`ensure-map` reloads when explicitly requested, when its path/inversion changes,
or after close. A failed replacement leaves the old map open. `load-map`
(internal API) opens an independent map; its caller owns `close-map`. Closing
is idempotent, invalidates handles, and frees prepared resources before closing
the dataset. Map-specific queries accept `:map`. A closed-map query signals a
Lisp error rather than entering native code with a stale handle.

## Prepared cache

The default enabled cache has a **1000 MiB global accounting budget**, shared
across open maps. It is lazy: this is an allowance, not an upfront allocation.
The default spatial candidate cells are 0.25°. They store feature IDs, while
one globally managed LRU owns prepared polygon geometries. Feature eviction
cannot leave dangling pointers in candidate cells; IDs are reread as needed.

Configuration, set before serving requests or in application configuration:

| Parameter | Default | Meaning |
| --- | ---: | --- |
| `*prepared-cache-enabled*` | `t` | Enable prepared predicates |
| `*prepared-cache-max-bytes*` | `1048576000` | Global conservative geometry charge, bytes |
| `*prepared-cache-max-entries*` | `50000` | Global feature/negative-entry limit |
| `*prepared-cache-cell-size*` | `0.25d0` | Candidate-cell width in degrees |
| `*prepared-cache-tile-bytes*` | `33554432` | Per-map candidate-index charge, bytes |
| `*prepared-cache-max-tiles*` | `4096` | Per-map candidate-cell LRU limit |
| `*prepared-cache-max-query-tiles*` | `256` | Larger boxes use indexed fallback |

The geometry accounting charges **16 × serialized WKB bytes + 4096 bytes per
entry**. GDAL does not expose the exact native size of a prepared structure;
this is a conservative estimate, **not measured RSS or a hard process-memory
cap**. Reservation/eviction occurs before cloning/preparing. An oversized feature
is not prepared; it uses exact indexed access. Negative entries, entry counts,
cell indices and large candidate sets are bounded separately. Transient GDAL
query allocations and allocator overhead are outside cache accounting. Native
geometry allocations are outside SBCL's dynamic-space limit.

The global map mutex protects layer iteration, predicates, cache eviction,
clearing, reload and close. Geometry handles are never shared unsynchronized.
Smaller runtime budgets are enforced on subsequent queries; `clear-map-cache`
releases a particular map immediately. Disabling the cache releases resident
prepared entries when a subsequent query trims the global pool. Statistics
report geometry charges, WKB payload, feature count, hits/misses/evictions,
fallback count and per-map spatial/coarse index usage. Counters are cumulative;
clearing entries does not reset the global counters.

All prepared predicates are exact. Point queries preserve strict containment
(boundary is not interior); segment/rectangle queries use intersection. Missing
preparation support, uncacheable features and oversized boxes fall back to the
indexed predicate. GDAL errors signal rather than being interpreted as water.
Antimeridian sailing segments are split into two adjacent longitude-domain
segments. The existing `intersects-land-p` endpoint-tile shortcut is retained;
it can skip intermediate land if the end tile is all water. Use the direct
`line-intersects-land-p` API when the complete segment must be tested.

The separate 0.1° coarse presence cache is map-owned, synchronized and packed
into two bits per cell (about 1.55 MiB at the default width). A map reload cannot
reuse another dataset's land/water flags. Grid-boundary samples test the whole
cell, avoiding degenerate zero-width rectangles.

## Tests and benchmark

Tests need the existing `python3` and `ogr2ogr` executables to build tiny indexed
fixtures. They install nothing and remove only their generated temporary data.

```lisp
(asdf:test-system "cl-map")
```

Coverage includes holes, islands, multipolygons, tile boundaries, antimeridian
crossings, forced feature/cell eviction, oversize/disabled fallbacks, multiple
maps, global accounting, cleanup, reload, and concurrent queries/clears.

Bitsailor's `diagnostics/production-vector-cache-benchmark.cl` compares both
production modes without replacing global functions. Run it in an isolated
process with local forecasts and fine-tuning excluded from the search timing.
The older prototype and its measurements remain documented in Bitsailor's
`diagnostics/VECTOR-LAND-ACCESS.md`.
