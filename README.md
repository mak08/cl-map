# cl-map

cl-map provides bindings to a very small subset of libgdal, useful for testing if a point is on land or not.

## API


## Dependencies

* Requires [libgdal 2.4.3](https://github.com/OSGeo/gdal/tree/v2.4.3)
  Other versions may work but were not tested. The preferred way is to install from source.
  Caution: use `make -j4` or similar when building. Building libgdal takes very long.
  
  Alternatively, install it as a package. 
  
* cl-map depends on the following Common Lisp packages
  * [CFFI](https://common-lisp.net/project/cffi/)
  * [log2](https://github.com/mak08/log2)
  * [cl-geomath](https://github.com/mak08/cl-geomath)
  
## Known issues

* If libgdal was installed as a package, exchanging libsqlite3 may cause libgdal.so to fail to load. In my case, using SBCL,
  the shared object loader complained that `sqlite3_table_column_name` could not be found in libgdal.
