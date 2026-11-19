#!/usr/bin/env python3
"""Build tiny indexed shapefiles and deterministic segment cases; installs nothing."""
import argparse
import json
from pathlib import Path
import random
import subprocess

def ring(x0,y0,x1,y1): return [[x0,y0],[x1,y0],[x1,y1],[x0,y1],[x0,y0]]
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('directory',type=Path)
args=parser.parse_args();args.directory.mkdir(parents=True,exist_ok=True)
geometries=[{'type':'Polygon','coordinates':[ring(0,0,3,3),ring(1,1,2,2)]},
 {'type':'Polygon','coordinates':[ring(4,1,4.05,1.05)]},
 {'type':'MultiPolygon','coordinates':[[ring(5,0,6,1)],[ring(5,2,6,3)]]},
 {'type':'Polygon','coordinates':[ring(179.7,1,179.8,2)]},
 {'type':'Polygon','coordinates':[ring(-179.8,1,-179.7,2)]}]
for name,geoms in [('land',geometries),('other',[{'type':'Polygon','coordinates':[ring(40,0,43,3)]}])]:
    source=args.directory/f'{name}.geojson'
    source.write_text(json.dumps({'type':'FeatureCollection','features':[
      {'type':'Feature','properties':{},'geometry':g} for g in geoms]}))
    subprocess.run(['ogr2ogr','-overwrite','-f','ESRI Shapefile',str(args.directory/f'{name}.shp'),
      str(source),'-lco','SPATIAL_INDEX=YES'],check=True)
segments=[(-1,.5,.5,.5),(.5,.5,.6,.6),(1.2,1.2,1.8,1.8),(.5,1.5,1.5,1.5),
 (3.9,1.025,4.15,1.025),(3.9,1.1,4.15,1.1),(5.5,1.2,5.5,1.8),
 (5.5,-1,5.5,4),(3,-1,3,4),(-2,-2,-1,-1)]
rng=random.Random(9019)
segments += [tuple(rng.uniform(-1,7) for _ in range(4)) for _ in range(1000)]
(args.directory/'segments.sexp').write_text('('+ '\n'.join('('+ ' '.join(f'{n:.14f}d0' for n in row)+')' for row in segments)+')')
print(args.directory)
