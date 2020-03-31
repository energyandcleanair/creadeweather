import pandas as pd
import os
import geetools
import time
from calendar import monthrange
import pyprind
import ee
import sys

ee.Initialize()


gadm1_fts = geetools.batch.featurecollection.fromGeoJSON('data/00_init/output/gadm1_filtered_bounds.geojson')
era5_ic = ee.ImageCollection("ECMWF/ERA5/DAILY")
gldas_ic = ee.ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H").select('Wind_f_inst','Tair_f_inst','Rainf_tavg','Qair_f_inst','Psurf_f_inst')


date_min = "2018-01-01"
date_max = "2021-01-01"
source_ic = gldas_ic.filterDate(date_min, date_max)

# Approach 1
def reduce_img_fn(ft_):
    def reduce_image(img):
        return ee.Feature(None,
                          ee.Image(img).reduceRegion(
                              geometry=ee.Feature(ft_).geometry(),
                              reducer=ee.Reducer.mean(),
                              # bestEffort=True,
                              scale=100000
                          ).combine(ft_.toDictionary()) \
                          .combine(ee.Dictionary({'id': ee.Image(img).id()}))
                          )

    return reduce_image


def iterate_fn(ft, fc_res):
    res = source_ic.map(reduce_img_fn(ft))
    return ee.FeatureCollection(fc_res).merge(res)


# Approach1 : One station at a time
bar = pyprind.ProgBar(len(gadm1_fts), title='GEE: Weather at GADM1s', stream=sys.stdout)
res_gadm1s = []
start_time = time.time()
for gadm1_ft in gadm1_fts[:2]:
    res_gadm1 = ee.FeatureCollection(source_ic.map(reduce_img_fn(gadm1_ft)))
    try:
        res_gadm1s.append(geetools.batch.featurecollection.toDict(res_gadm1))
    except Exception as e:
        print("Error for GADM1 %s: %s" % (gadm1_ft.get('gadm1_name').getInfo(),str(e)))
        continue
    bar.update()
elapsed_time=time.time()-start_time
print("Method1:" + time.strftime("%H:%M:%S", time.gmtime(elapsed_time)))



# Approach2 : all together
def reduce_image(img):
    return ee.Image(img).reduceRegions(
                          collection=ee.FeatureCollection(gadm1_fts[:2]),
                          reducer=ee.Reducer.mean(),  # At a single point, no need to average
                          tileScale=4
                      )

start_time = time.time()
res_approach2 = ee.FeatureCollection(source_ic.map(reduce_image)).flatten()
res_approach2_info = geetools.batch.featurecollection.toDict(res_approach2)
elapsed_time=time.time()-start_time
print("Method2:" + time.strftime("%H:%M:%S", time.gmtime(elapsed_time)))




# res_stations_data = [x['features'] for x in res_stations]
# res_stations_data = [y['properties'] for x in res_stations_data for y in x]
# len(res_stations_data)
#
# filepath = os.path.join("gee","results","stations_weather.csv")
# pd.DataFrame(res_stations_data).to_csv(filepath)