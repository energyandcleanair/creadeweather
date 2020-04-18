import pandas as pd
import os
import geetools
import time
from calendar import monthrange
import pyprind
import ee
import sys

ee.Initialize()


country_fc = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017").filterMetadata('wld_rgn','equals','Europe')
no2_ic = ee.ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2").select('tropospheric_NO2_column_number_density')

date_min = "2018-01-01"
date_max = "2021-01-01"
no2_ic = no2_ic.filterDate(date_min, date_max)
source_ic = no2_ic

# Approach 1
def reduce_img_fn(ft_):
    def reduce_image(img):
        return ee.Feature(None,
                          ee.Image(img).reduceRegion(
                              geometry=ee.Feature(ft_).geometry(),
                              reducer=ee.Reducer.mean(),
                              bestEffort=True
                              # scale=100000
                          ).combine(ft_.toDictionary()) \
                          .combine(ee.Dictionary({'id': ee.Image(img).id()}))
                          )

    return reduce_image


def iterate_fn(ft, fc_res):
    res = source_ic.map(reduce_img_fn(ft))
    return ee.FeatureCollection(fc_res).merge(res)


# Approach1 : One region at a time
bar = pyprind.ProgBar(len(country_fc), title='GEE: NO2 at Countries', stream=sys.stdout)
res_gadm1s = []
start_time = time.time()
for country_ft in country_fc:
    res_ft = ee.FeatureCollection(source_ic.map(reduce_img_fn(country_ft)))
    try:
        res_ft = geetools.batch.featurecollection.toDict(res_ft)
        filepath = os.path.join("gee", "results", res_ft.get('country_co')+".csv")
        pd.DataFrame(res_ft).to_csv(filepath)

    except Exception as e:
        print("Error for Country %s: %s" % (res_ft.get('country_co').getInfo(),str(e)))
        continue
    bar.update()
elapsed_time=time.time()-start_time
print("Method1:" + time.strftime("%H:%M:%S", time.gmtime(elapsed_time)))



#
# # Approach2 : all together
# def reduce_image(img):
#     return ee.Image(img).reduceRegions(
#                           collection=ee.FeatureCollection(gadm1_fts[:2]),
#                           reducer=ee.Reducer.mean(),  # At a single point, no need to average
#                           tileScale=4
#                       )
#
# start_time = time.time()
# res_approach2 = ee.FeatureCollection(source_ic.map(reduce_image)).flatten()
# res_approach2_info = geetools.batch.featurecollection.toDict(res_approach2)
# elapsed_time=time.time()-start_time
# print("Method2:" + time.strftime("%H:%M:%S", time.gmtime(elapsed_time)))
#
#


# res_stations_data = [x['features'] for x in res_stations]
# res_stations_data = [y['properties'] for x in res_stations_data for y in x]
# len(res_stations_data)
#
#
