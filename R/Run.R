library(tibble)
library(rcrea)
library(tidyr)

library(ecmwfr)
library(raster)
library(rgdal)
library(lubridate)
library(ncdf4)
library(chron)
library(stringr)


library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(devtools)

library(sf)
library(ncdf4)
library(terra)



dir.create('DIR_NOAA')
dir.create('DIR_PBL')
dir.create('DIR_ERA5')

Sys.setenv(DIR_NOAA_ISD=file.path(getwd(), 'DIR_NOAA'))
Sys.setenv(DIR_PBL=file.path(getwd(), 'DIR_PBL'))
Sys.setenv(DIR_ERA5=file.path(getwd(), 'DIR_ERA5'))
Sys.setenv(uid='124338')
Sys.setenv(cds_api_key='809cb746-d36d-4524-b435-8a9f07f8ae17')


start.time <- Sys.time()

results <- deweather(city='Delhi',poll='pm25',source='cpcb', training_start_trend = '2021-06-01', date_to='2021-06-03')

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken