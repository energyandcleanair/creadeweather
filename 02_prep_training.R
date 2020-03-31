require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tibble)
require(lubridate)

meas_at_gadm <- readRDS('data/00_init/output/meas_at_gadm.RDS')

# Split by pollutant
meas_at_gadm <- meas_at_gadm %>% tidyr::unnest(cols=c(meas)) %>%
  rename(value=Concentration) %>%
  group_by(gadm1_id, gadm1_name, AirPollutant) %>%
  tidyr::nest() %>% rename(meas=data)
  

#-------------------------
# Preparing weather data
#-------------------------
csv_pattern = Sys.glob("data/01_weather/output/glas_gadm1/*.csv")
csv_files = lapply(csv_pattern, read.csv)

csv_tbls <- lapply(csv_files, function(x){
  tryCatch({
    new_tibble(x) %>% dplyr::select(id, GID_1, Psurf_f_inst, Qair_f_inst, Rainf_tavg, Tair_f_inst, Wind_f_inst)
  },error=function(cond){
    NA
  })
})
print(paste("Wrong tibbles: [",length(csv_tbls[is.na(csv_tbls)]),"/", length(csv_tbls),"]"))
csv_tbls <- csv_tbls[!is.na(csv_tbls)]

weath_3h_gadm <- do.call(bind_rows, csv_tbls)
weath_3h_gadm <- weath_3h_gadm %>% mutate(date=as.POSIXct(id, format="A%Y%m%d_%H%M", tz="UTC"))

# Average per day
weath_1d_gadm <- weath_3h_gadm %>% mutate(date=lubridate::date(date)) %>%
  group_by(GID_1, date) %>%
  summarise(
    Psurf_f_inst=mean(Psurf_f_inst, na.rm=T),
    Qair_f_inst=mean(Qair_f_inst, na.rm=T),
    Rainf_tavg=mean(Psurf_f_inst, na.rm=T),
    Tair_f_inst=mean(Tair_f_inst, na.rm=T),
    Wind_f_inst=mean(Wind_f_inst, na.rm=T)) %>%
  rename(gadm1_id=GID_1) %>% 
  group_by(gadm1_id) %>% tidyr::nest()
            

#---------------------------
# Merging with measurements
#---------------------------
meas_weather_gadm1 <- meas_at_gadm %>%
  left_join(
    weath_1d_gadm %>% rename(weather=data)) %>% filter(!is.null(weather[[1]])) %>%
  rowwise() %>%
  mutate(meas= list(meas %>% left_join(weather))) %>%
  dplyr::select(-weather)


saveRDS(meas_weather_gadm1, 'data/02_prep_training/output/meas_weather_gadm1.RDS')

