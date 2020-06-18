source('scripts/_load_dependencies.R')

polls <- c(rcrea::PM25, rcrea::NO2) #, rcrea::PM10) #, rcrea::PM25, rcrea::PM10, rcrea::SO2, rcrea::O3, rcrea::CO)
source_ <- c("eea")
country <- NULL #"VN"  #c("ID") #,"VN","PH")
city <- c("Paris","Lyon", "London","Portsmouth","London","Manchester",
 "Copenhagen","Madrid","Barcelona","Rome", "Milan","Turin",
 "Berlin","Munich","Frankfurt","Düsseldorf","Brussels","Minsk",
 "Helsinki","Athens","Budapest","Bucarest","Vienna","Bern",
 "Lisbon","Ljubljana","Bratislava","Oslo", "Prague","Riga",
"Warsaw","Tirana","Southampton", "Reading", "Oxford")
training_start <- "2017-01-01"
training_end_anomaly <- "2020-01-01"
training_end_trend <- "2099-01-01"
#----------------------
# 1. Get measurements
#----------------------
meas <- rcrea::measurements(poll=polls,
                            country=country,
                            city=city,
                            date_from=training_start,
                            source=source_,
                            deweathered=F,
                            with_metadata=T,
                            with_geometry=T) %>%
  group_by(region_id, poll, unit, source, timezone, process_id, country, geometry) %>%
  tidyr::nest() %>%
  rename(station_id=region_id, meas=data) %>% ungroup()

meas_sf <- meas %>% ungroup() %>%
  mutate(geometry=sf::st_centroid(sf::st_as_sfc(geometry))) %>%
  st_as_sf(sf_column_name="geometry", crs = 4326)

#----------------------
# 2. Add weather
#----------------------
meas_weather <- collect_weather(meas_sf,
                                years=seq(lubridate::year(lubridate::date(training_start)), 2020),
                                years_force_refresh = 2020,
                                add_pbl=F,
                                add_sunshine=F,
                                n_per_station=3
)

#----------------------
# 3. Clean data
#----------------------
data <- prep_data(meas_weather=meas_weather)

#----------------------
# 4. Train models
#----------------------
normalise <- F
detect_breaks <- F
trees <- 10000
samples <- 100
interaction.depth <- c(2)
learning.rate <- c(0.01)
lag <- 1
engine <- "gbm"
link <- "log"
weather_vars <- c(
  list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max'))
)

# time_vars_output <- tibble(
#   time_vars=c(list(c())),
#   output=c('anomaly'),
#   training_end=c(training_end_anomaly)
# )
# 
time_vars_output <- tibble(
  time_vars=c(list(c()),list(c('trend'))),
  output=c('anomaly','trend'),
  training_end=c(training_end_anomaly, training_end_trend)
)

# 
# time_vars_output <- tibble(
#   time_vars=c(list(c('trend'))), #,list(c('trend','yday'))),
#   output=c('trend'),
#   training_end=c(training_end_trend)
# )

# time_vars <- c(
#   # list(c())
#   list(c('trend'))
#   # list(c('trend','yday'))
#   # list(c('trend','yday','month')),
#   # list(c('trend','yday','week')),
#   # list(c('trend','month','wday')) 
# )
# output <- c("trend","anomaly")

configs <-  tibble() %>%
  tidyr::expand(trees, lag, weather_vars, time_vars_output, engine, link, learning.rate, interaction.depth) %>%
  rowwise() %>%
  mutate(process_deweather=
           gsub("'","\"",paste0("{",
                                "'engine':'",engine,"',",
                                "'trees':'",trees,"',",
                                "'learning.rate':'",learning.rate,"',",
                                "'interaction.depth':'",interaction.depth,"',",
                                "'lag':'",lag,"',",
                                "'training_start':'",training_start,"',",
                                "'training_end':'",training_end,"',",
                                "'time_vars':['",paste0(time_vars,collapse="','"),"'],",
                                "'weather_vars':['",paste0(weather_vars,collapse="','"),"'],",
                                "'link':'",link,"',",
                                "'output':'",output,"'",
                                "}")
           )
  )

results_nested <- configs %>% rowwise() %>%
  mutate(
    result=list(train_models(
      engine=engine,
      meas_weather=tibble(data),
      weather_vars=weather_vars,
      time_vars=time_vars,
      trees=trees,
      samples=samples,
      interaction.depth=interaction.depth,
      learning.rate=learning.rate,
      lag=lag,
      link=link,
      normalise=normalise,
      detect_breaks=detect_breaks,
      training_date_cut=training_end,
      save_result=F,
      return_result=T)
    ))


#--------------------------------------
# 5. Post-compute / aggregate results
#--------------------------------------
results_anomaly <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))
results_trend <- results_nested %>% dplyr::filter(output=='trend') %>% tidyr::unnest(cols=c(result))

# Creating measurements anomaly to upload
results_anomaly <- results_anomaly  %>% rowwise()  %>%
  dplyr::mutate(normalised=list(predicted %>%
                                  filter(set=='testing') %>%
                                  mutate(value=value-predicted)), # Not residuals but ANOMALY (i.e. -1 * residuals)
         unit=paste('Δ', unit), # To force ploting on different charts on Dashboard
         process_deweather = gsub("}",",\"output\":\"anomaly\"}",process_deweather)) %>%
  dplyr::rename(region_id=station_id) %>%
  dplyr::select(process_id, process_deweather, normalised, poll, unit, region_id, source)
#   
results_trend <- results_trend  %>% rowwise()  %>%
  dplyr::mutate(normalised=list(trend)) %>%
  dplyr::rename(region_id=station_id) %>%
  dplyr::select(process_id, process_deweather, normalised, poll, unit, region_id, source)


# Group by GADM2 and GADM1
# results_anomaly_gadm1
# process_id replace city->gadm
locs <- rcrea::locations(source=source_, with_meta = T) %>% mutate(city=tolower(city))

agg_gadm1 <- function(results, locs){
  results %>%
    left_join(locs %>% dplyr::select(city, gid_1), by=c("region_id"="city")) %>%
    mutate(process_id=gsub("city","gadm1",process_id)) %>%
    tidyr::unnest(cols=normalised) %>%
    group_by(process_id, process_deweather, poll, unit, source, gid_1, date) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    tidyr::nest() %>%
    rename(region_id=gid_1, normalised=data)
}

agg_gadm2 <- function(results, locs){
  results %>%
    left_join(locs %>% dplyr::select(city, gid_1, gid_2), by=c("region_id"="city")) %>%
    mutate(process_id=gsub("city","gadm2",process_id)) %>%
    tidyr::unnest(cols=normalised) %>%
    group_by(process_id, process_deweather, poll, unit, source, gid_2, date) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    tidyr::nest() %>%
    rename(region_id=gid_2, normalised=data)
}
# 
results_trend_gadm1 <- agg_gadm1(results_trend, locs)
results_trend_gadm2 <- agg_gadm2(results_trend, locs)

results_anomaly_gadm1 <- agg_gadm1(results_anomaly, locs)
results_anomaly_gadm2 <- agg_gadm1(results_anomaly, locs)


#--------------------
# 6. Upload results
#--------------------
processes <- results_trend %>% distinct(process_id, process_deweather)
# 
results_trend_uploaded <- results_trend %>% rowwise() %>%
  mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("trend_gbm_lag",lag)))

results_trend_gadm1_uploaded <- results_trend_gadm1 %>% rowwise() %>%
  mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("trend_gbm_lag",lag,"_gadm1")))

results_trend_gadm2_uploaded <- results_trend_gadm2 %>% rowwise() %>%
  mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("trend_gbm_lag",lag,"_gadm2")))

results_anomaly_uploaded <- results_anomaly %>% rowwise() %>%
 mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("anomaly_gbm_lag",lag)))

results_anomaly_gadm1_uploaded <- results_anomaly_gadm1 %>% rowwise() %>%
 mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("anomaly_gbm_lag",lag,"_gadm1")))

results_anomaly_gadm2_uploaded <- results_anomaly_gadm2 %>% rowwise() %>%
 mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, region_id, normalised, source, paste0("anomaly_gbm_lag",lag,"_gadm2")))
