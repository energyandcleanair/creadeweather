require(ggplot2)
require(creadb)
require(countrycode)
require(purrr)
require(tidyr)
require(raster)
require(dplyr)
source('99_crea_theme.R')
source('99_utils.R')

folder <- file.path('plots')
folder_country <- file.path(folder, 'country')
folder_region <- file.path(folder, 'region')
folder_map <- file.path(folder, 'map')

dir.create(folder, showWarnings = FALSE)
dir.create(folder_country, showWarnings = FALSE)
dir.create(folder_region, showWarnings = FALSE)
dir.create(folder_map, showWarnings = FALSE)

gpw <- raster::raster(x=file.path('data','00_init','input','gpw_v4_population_density_rev11_2020_15_min.tif'))

iso2 <- c("DK","CZ","AT","MT","CH","DE","LU","SK","LT","FR","LV","HU","FI","RS","IE","AD","PT","ES","HR","MK","BE","PL","GR","IT","GB","SL","SI","AL","CY","RO","NO")


lockdown <- read.csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vTKMedY9Mzy7e81wWU95Ent79Liq7UwbUz0qTQbkSeAmFCPfqIVNbl1zs99bUOgsJUJbz53GxvBfeiP/pub?gid=0&single=true&output=csv'))
lockdown$movement <- strptime(lockdown$movement_national,"%Y%m%d")
lockdown$school <- strptime(lockdown$school,"%Y%m%d")
lockdown$workplace <- strptime(lockdown$workplace,"%Y%m%d")
lubridate::year(lockdown$movement) <- 0
lubridate::year(lockdown$school) <- 0
lubridate::year(lockdown$workplace) <- 0
lockdown$school_workplace <- pmin(lockdown$school, lockdown$workplace, na.rm=T)
lockdown$iso2 <- countrycode(lockdown$iso3, origin='iso3c', destination='iso2c')


# Plot national pollutants
plot_nations_of <- function(iso2, poll, folder, rolling_days){
    
  meas <- creadb::measurements(country=iso2,
                               aggregate_level = 'country',
                               poll=poll, date_from='2018-01-01', source = 'eea') %>%
    rowwise() %>%
    mutate(city=countrycode(city, origin='iso2c',destination='country.name')) %>%
    mutate(date=lubridate::force_tz(lubridate::with_tz(date, tz=ifelse(is.na(timezone),'UTC',timezone)), tz="UTC")) %>%
    filter(!is.na(date))
  
  meas <- meas %>% dplyr::filter(substr(timezone,1,6)=='Europe') # Remove remote islands
  meas <- meas %>%
    utils.rolling_average(average_by='day', average_width=rolling_days,
                          group_cols=c('country','poll','unit','source','city'),
                          avg_cols=c('value')) %>%
    dplyr::rename(value_roll=value) %>%
    dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
    dplyr::mutate(yday=lubridate::yday(date)) %>%
    dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) 

  meas <- meas %>% left_join(lockdown, by=c("country"="iso2"))
  
  date_from <-lubridate::date('0000-01-01')
  date_to <-lubridate::date('0000-05-01')

  (plt_country <- 
    ggplot(meas) +
      geom_line(aes(x=date_in_year, y=value_roll, color=year, size=year)) +
    theme_crea() +
    labs(title=paste(toupper(poll),'levels in selected countries'),
         subtitle=paste0(rolling_days,'-days running average'), y="Concentration [µg/m3]", size='Year', colour="Year",
         caption="Source: CREA based on European Environment Agency and Oxford COVID-19 Government Response Tracker") +
    scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                   labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
    ylim(0, NA) +
    theme(axis.title.x=element_blank()) +
    facet_wrap(~city, scales='free_y') +
    geom_vline(aes(xintercept=lubridate::date(movement), linetype='1'), colour=pal_crea['Turquoise']) +
    geom_vline(aes(xintercept=lubridate::date(school_workplace), linetype='2'), colour=pal_crea['Turquoise']) +
    scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
    scale_size_manual(values=c(0.3,0.3,0.6)) +
    scale_color_crea_d("dramatic"))

  ggsave(file.path(folder,paste0(poll,'_countries_roll',rolling_days,'.png')), plt_country, width=1024/300, height=800/300, scale=3, unit='in')
  write.csv(meas %>%
              dplyr::rename(iso2=country, country=city, source=source.x) %>%
              dplyr::select('country','poll','unit','source','date','value_roll','school','workplace','movement','school_workplace')%>%
              dplyr::mutate(school=lubridate::date(school)+lubridate::years(2020),
                             workplace=lubridate::date(workplace)+lubridate::years(2020),
                             movement=lubridate::date(movement)+lubridate::years(2020),
                             school_workplace=lubridate::date(school_workplace)+lubridate::years(2020)) %>%
              arrange(country,date),
            file.path(folder,paste0(poll,'_countries_roll',rolling_days,'.csv')),
            row.names = FALSE)
  return(plt_country)
}

roi <- c(7,30)
poi <- c(creadb::PM10, creadb::NO2)
for(rolling in roi){
  for(poll in poi){
    plot_nations_of(iso2, poll, folder=folder_country, rolling_days=rolling)
  }
}

# Plot regional pollutants
plot_regions_of <- function(iso2, poll, folder, rolling_days){
  iso2_ <- iso2
  country_name <- countrycode(iso2,'iso2c','country.name')
  meas <- creadb::measurements(country=c(iso2),
                               aggregate_level = 'city',
                               poll=poll, date_from='2018-01-01', source = 'eea', with_metadata = T, collect=F) %>%
    group_by(country, gid_1, name_1, date, poll, unit, source, timezone) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    collect() %>%
    rowwise() %>%
    # tidyr::replace_na(list(timezone = "UTC")) %>% 
    # dplyr::mutate(date = lubridate::force_tz(date, tzone = timezone)) %>%
    filter(!is.na(date)) %>% filter(!is.na(name_1))

  meas <- meas %>% filter(substr(timezone,1,6)=='Europe') # Remove remote islands
  meas <- meas %>%
    utils.rolling_average(average_by='day', average_width=rolling_days,
                          group_cols=c('country','poll','unit','source','name_1'),
                          avg_cols=c('value')) %>%
    dplyr::rename(value_roll=value) %>%
    dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
    dplyr::mutate(yday=lubridate::yday(date)) %>%
    dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) 
  
  meas <- meas %>% left_join(lockdown, by=c("country"="iso2"))
  
  date_from <-lubridate::date('0000-01-01')
  date_to <-lubridate::date('0000-05-01')

  (plt_regions <- ggplot(meas) +
      geom_line(aes(x=date_in_year, y=value_roll, color=year, size=year)) +
      theme_crea() +
      labs(title=paste(toupper(poll),'levels in regions of',country_name),
           subtitle=paste0(rolling_days,'-days running average'), y="Concentration [µg/m3]", size='Year', colour="Year",
           caption="Source: CREA based on European Environment Agency and Oxford COVID-19 Government Response Tracker") +
      scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                   labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
      ylim(0, NA) +
      theme(axis.title.x=element_blank()) +
      facet_wrap(~name_1) +
      geom_vline(aes(xintercept=lubridate::date(movement), linetype='1'), colour=pal_crea['Turquoise']) +
      geom_vline(aes(xintercept=lubridate::date(school_workplace), linetype='2'), colour=pal_crea['Turquoise']) +
      scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
      scale_size_manual(values=c(0.3,0.3,0.6)) +
      scale_color_crea_d("dramatic"))
      
  
  ggsave(file.path(folder,paste0('no2_',tolower(country_name),'_regions_roll',rolling_days,'.png')), plt_regions, width=1024/300, height=800/300, scale=3, unit='in')
  write.csv(meas %>%
              # dplyr::rename(iso2=country, country=city, source=source.x) %>%
              # dplyr::select('country','poll','unit','source','date','value_roll','school','workplace','movement','school_workplace')%>%
              dplyr::mutate(school=lubridate::date(school)+lubridate::years(2020),
                            workplace=lubridate::date(workplace)+lubridate::years(2020),
                            movement=lubridate::date(movement)+lubridate::years(2020),
                            school_workplace=lubridate::date(school_workplace)+lubridate::years(2020)) %>%
              arrange(country,date), file.path(folder,paste0('no2_',tolower(country_name),'_regions_roll',rolling_days,'.csv')))
  
}

coi <- c('ES','FR','IT','DE','PL','CZ','GB')
roi <- c(7,30)
poi <- c(creadb::PM10, creadb::NO2)
for(country in coi){
  for(rolling in roi){
    for(poll in poi){
      plot_regions_of(country, poll, folder=folder_region, rolling_days=rolling)
    }
  }
}


 
# plot_national <- function(pollutant_){
#   
#   date_from <-lubridate::date('0000-02-01')
#   date_to <-lubridate::date('0000-04-08')
#   ggplot(data_national %>% filter(pollutant==pollutant_) %>% left_join(lockdown %>% rename(lockdown_date=date)) %>%
#            utils.rolling_average(average_by='day', average_width=7, group_cols=c('country', 'pollutant'), avg_cols=c('value')) %>%
#            # dplyr::select(country, pollutant, date, value, predicted) %>%
#            dplyr::mutate(year=factor(lubridate::year(date))) %>%
#            dplyr::mutate(yday=lubridate::yday(date)) %>%
#            dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) %>%
#            dplyr::filter(date>='2018-01-01', yday>=lubridate::yday(date_from), yday<=lubridate::yday(date_to))  %>% filter(!is.na(country))) + ylim(c(0, NA)) +
#     geom_line(aes(x=date_in_year, y=value, color=year, linetype='solid')) +
#     facet_wrap(~country, scales='free_y') + theme_crea() +
#     scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
#                  labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
#     labs(title=paste(pollutant_,'levels in selected countries'), subtitle='7 days rolling average',
#          y ='Concentration (µg/m3)', colour="Year") +
#     theme(axis.title.x=element_blank()) +
#     geom_vline(data=lockdown, aes(xintercept=date, linetype='dashed'), linetype='dashed', colour=pal_crea['Turquoise']) +
#     scale_linetype(name = "", labels=c('Lockdown')) +
#     guides(linetype = guide_legend(override.aes = list(colour = pal_crea['Turquoise'], linetype='dashed')))
# }
# 

plot_national_average <- function(iso2, poll, years_before, folder){
  
  doy_max <- lubridate::yday('2020-04-20')
  count_threshold <- 15 # Number of days with measurements in the period to consider the station
  
  meas <- creadb::measurements(country=iso2, average_by = 'day', aggregate_level = "location", poll=poll, collect=F) %>%
    left_join(lockdown %>% mutate(movement_yday=lubridate::yday(movement)) %>% select(iso2, movement_yday), by=c("country"="iso2"), copy=T) %>%
    dplyr::mutate(doy=sql('extract(doy from date)'), year=sql('extract(year from date)')) %>%
    dplyr::filter(doy>=movement_yday, doy<=doy_max) %>%
    dplyr::group_by(country, year, location_id, poll, unit) %>%
    dplyr::summarise(value=mean(value, na.rm=T), count=n()) %>%
    dplyr::collect()
  
  stations_to_keep <- meas %>% select(location_id, year, count) %>%
    group_by(year, location_id) %>%
    right_join(tibble(year=years_before)) %>%
    group_by(location_id) %>%
    summarise(count=min(count, na.rm=FALSE)) %>%
    filter(count >= count_threshold)
  
  meas <- stations_to_keep %>% select(location_id) %>%
    left_join(meas) %>%
    ungroup() %>%
    filter(year %in% c(years_before, 2020)) %>%
    mutate(period=ifelse(year %in% years_before, 'before', 'after')) %>%
    mutate(country=countrycode::countrycode(country, origin='iso2c', destination='country.name')) %>%
    tidyr::spread(period,value) %>% group_by(country, location_id, poll, unit) %>%
    summarise(after=mean(after, na.rm=T), before=mean(before, na.rm=T)) %>%
    filter(!is.na(before) & !is.na(after)) %>%
    mutate(ratio=(after-before)/before)
  
  plt <- ggplot(meas) +
    geom_boxplot(aes(y=country, x=ratio)) +
    labs(x=NULL,
         title=paste('Effect of national lockdowns on',toupper(poll),'levels'),
         subtitle=paste('Average levels after lockdown vs levels at the same period in previous years - No weather normalization'),
         caption=paste('Years of reference: ',paste(years_before, collapse=','),'. A lockdown is defined as a national restriction of internal movement.','\nSource: CREA based on European Environment Agency and Oxford COVID-19 Government Response Tracker')) + 
    scale_x_continuous(labels = scales::percent) + 
    geom_vline(xintercept=0, linetype='dotted') +
    theme_crea() +
    scale_color_crea_d("dramatic")
  
  basename <- paste0(poll,'_countries_box_',min(years_before),'_2020')
  ggsave(file.path(folder,paste0(basename,'.png')), plt, width=1024/300, height=800/300, scale=3, unit='in')
  write.csv(plot_data %>% arrange(country, location_id),
            file.path(folder,paste0(basename, '.csv')),
            row.names = FALSE)
  
  return(meas)
}

plot_national_average(iso2, creadb::NO2, years_before=seq(2018,2019), folder = folder_country)
plot_national_average(iso2, creadb::PM10, years_before=seq(2018,2019), folder = folder_country)

meas_var_no2 <- plot_national_average(iso2, creadb::NO2, years_before=seq(2015,2019), folder = folder_country)
meas_var_pm10 <- plot_national_average(iso2, creadb::PM10, years_before=seq(2015,2019), folder = folder_country)



# Population weighed number
locs <- creadb::locations(id=unique(meas_var_no2$location_id))
meas_var_no2_r <- meas_var_no2 %>% dplyr::left_join(locs %>% dplyr::select(id, geometry),
                                                    by=c('location_id'='id'))
meas_var_no2_r$gpw <- raster::extract(gpw, sf::st_as_sf(meas_var_no2_r))
meas_var_no2_r$delta <- meas_var_no2_r$after - meas_var_no2_r$before
meas_var_no2_r %>% ungroup() %>% filter(!is.na(gpw)) %>%
  summarise(delta = weighted.mean(delta,gpw),
            ratio = weighted.mean(ratio,gpw))

locs <- creadb::locations(id=unique(meas_var_pm10$location_id))
meas_var_pm10_r <- meas_var_pm10 %>% dplyr::left_join(locs %>% dplyr::select(id, geometry),
                                                    by=c('location_id'='id'))
meas_var_pm10_r$gpw <- raster::extract(gpw, sf::st_as_sf(meas_var_pm10_r))
meas_var_pm10_r$delta <- meas_var_pm10_r$after - meas_var_pm10_r$before
meas_var_pm10_r %>% ungroup() %>% filter(!is.na(gpw)) %>%
  summarise(delta = weighted.mean(delta,gpw),
            ratio = weighted.mean(ratio,gpw))

# Plot Lombardia
source('00_prepare_input_lombardia.R')

plot_lombardia <- function(meas_lombardia, polls){
  meas_lombardia_avg <- meas_lombardia %>%
    ungroup() %>%
    dplyr::select(pollutant,meas) %>%
    tidyr::unnest(cols=c(meas)) %>%
    mutate(date=lubridate::date(date)) %>%
    group_by(pollutant,date) %>%
    summarise(value=mean(value))
  
  date_from <-lubridate::date('0000-01-01')
  date_to <-lubridate::date('0000-04-15')
  meas_lombardia_plot <- meas_lombardia_avg %>%
    utils.rolling_average(average_by='day', average_width=7, group_cols=c('pollutant'), avg_cols=c('value')) %>%
    dplyr::mutate(year=factor(lubridate::year(date))) %>%
    dplyr::mutate(yday=lubridate::yday(date)) %>%
    dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) 
  
  plt_lombardia <- ggplot(meas_lombardia_plot %>% filter(tolower(pollutant) %in% polls)) +
    geom_line(aes(x=date_in_year,y=value, color=year)) + theme_crea() +
    labs(subtitle='7-days running average', caption='Source: ARPA LOMBARDIA')+
    scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                 labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
    theme(axis.title.x=element_blank()) +
    geom_vline(data=tibble(pollutant=toupper(polls),date=lubridate::date('0000-03-08')),
               aes(xintercept=date,linetype='1'), colour = pal_crea['Turquoise']) +
    ylim(0, NA) +
    scale_linetype_manual(name = "", values=c('dashed'), labels=c('Lombardia lockdown')) +
    guides(linetype = guide_legend(override.aes = list(colour = pal_crea['Turquoise'], linetype='dashed')))
    
  if(length(polls)>1){
    plt_lombardia <- plt_lombardia + labs(title="Pollutant levels in Lombardia") + facet_wrap(~pollutant, scales='free_y')
  }else{
    plt_lombardia <- plt_lombardia + labs(title=paste(toupper(polls), "level in Lombardia"))
  }
  plt_lombardia
  filename <- paste0(paste0(tolower(polls),collapse='_'),'_lombardia_regions.png')
  ggsave(file.path(plot_folder,filename), plt_lombardia, width=1024/300, height=800/300, scale=3, unit='in')
}
# 
# if(!exists('meas_lombardia')){
#   meas_lombardia <- lombardia.get_meas()  
# }
# 
# plot_lombardia(meas_lombardia, c(creadb::NO2,creadb::PM10,creadb::CO,creadb::SO2))
# plot_lombardia(meas_lombardia, creadb::NO2)
# plot_lombardia(meas_lombardia, creadb::CO)
# plot_lombardia(meas_lombardia, creadb::PM25)

