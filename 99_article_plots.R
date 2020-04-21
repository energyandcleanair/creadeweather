require(ggplot2)
require(creadb)
require(countrycode)
require(purrr)
require(tidyr)
source('99_crea_theme.R')
source('99_utils.R')

plot_folder <- file.path('plots')
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
plot_nations_of <- function(iso2, poll){
    
  meas <- creadb::measurements(country=iso2,
                               aggregate_level = 'country',
                               poll=poll, date_from='2018-01-01', source = 'eea') %>%
    rowwise() %>%
    mutate(city=countrycode(city, origin='iso2c',destination='country.name')) %>%
    mutate(date=lubridate::force_tz(lubridate::with_tz(date, tz=ifelse(is.na(timezone),'UTC',timezone)), tz="UTC")) %>%
    filter(!is.na(date))
  
  meas <- meas %>% filter(substr(timezone,1,6)=='Europe') # Remove remote islands
  meas <- meas %>% utils.rolling_average(average_by='day', average_width=7,
                                 group_cols=c('country','poll','unit','source','city'),
                                 avg_cols=c('value')) %>%
    dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
    dplyr::mutate(yday=lubridate::yday(date)) %>%
    dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) 

  meas <- meas %>% left_join(lockdown, by=c("country"="iso2"))
  
  date_from <-lubridate::date('0000-01-01')
  date_to <-lubridate::date('0000-04-15')

  (plt_country <- 
    ggplot(meas) +
      geom_line(aes(x=date_in_year, y=value, color=year, size=year)) +
    theme_crea() +
    labs(title=paste(toupper(poll),'levels in selected countries'),
         subtitle='7-days running average', y="Concentration [µg/m3]", size='Year', colour="Year",
         caption="Source: CREA based on European Environment Agency and Oxford COVID-19 Government Response Tracker") +
    scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                   labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
    ylim(0, NA) +
    theme(axis.title.x=element_blank()) +
    facet_wrap(~city, scales='free_y') +
    geom_vline(aes(xintercept=lubridate::date(movement), linetype='1'), colour=pal_crea['Turquoise']) +
    geom_vline(aes(xintercept=lubridate::date(school_workplace), linetype='2'), colour=pal_crea['Turquoise']) +
    scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
    scale_size_manual(values=c(0.3,0.3,0.6)))

  ggsave(file.path(plot_folder,paste0(poll,'_countries.png')), plt_country, width=1024/300, height=800/300, scale=3, unit='in')
  return(plt_country)
}
plot_nations_of(iso2, creadb::NO2)
plot_nations_of(iso2, creadb::PM10)
plot_nations_of(iso2, creadb::CO)

# Plot regional pollutants
plot_regions_of <- function(iso2, poll){
  iso2_ <- iso2
  country_name <- countrycode(iso2,'iso2c','country.name')
  meas <- creadb::measurements(country=c(iso2),
                               aggregate_level = 'gadm1',
                               poll=poll, date_from='2018-01-01', source = 'eea') %>%
    rowwise() %>%
    mutate(date=lubridate::force_tz(lubridate::with_tz(date, tz=ifelse(is.na(timezone),'UTC',timezone)), tz="UTC")) %>%
    filter(!is.na(date)) %>% filter(!is.na(city))

  meas <- meas %>% filter(substr(timezone,1,6)=='Europe') # Remove remote islands
  meas <- meas %>% utils.rolling_average(average_by='day', average_width=7,
                                         group_cols=c('country','poll','unit','source','city'),
                                         avg_cols=c('value')) %>%
    dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
    dplyr::mutate(yday=lubridate::yday(date)) %>%
    dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) 
  
  meas <- meas %>% left_join(lockdown, by=c("country"="iso2"))

  (plt_regions <- ggplot(meas) +
      geom_line(aes(x=date_in_year, y=value, color=year, size=year)) +
      theme_crea() +
      labs(title=paste(toupper(poll),'levels in regions of',country_name),
           subtitle='7-days running average', y="Concentration [µg/m3]", size='Year', colour="Year",
           caption="Source: CREA based on European Environment Agency and Oxford COVID-19 Government Response Tracker") +
      scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                   labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
      ylim(0, NA) +
      theme(axis.title.x=element_blank()) +
      facet_wrap(~city) +
      geom_vline(aes(xintercept=lubridate::date(movement), linetype='1'), colour=pal_crea['Turquoise']) +
      geom_vline(aes(xintercept=lubridate::date(school_workplace), linetype='2'), colour=pal_crea['Turquoise']) +
      scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
      scale_size_manual(values=c(0.3,0.3,0.6)))
  
  ggsave(file.path(plot_folder,paste0('no2_',tolower(country_name),'_regions.png')), plt_regions, width=1024/300, height=800/300, scale=3, unit='in')
}

plot_regions_of('FR', creadb::NO2)
plot_regions_of('IT', creadb::NO2)
plot_regions_of('DE', creadb::NO2)



# 
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

if(!exists('meas_lombardia')){
  meas_lombardia <- lombardia.get_meas()  
}

plot_lombardia(meas_lombardia, c(creadb::NO2,creadb::PM10,creadb::CO,creadb::SO2))
plot_lombardia(meas_lombardia, creadb::NO2)
plot_lombardia(meas_lombardia, creadb::CO)
plot_lombardia(meas_lombardia, creadb::PM25)

