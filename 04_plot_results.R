require(dplyr)
require(ggplot2)
require(countrycode)
require(revgeo)
require(sf)
require(scales)

source('99_crea_theme.R')
source('99_utils.R')

result_folder <- 'data/03_train_models/output/20200408_074129_lag7_novisibility_no2020_025deg_depth5'
result_rds_path <- file.path(results_folder, '20200408_074129_results.RDS')
timestamp_str <- '20200408_074129'
data <- readRDS(result_rds_path)

# Remove redundant station_id from measurements
if('station_id' %in% colnames(data$meas_weather[[1]])){
  data <- data %>%  filter(!is.na(meas_weather)) %>% rowwise() %>% mutate(meas_weather=list(meas_weather %>% dplyr::select(-c(station_id))))
}

# Attach city to data
data <- utils.add_city(data)
data$country <- factor(countrycode::countrycode(data$gadm0_id, origin='iso3c', destination='country.name'), ordered = T)


# Period of interest
doys <- c(lubridate::yday("2020-03-15"), lubridate::yday("2020-04-01"))

# Country ranking boxplot
data <- data %>% rowwise() %>%
  mutate(march2020_value=utils.average_over_yearly_periods(meas_weather, 'value', years=c(2020), doys=doys),
         march2020_predicted=utils.average_over_yearly_periods(meas_weather, 'predicted', years=c(2020), doys=doys)) %>%
  mutate(march2020_ratio=-1+march2020_value/march2020_predicted)

# We sort by NO2
data$country <- reorder(data$country, data$march2020_ratio * (ifelse(data$pollutant=='NO2',-1,NA)), FUN=median, na.rm=T)
library(grid)
text_high <- textGrob("Reduction", gp=gpar(fontsize=11, fill=pal_crea['Turquoise']))
text_low <- textGrob("Increase", gp=gpar(fontsize=11, fill=pal_crea['Green']))

(p1 <- ggplot(data %>% filter(!is.na(country))) +
    geom_boxplot(aes(y=country, x=march2020_ratio), outlier.shape =NA) +
    labs(title='Reduction in air pollutant concentration',
         subtitle='14 March 2020 - 1 April 2020 vs. predicted concentration') +
    theme_crea() +
    facet_grid(~pollutant) +
    geom_vline(xintercept = 0, color=pal_crea['Blue'], type=1) +
    scale_x_continuous(labels=percent, limits=c(-1,1)) + 
    theme(axis.title=element_blank()) +
    theme(plot.margin = unit(c(1,1,2,1), "lines")) +
    annotation_custom(text_high,xmin=-0.88,xmax=-0.88,ymin=-1.07,ymax=-1.07) + 
    annotation_custom(text_low,xmin=0.9,xmax=0.9,ymin=-1.07,ymax=-1.07) +
    coord_cartesian(clip = "off")
    )

ggsave(filename=file.path(result_folder,'boxplot_country_ranking_w_weather.png'), p1, width=1024/100, height=800/100, units = 'in')


# Map
data$march2020_ratio_c <- cut(data$march2020_ratio, labels=c("Decrease >40%",
                                                             "Decrease 20-40%",
                                                             "Decrease 10-20%",
                                                             "Decrease 0-10%",
                                                             "Increase 0-10%",
                                                             "Increase 10-20%",
                                                             "Increase 20-40%",
                                                             "Increase >40%"
),
breaks=c(-Inf,-0.4,-0.2,-0.1,0,0.1,0.2,0.4,+Inf))

data$march2020_ratio_c <- reorder(data$march2020_ratio_c, desc(data$march2020_ratio_c))
plot.output_map(data %>% filter(!is.na(march2020_ratio_c)), result_folder=result_folder, timestamp_str = timestamp_str,
                meas_col='march2020_ratio_c',
                title="Reduction in air pollutant concentration",
                scale=scale_color_brewer(palette='RdBu', na.value="grey"),
                labs=labs(fill="", colour="",
                          subtitle="14 March 2020 - 1 April 2020 vs. predicted concentration",
                          caption="Source: CREA based on European Environment Agency, NOAA, UNCAR"))


# National averages
data_national <- data %>% ungroup() %>% select(country, pollutant, meas_weather) %>%
  tidyr::unnest(cols=meas_weather) %>%
  group_by(country, pollutant, date) %>%
  summarise(value=mean(value, na.rm=T))

lockdown <- read.csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vTKMedY9Mzy7e81wWU95Ent79Liq7UwbUz0qTQbkSeAmFCPfqIVNbl1zs99bUOgsJUJbz53GxvBfeiP/pub?gid=0&single=true&output=csv'))
lockdown$date <- lubridate::date(lockdown$date)
lubridate::year(lockdown$date) <- 0

plot_national <- function(pollutant_){
  
  date_from <-lubridate::date('0000-02-01')
  date_to <-lubridate::date('0000-04-08')
  ggplot(data_national %>% filter(pollutant==pollutant_) %>% left_join(lockdown %>% rename(lockdown_date=date)) %>%
           utils.rolling_average(average_by='day', average_width=7, group_cols=c('country', 'pollutant'), avg_cols=c('value')) %>%
           # dplyr::select(country, pollutant, date, value, predicted) %>%
           dplyr::mutate(year=factor(lubridate::year(date))) %>%
           dplyr::mutate(yday=lubridate::yday(date)) %>%
           dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) %>%
           dplyr::filter(date>='2018-01-01', yday>=lubridate::yday(date_from), yday<=lubridate::yday(date_to))  %>% filter(!is.na(country))) + ylim(c(0, NA)) +
    geom_line(aes(x=date_in_year, y=value, color=year, linetype='solid')) +
    facet_wrap(~country, scales='free_y') + theme_crea() +
    scale_x_date(limits=c(date_from, date_to), breaks = seq(date_from, date_to, '1 month'),
                     labels=scales::date_format("%b", tz=attr(min(date_from),"tz"))) +
    labs(title=paste(pollutant_,'levels in selected countries'), subtitle='7 days rolling average',
         y ='Concentration (µg/m3)', colour="Year") +
    theme(axis.title.x=element_blank()) +
    geom_vline(data=lockdown, aes(xintercept=date, linetype='dashed'), linetype='dashed', colour=pal_crea['Turquoise']) +
    scale_linetype(name = "", labels=c('Lockdown')) +
    guides(linetype = guide_legend(override.aes = list(colour = pal_crea['Turquoise'], linetype='dashed')))
  }

plot_national('NO2')
plot_national('PM10')








# Examples = Value vs Predict
best_country_fit <- data %>% filter(!is.na(country)) %>% ungroup() %>%
  group_by(country, pollutant) %>%
  dplyr::top_n(n=1, wt=rsq_test)


ggplot(best_country_fit %>% ungroup() %>% filter(pollutant=='NO2') %>% select(country, pollutant, meas_weather, station_id, city) %>%
    tidyr::unnest(cols=meas_weather) %>%
    dplyr::select(country, pollutant, date, station_id, value, predicted) %>%
      dplyr::filter(date>='2020-01-01')  %>% filter(!is.na(station_id))%>%
    tidyr::gather(key="key",value="value",-c(country, station_id, pollutant, date))) + ylim(c(0, NA)) +
  geom_line(aes(x=date, y=value, color=key)) + facet_wrap(~country+station_id, scales='free_y') + theme_crea()

# Examples - 2018 vs 2019 vs 2020
selected_cities <- (data %>% arrange(march2020_ratio) %>% distinct(city) %>% head(10))$city
# selected_cities <- c('Zagreb - Centar','Sofia', 'Zürich (Kreis 10) / Wipkingen', 'Copenhagen', 'Madrid', 'Milan', 'Milano', 'Paris', 'Vienna','Berlin','Sotkcholm','Varsaw','Vatican City','Bristol')

ggplot(data %>% filter(city %in% selected_cities) %>% distinct(city, pollutant, .keep_all = T) %>% ungroup() %>% filter(pollutant=='NO2') %>% select(country, pollutant, meas_weather, city, station_id) %>%
         tidyr::unnest(cols=meas_weather) %>%
         utils.rolling_average(average_by='day', average_width=7, group_cols=c('country', 'pollutant', 'city', 'station_id'), avg_cols=c('value','predicted')) %>%
         dplyr::select(country, pollutant, date, city, station_id, value, predicted) %>%
         dplyr::mutate(year=factor(lubridate::year(date))) %>%
         dplyr::mutate(yday=lubridate::yday(date)) %>%
         dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) %>%
         dplyr::filter(date>='2018-01-01', yday>=lubridate::yday('2020-03-01'), yday<=lubridate::yday('2020-04-08'))  %>% filter(!is.na(station_id))) + ylim(c(0, NA)) +
  geom_line(aes(x=date_in_year, y=value, color=year)) + facet_wrap(~city, scales='free_y') + theme_crea() + labs(title='NO2 levels in selected cities', subtitle='7 days average')


ggplot(data %>% filter(city %in% important_cities) %>% ungroup() %>% filter(pollutant=='PM10') %>% select(country, pollutant, meas_weather, city) %>%
         tidyr::unnest(cols=meas_weather) %>%
         dplyr::select(country, pollutant, date, city, station_id, value, predicted) %>%
         dplyr::mutate(year=factor(lubridate::year(date))) %>%
         dplyr::mutate(yday=lubridate::yday(date)) %>%
         dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) %>%
         dplyr::filter(date>='2018-01-01', yday>=65, yday<=93)  %>% filter(!is.na(station_id))) + ylim(c(0, NA)) +
  geom_line(aes(x=date_in_year, y=value, color=year)) + facet_wrap(~city, scales='free_y') + theme_crea() + labs(title='PM10 levels in selected cities')

# Maps






