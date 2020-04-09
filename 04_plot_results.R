require(dplyr)
require(ggplot2)
require(countrycode)
require(revgeo)
require(sf)
source('99_crea_theme.R')
source('99_utils.R')

results_folder <- 'data/03_train_models/output/20200407_140932_lag7_novisibility_no2020_05deg_depth3'
result_rds_path <- file.path(results_folder, '20200407_140932_results.RDS')
data <- readRDS(result_rds_path)

# Remove redundant station_id from measurements
if('station_id' %in% colnames(data$meas_weather[[1]])){
  data <- data %>%  filter(!is.na(meas_weather)) %>% rowwise() %>% mutate(meas_weather=list(meas_weather %>% dplyr::select(-c(station_id))))
}

# Attach city to data
data <- utils.add_city(data)
data$country <- factor(countrycode::countrycode(data$gadm0_id, origin='iso3c', destination='country.name'), ordered = T)

# Country ranking boxplot
data <- data %>% rowwise() %>%
  mutate(march2020_value=utils.average_over_yearly_periods(meas_weather, 'value', years=c(2020), doys=c(62:93)),
         march2020_predicted=utils.average_over_yearly_periods(meas_weather, 'predicted', years=c(2020), doys=c(62:93))) %>%
  mutate(march2020_ratio=-1+march2020_value/march2020_predicted)

data$country <- reorder(data$country, data$march2020_ratio, FUN=median, na.rm=T)
(p1 <- ggplot(data %>% filter(!is.na(country))) + geom_boxplot(aes(y=country, x=march2020_ratio), outlier.shape =NA) + xlim(c(-1,1)) + labs(title='March 2020') + theme_crea() + geom_vline(xintercept = 0, color=pal_crea['Blue'], type=1))
ggsave(filename=file.path(results_folder,'boxplot_country_ranking.pdf'), p1)


# Map
data$march2020_ratio_c <- cut(data$march2020_ratio, labels=c("Decrease >100% (sic)",
                                                             "Decrease 60-100%",
                                                             "Decrease 40-60%",
                                                             "Decrease 20-40%",
                                                             "Decrease 10-20%",
                                                             "Decrease 5-10%",
                                                             "Decrease 0-5%",
                                                             "Increase 0-5%",
                                                             "Increase 5-10%",
                                                             "Increase 10-20%",
                                                             "Increase 20-40%",
                                                             "Increase 40-60%",
                                                             "Increase 60-100%",
                                                             "Increase > 100%"
),
breaks=c(-Inf,-1,-0.6,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.6,1,+Inf))

data$march2020_ratio_c <- reorder(data$march2020_ratio_c, desc(data$march2020_ratio_c))
plot.output_map(data %>% filter(!is.na(country)), result_folder=result_folder, timestamp_str = timestamp_str,
                meas_col='march2020_ratio_c', title="March 2020 - Value vs Predicted",
                scale=scale_fill_brewer(palette='RdBu', na.value="grey"),
                labs=labs(fill=""))





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
important_cities <- c('Zagreb - Centar','Sofia','Copenhagen','Tallinn','Riga','Versailles','Göteborg','Munich','Vatican City','Bristol')

ggplot(data %>% filter(city %in% important_cities) %>% ungroup() %>% filter(pollutant=='NO2') %>% select(country, pollutant, meas_weather, city, station_id) %>%
         tidyr::unnest(cols=meas_weather) %>%
         utils.rolling_average(average_by='day', average_width=7, group_cols=c('country', 'pollutant', 'city', 'station_id'), avg_cols=c('value')) %>%
         dplyr::select(country, pollutant, date, city, station_id, value, predicted) %>%
         dplyr::mutate(year=factor(lubridate::year(date))) %>%
         dplyr::mutate(yday=lubridate::yday(date)) %>%
         dplyr::mutate(date_in_year=lubridate::date(strftime(date,format="0000-%m-%d"))) %>%
         dplyr::filter(date>='2018-01-01', yday>=65, yday<=93)  %>% filter(!is.na(station_id))) + ylim(c(0, NA)) +
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






