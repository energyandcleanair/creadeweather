#' Training a model using svr
#'
#'
#' @param station_id 
#' @param data 
#' @param pollutant 
#' @param unit 
#' @param training_date_cut
#' @param link: either null or 'log'
#'
#' @return
#' @export
#'
#' @examples
train_model_svr <- function(data,
                                  training_date_cut,
                                  weather_vars,
                                  time_vars,
                                  normalise,
                                  samples,
                                  link=NULL,
                                  ...){
  
  n_cores <- as.integer(future::availableCores()-1)
  
  link <- if(is.null(link) || is.na(link)) NULL else link

  # Using deweather to prepare data re time vars
  # Correspondance between our time variables and deweather ones
  # our=deweather
  time_vars_corr <- list(
    "trend"="trend",
    "wday"="weekday",
    "month"="month",
    "week"="week",
    "yday"="jday",
    "hour"="hour"
  )
  
  if(any(!time_vars %in% c(names(time_vars_corr), colnames(data)))){
    stop(paste("Deweather can only create the following timevars:", paste(names(time_vars_corr), collapse=",")))
  }else{
    time_vars <- c(unlist(time_vars_corr[time_vars], use.names=F), setdiff(time_vars,names(time_vars_corr)))
  }
  
  if(!is.null(link) && (link!='log')){
    stop("link can only be NULL or 'log'")
  }
  
  
  data_prepared <- data %>%
    mutate(date=as.POSIXct(date)) %>%
    deweather::prepData(add=time_vars)
 
  data_prepared[data_prepared$date >= training_date_cut,'set'] <- "testing"
  data_prepared[data_prepared$date <= training_date_cut,'set'] <- "training" # Actually, gbm will use a fraction of it for validation
  
  # Creating model
  model_svm  <- function(training_data, formula){
    print("Training SVR")
    model_ <- e1071::svm(
      formula = formula,
      data = as.matrix(training_data)
    )
    print("Done")
    return(model_)
  }

  formula_vars <- c(time_vars, weather_vars)
  formula <- reformulate(termlabels=formula_vars,
                         response='value')  
  
  data_prepared <- data_prepared %>%
    dplyr::filter_at(formula_vars, any_vars(!is.na(.))) %>%
    dplyr::filter_at("value", all_vars(!is.na(.)))
  
  # Add "link" transformation if required
  if(!is.null(link) && link=='log'){
    data_prepared$value <- log(data_prepared$value)
  }

  
  #----------------
  # Fit model
  #----------------
  model <- model_svm(data_prepared %>% dplyr::filter(set=="training") %>%
                       dplyr::select(c("value", formula_vars)), formula) 
  
  #----------------
  # Predict
  #----------------
  data_prepared$predicted <- predict(model, as.matrix(data_prepared %>% dplyr::select(formula_vars)))
  
  
  if(!is.null(link) && (link=='log')){
    data_prepared$value <- exp(data_prepared$value)
    data_prepared$predicted <- exp(data_prepared$predicted)
  }
  
  
  data_prepared$residuals <- data_prepared$predicted - data_prepared$value
  
  data_test <- data_prepared %>% filter(set=="testing") %>% filter(!is.na(value))
  model$rmse_test <- Metrics::rmse(data_test$value, data_test$predicted)
  model$mae_test <- Metrics::mae(data_test$value, data_test$predicted)
  model$rsquared_test <- 1 - sum((data_test$predicted - data_test$value)^2) / sum((data_test$value - mean(data_test$value))^2)

  data_training <- data_prepared %>% filter(set=="training") %>% filter(!is.na(value))
  model$rmse_training <- rmse(data_training$value, data_training$predicted)
  model$mae_training <- mae(data_training$value, data_training$predicted)
  model$rsquared_training <- 1 - sum((data_training$predicted - data_training$value)^2) / sum((data_training$value - mean(data_training$value))^2)

  # save space
  model_light <- model
  model_light$trees<- NULL
  
  res <- tibble(model=list(model_light),
                predicted=list(data_prepared %>% dplyr::select(date, set, value, predicted))
  )
   
  if(normalise){
    warning("Normalised not managed yet for gbm engine. Use deweather instead if normalisation is your goal")
  }
  
  if(length(time_vars)>0){
    
    dates <- tibble(date=lubridate::date(unique(data_prepared %>% filter(set=='training') %>% pull(date)))) %>%
      dplyr::mutate(yday_joiner=lubridate::yday(date)) %>%
      dplyr::mutate(month_joiner=lubridate::month(date)) %>%
      dplyr::mutate(wday_joiner=lubridate::wday(date, week_start=1)) %>%
      dplyr::mutate(season_joiner = forcats::fct_collapse(
        .f = factor(lubridate::month(date)),
        Spring = c("3","4","5"),
        Summer = c("6","7","8"),
        Autumn = c("9","10","11"),
        Winter = c("12","1","2")
      ))
      
    
    if("trend" %in% time_vars){
      trend_trend <- plot.gbm(model, "trend", continuous.resolution = nrow(dates)*2, return.grid = T) %>%
        rowwise() %>%
        mutate(y=ifelse(!is.null(link) && (link=='log'),exp(y),y)) %>%
        mutate(date=lubridate::date(lubridate::date_decimal(trend))) %>%
        dplyr::select(date,trend=y) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(trend=mean(trend, na.rm=T))
      
      dates <- dates %>% left_join(trend_trend)
    }
    
    if("jday" %in% time_vars){
      trend_jday <- plot.gbm(model, "jday",continuous.resolution = 366, return.grid = T) %>%
        rowwise() %>%
        mutate(y=ifelse(!is.null(link) && (link=='log'),exp(y),y)) %>%
        mutate(jday=round(jday)) %>% dplyr::select(yday_joiner=jday, jday=y)
      
      dates <- dates %>% left_join(trend_jday)
    }
    
    if("month" %in% time_vars){
      trend_month <- plot.gbm(model, "month", return.grid = T) %>%
        rowwise() %>%
        mutate(y=ifelse(!is.null(link) && (link=='log'),exp(y),y)) %>%
        dplyr::select(month_joiner=month, month=y)
      trend_month$month_joiner <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,
                             aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(trend_month$month_joiner)]
      
      dates <- dates %>% left_join(trend_month)
    }
    
    if("weekday" %in% time_vars){
      trend_weekday <- plot.gbm(model, "weekday", return.grid = T) %>%
        rowwise() %>%
        mutate(y=ifelse(!is.null(link) && (link=='log'),exp(y),y)) %>%
        dplyr::select(wday_joiner=weekday, weekday=y)
      trend_weekday$wday_joiner <- c(monday=1,tuesday=2,wednesday=3,thursday=4,friday=5,
                                     saturday=6,sunday=7)[tolower(trend_weekday$wday_joiner)]
      
      dates <- dates %>% left_join(trend_weekday)
    }
    
    if("season" %in% time_vars){
      trend_season <- plot.gbm(model, "season", return.grid = T) %>%
        rowwise() %>%
        mutate(y=ifelse(!is.null(link) && (link=='log'),exp(y),y)) %>%
        dplyr::select(season_joiner=season, season=y)
      
      dates <- dates %>% left_join(trend_season)
    }
    # trend <- tibble(dates) %>% dplyr::select_at(c(time_vars,"date"))
    trend <- tibble(
      date=dates$date,
      value= dates %>% dplyr::select(time_vars) %>% rowSums(na.rm=TRUE))

    # trend$value <- trend$value
    res$trend <- list(tibble(trend))
  }
  
  res
}
