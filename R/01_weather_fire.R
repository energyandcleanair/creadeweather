# Attaching fire information
# 2 x 2  options
# Using active fires points vs fire radiative power raster
# Using a buffer around geometry vs a buffer around trajectories (more time consuming)


#' Add fire related information to weather data
#'
#' @param weather the dataframe to add fire information to
#' @param source should we use FIRMS active fire data or GFAS emission data (either "gfas" or "viirs")
#' @param mode should we consider a simple buffer around geometry, a wind oriented buffer, or calculate trajectories.
#' attach_mode can be "circular", "oriented" or "trajectory"
#' @param buffer_km buffer around geometry or trajectories. Default 200km for `circular` and `oriented`, 10km for `trajectory`
#' @param met_type met_type used for hysplit (if mode=="trajectory" or mode=="dispersion")
#' @param height height used for hysplit (if mode=="trajectory")
#' @param duration_hour duration in hours used for hysplit (if mode=="trajectory" or mode=="dispersion")
#' @param trajs_height receptor height for trajectories in meter.
#' If null or NA, pbl average will be considered.
#'
#' @return
#' @export
#'
#' @examples
fire.add_fire <- function(weather,
                          source="gfas", #or "viirs"
                          mode="oriented",
                          met_type="gdas1",
                          duration_hour=72,
                          delay_hour=24,
                          buffer_km=NULL,
                          split_days=F,
                          trajs_height=NULL,
                          trajs_parallel=T,
                          trajs_height_default=50, 
                          save_trajs_filename=NULL){
  
  
  if(is.null(buffer_km)){
    buffer_km <- switch(mode,"circular"=200, "oriented"=200, "trajectory"=10)
  }
  
  if(source=="gfas"){
    attach_fn <- creatrajs::gfas.attach_to_trajs
    fire_vars_pattern <- "pm25_emission"
  }else{
    attach_fn <- creatrajs::fire.attach_to_trajs
    fire_vars_pattern <- c("^fire_.*")
  }
  
  cols <- setdiff(c(names(weather), "date", "trajs_height", "wd_copy", "ws_copy"),"weather")
  
  w <- tibble(weather) %>%
    tidyr::unnest(weather) %>%
    # We keep wind information (in case mode=='oriented')
    # as well as pbl for trajectory height (in case mode=='trajectory', used for height in splitr)
    rowwise() %>%
    mutate(wd_copy=wd,
           ws_copy=ws,
           trajs_height=ifelse(is.null(!!trajs_height) || is.na(!!trajs_height),
                                  ifelse(all(c("pbl_min","pbl_max") %in% names(.)),
                                         (pbl_min + pbl_max)/2,
                                         trajs_height_default),
                                  !!trajs_height)) %>%
    mutate(trajs_height=tidyr::replace_na(trajs_height, trajs_height_default)) %>%
    tidyr::nest(meas=!cols)
  
  if(mode=="trajectory"){
    
    print("Calculating trajs")
    wt <- w
    wt$trajs <- creatrajs::trajs.get(
        dates=wt$date,
        location_id=wt$location_id,
        geometry=wt$geometry,
        met_type=met_type,
        heights=wt$trajs_height,
        duration_hour=duration_hour,
        hours=seq(0,23,4), # To make it faster, we don't calculate trajectories every hour
        timezone=wt$timezone,
        cache_folder=NULL, #utils.get_cache_folder('trajs'),
        parallel=trajs_parallel
    )
    names(wt$trajs) <- NULL
    print("Done")
    
    if(!is.null(save_trajs_filename)){
      print(paste("Exporting trajectories to ", save_trajs_filename))
      saveRDS(wt, save_trajs_filename)
      print("Done")
    }
    
    print("Attaching fires to trajectories")
    wtf <- tryCatch({
      attach_fn(wt,
                buffer_km=buffer_km,
                parallel=trajs_parallel,
                mc.cores=max(round(parallel::detectCores()-2), 1),
                split_days=split_days)  
    }, error=function(e){
      print("Failed to attach fires. Adding NAs instead")
      return(wt %>%
        rowwise() %>%
        mutate(fires=list(tibble(fire_frp=NA, fire_count=NA))) %>%
        ungroup())
    })
    print("Done")
    
  # }else if(mode=="dispersion"){
  #   print("Calculating dispersion")
  #   wt <- w %>% rename(location_id=location_id)
  #   wt$dispersion <- creatrajs::dispersion.get(
  #     dates=wt$date,
  #     location_id=wt$location_id,
  #     geometry=wt$geometry,
  #     met_type=met_type,
  #     heights=wt$trajs_height,
  #     duration_hour=duration_hour,
  #     timezone=wt$timezone,
  #     cache_folder=utils.get_cache_folder('dispersion'),
  #     parallel=T
  #   )
  #   names(wt$dispersion) <- NULL
  #   print("Done")
  #   
  #   if(!is.null(save_trajs_filename)){
  #     print(paste("Exporting dispersion to ", save_trajs_filename))
  #     saveRDS(wt, save_trajs_filename)
  #     print("Done")
  #   }
  #   
  #   print("Attaching fires to dispersion")
  #   wtf <- attach_fn(wt, buffer_km=buffer_km)
  #   print("Done")
  #   
  }else if(mode=="oriented"){
    if(source=="gfas"){
      stop("GFAS not yet supported with oriented extent")
    }
    print("Calculating extents")
    wt <- w %>%
      rowwise() %>%
      rename(date_meas=date) %>%
      mutate(extent=creatrajs::trajs.oriented_extent(geometry,
                                        duration_hour=duration_hour,
                                        ws=ws_copy,
                                        wd=wd_copy))
    print("Done")
    
    print("Attaching fires to extents")
    wtf <- creatrajs::fire.attach_to_extents(wt, delay_hour=duration_hour)
    print("Done")
    
  }else if(mode=="circular"){
    print("Calculating extents")
    if(source=="gfas"){
      stop("GFAS not yet supported with circular extent")
    }
    
    wt <- w %>%
      rowwise() %>%
      rename(date_meas=date) %>%
      mutate(extent=creatrajs::trajs.circular_extent(geometry,
                                                     buffer_km=buffer_km))
    print("Done")
    
    print("Attaching fires to extents")
    wtf <- creatrajs::fire.attach_to_extents(wt, delay_hour=duration_hour)
    print("Done")
  }
  
  # Adding frp to weather data
  result <- weather %>%
    left_join(wtf %>%
                tidyr::unnest(fires) %>%
                dplyr::select_at(c("location_id"="location_id", "date",
                                   grep(fire_vars_pattern, names(.), value=T))) %>%
                tidyr::nest(frp=-c(location_id))) %>%
    rowwise() %>%
    mutate(weather=list(weather %>% left_join(frp))) %>%
    dplyr::select(-c(frp))
  
  return(result)
}
#' 
#' 
#' 
#' frp.geotiffs <- function(date_from, date_to, extent, ...){
#'   
#'   
#'   r <- MODIStsp::MODIStsp(
#'     gui = FALSE,
#'     start_date = format(lubridate::date(date_from)-lubridate::days(8), "%Y.%m.%d"), #Files are every 8 days. MODISstp doesn't find any file if inbetween
#'     end_date   = format(date_to,"%Y.%m.%d"),
#'     out_folder = file.path(Sys.getenv("DIR_MODIS"),"modisstp","processed"),
#'     out_folder_mod = file.path(Sys.getenv("DIR_MODIS"),"modisstp"),
#'     spatmeth = "tiles",
#'     # Tiles for India and Pakistan
#'     start_x = 23,
#'     end_x = 26,
#'     start_y = 5,
#'     end_y = 8,
#'     selcat = "Land Cover Characteristics - Thermal Anomalies and Fire",
#'     selprod = "ThermalAn_Fire_Daily_1Km (M*D14A1)",
#'     prod_version = "6",
#'     sensor = "Both",
#'     bandsel = "MaxFRP",
#'     download_server = "http",
#'     user = Sys.getenv("EARTHDATA_USR"),
#'     password = Sys.getenv("EARTHDATA_PWD"),
#'     downloader = "http",
#'     download_range = "Full",
#'     out_projsel = "Used Defined",
#'     output_proj = "3857",
#'     out_res_sel = "Native",
#'     out_res = NULL,
#'     resampling = "max",
#'     reprocess = F,
#'     delete_hdf = F,
#'     nodata_change = F,
#'     scale_val = F,
#'     out_format = "GTiff",
#'     ts_format = "R RasterStack",
#'     compress = "LZW",
#'     n_retries = 10,
#'     verbose = TRUE)
#'   
#'   
#'   frp.utils.date_rasters(rasters)
#' }
#' 
#' 
#' #' Archive active fire files were downloaded one year by one year
#' #' which creates memory issues. We split them in more digestible ones.
#' #' Should only be run once, manually on downloaded files.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' frp.split_yearly_activefire_by_date <- function(f){
#'   
#'   d <- data.table::fread(f)
#'   dates <- unique(d$acq_date)
#'   
#'   write_date <- function(date, d, f){
#'     f_date <- file.path(dirname(f),
#'                         gsub("[0-9]{4}", strftime(date, "%Y%j"),
#'                              gsub("csv", "txt", basename(f))))
#'     write.csv(d[d$acq_date==date], f_date, row.names=FALSE)
#'   }
#'   pbapply::pblapply(dates, write_date, d=d, f=f)
#' }