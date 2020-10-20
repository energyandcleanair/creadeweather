# Getting Planet Boundary Layer

#NCP/NCAR Reanalysis: https://psl.noaa.gov/cgi-bin/db_search/DBSearch.pl?Dataset=NCEP+Reanalysis+Daily+Averages&Variable=Geopotential+height&group=0&submit=Search

ncep.folder <- function(){
  dir_ncep <- Sys.getenv("DIR_NCEP_REANALYSIS")
  if(dir_ncep==""){
    return(utils.get_bucket_mnt("ncep"))
  }else{
    return(dir_ncep)
  }
}

ncep.download_year <- function(year, force=T){
  dir_ncep <- ncep.folder()
  option <- ifelse(force," -N "," -nc ")
  cmd <- paste0("wget -P ", dir_ncep, option, "https://psl.noaa.gov/thredds/fileServer/Datasets/ncep.reanalysis/pressure/hgt.", year, ".nc")
  system(cmd)
}

ncep.add_pbl <- function(weather, years_force_refresh){
  positions <- weather
}