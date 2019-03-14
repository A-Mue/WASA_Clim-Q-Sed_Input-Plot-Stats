##############
## get-ncep ##
##############

# Example template to download and process NCEP and GPCC reanalysis data

# Install/load packages

  library(devtools)
  library(dplyr)
  #install.packages("lwgeom")
  library(lwgeom)
  #install.packages("lubridate")
  library(lubridate)
  #install.packages("ncdf4")
  library(ncdf4)
  #install.packages("R.utils")
  library(R.utils)
  #install.packages("sf")
  library(sf)
  install_github('jmigueldelgado/scraping',force=T)
  library(scraping)


# Define request, download and convert from ncdf to data frame and save

  # Select (multiple) coordinates OR region extent
    # coordinates as points 
      # centroids = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/centroids_subbas_LatLong.txt",
      #                      header = TRUE,  sep = "", dec = ".")
      # centroids = data.frame(centroids)
      # coor <- data.frame(lon=centroids$long, lat=centroids$lat) #enter your coordinates here, e.g. #coor <- data.frame(lon=13.40,lat=52.52)
      
    # OR
    # region extend
      #left=W, right=E, bottom=S, top=N
      coor <- data.frame(l=47.41,r=52.44,b=29.69,t=34.33)
      
    
  # Select parameters and time
  # Data source: 
  # NCEP data for T (var='temperature'), RH ('relative humidity'), and Rad ('net radiation')
      # also available for P ('precipitation rate'), but coarser resolution than GPCC data
  # GPCC data for P (var='gpcc precipitation') with number of gauging stations used for these data ('number of gauges')
      
  var <- c('temperature','relative humidity','net radiation','precipitation rate')  #all available NCEP variables: c('temperature','relative humidity','u wind','v wind','soil heat flux','net radiation','precipitation rate')
  var <- c('gpcc precipitation')#,'number of gauges')       # GPCC 
  
  years <- as.character(c(1950:2018))  # years <- c('1980','1981')
 
  
  # Set working directory (location to store downloaded files)
  setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/scraping-download/")
 
  # Define request
  request <- def_request(coor,var,years) # coor, var & years to be extracted from nf-files
  knitr::kable(request)  # formatted table of request
  
  # Download and convert from ncdf to data frame and save

  get_nc(request) # download nc-files
  
  nc2rds(request) # create rds-files for defined request

  
#----------------------------------------------------------  

# Print metadata

  get_nc_meta(request,var[2])

  
#----------------------------------------------------------

# Load and view rds data examples

# Install/load packages

library(scraping)
library(dplyr)
library(lubridate)

# Directory of downloaded NCEP-files
myproj="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/scraping-download/" 

# To load the examples, the rds-Files in the above directory are used

# Load 'temperature'

  var=lookup_var('temperature') %>% pull(varname)
  
  df1=readRDS(paste0(myproj,var,'.rds'))
  
  head(df1) %>% knitr::kable()
  
  df1 %>%
    group_by(day=floor_date(time,"day")) %>%
    summarise(daily_max=max(value),daily_min=min(value),daily_mean=mean(value),var=first(var)) %>%
    head() %>%
    knitr::kable()


# Load 'relative humidity'

  var=lookup_var('relative humidity') %>% pull(varname)
  df2=readRDS(paste0(myproj,var,'.rds'))
  
  head(df2) %>% knitr::kable()
  
  df2 %>%
    group_by(day=floor_date(time,"day")) %>%
    summarise(daily_max=max(value),daily_min=min(value),daily_mean=mean(value),var=first(var)) %>%
    head() %>%
    knitr::kable()


# Load 'net radiation'

  var=lookup_var('net radiation') %>% pull(varname)
  df3=readRDS(paste0(myproj,var,'.rds'))
  
  head(df3) %>% knitr::kable()
  
  df3 %>%
    group_by(day=floor_date(time,"day")) %>%
    summarise(daily_max=max(value),daily_min=min(value),daily_mean=mean(value),var=first(var)) %>%
    head() %>%
    knitr::kable()


# Load 'precipitation rate'

  var=lookup_var('precipitation rate') %>% pull(varname)
  df4=readRDS(paste0(myproj,var,'.rds'))
  
  head(df4) %>% knitr::kable()
  
  df4 %>%
    group_by(day=floor_date(time,"day")) %>%
    summarise(daily_max=max(value),daily_min=min(value),daily_mean=mean(value),var=first(var)) %>%
    head() %>%
    knitr::kable()
