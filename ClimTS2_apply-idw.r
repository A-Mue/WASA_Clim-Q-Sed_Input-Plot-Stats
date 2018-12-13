                                      #
# Script to interpolate NCEP climate data to subbasin centroids
# and generate model input of climate timeseries for WASA-SED
#
# Authors: J. M. Delgado, A. Müller
#
# Required:
#     - subbasin centroids in Latitude Longitude projection (LatLon)
#     - rds-Files from NCEP/GPCC download (see script ClimTS1)
#     - line-wise execution of this script
#
# Note: Climate model input rounded to 0 digits.
#__________________________________________________________________

# Load packages (or first install, if needed)
require(devtools)
require(dplyr)
require(gstat)
require(lubridate)
require(readr)
require(sp)
require(sf)
require(tidyr)
install_github('jmigueldelgado/wasa.ops')
require(wasa.ops)


# Input (please adjust)
#-------------------------------
# Directory of rds-files (output of script ClimTS1; air.rds, dswrf.rds, prate.rds, rhum.rds)
myproj="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/scraping-download/" 
myproj=getwd()


                                        # Directory & name of subbas-centroid file
subcent=read_tsv("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/centroids_subbas_LatLong.txt")
subcent=read_tsv("./centroids_subbas_LatLong.txt")

# Your target projection code (see GrassGIS project for lumpR)
## Example: the UTM code for Karun basin in Iran is N39. In epsg:
epsg=32639

# Output (please adjust)
#-------------------------------
# Directory & folder name to save WASA-SED input files of climate timeseries
clim4wasa=("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/ClimateData_AnneJose/climTS4wasa")
clim4wasa=getwd()

#--------------------------------------------------------------------------------------
# The following steps need to be conducted for 4 climate variables (P, T, Rad, RH); no further adjustments necessary

# Read subbas-centroids, reproject to "epsg" (defined above) & add location-ID
centroids <- subcent %>%
  st_as_sf(coords=c(1,2)) %>%   
  st_set_crs(4326) %>%
  st_transform(crs=epsg) %>%
  as(.,"Spatial") %>%
  as_tibble() %>%
  rename(x=coords.x1,y=coords.x2)


#--------------------------------------------------------------------------------------
# Precipitation
#--------------------------------------------------------------------------------------

# Read data and convert unit from kg/m2/s to mm/d
prate=readRDS(paste0(myproj,'prate.rds')) %>% mutate(value=value*3600*24) 
head(prate)

# Reproject rds-data and name grid centroids (by location)
obs_xy <- prate %>%
    st_as_sf(coords=c(1,2)) %>%   
    st_set_crs(4326) %>%               # LatLon
    st_transform(crs=epsg) %>%         # Transform LatLon to planar coordinate system (epsg) for interpolation
    as(.,"Spatial") %>%       
    as_data_frame() %>%
    rename(x=coords.x1,y=coords.x2)

# Define location for inverse distance weighting (idw)
locations <- obs_xy %>%                # Centroids coordinates (x,y) of NCEP data grid  
    distinct(x,y) %>%                  # Select x,y-combinations occuring only once
    mutate(locations=seq(1,nrow(.)))   # Create location-ID number (1 to last entry of coordinates)

# Combine tables "obs_xy" and "locations"; attach column with location-ID    
obs_xy = obs_xy %>% left_join(.,locations,by=c('x','y'))

# Interpolate NCEP data to subbas-centroids using inverse distance weighting (idw)
# Note: idw based on x,y (not yet altitude)
idwlist <- list()
j <- 0
for(dt in unique(obs_xy$time))
{
    print(paste0("idw interpolation of ",as_datetime(dt)))            
    j <- j+1
    dat <- filter(obs_xy,time==dt)       # Filter for daily timesteps & obs-points
    gs <- gstat(formula=value~1,locations=~x+y,data=select(dat,x,y,value)) # Create geostatistical model (linear), depending on x,y
    idwlist[[j]] <- predict(gs,centroids,debug.level=0) %>% mutate(time=as_datetime(dt),cat=centroids$cat) # Interpolate NCEP data to subbas-centr.
}

    # idwlist[[1]] >%> head()    # Select and show day 1

# Reformat data to dataframe, rename columns, arrange data
long <- do.call(rbind,idwlist) %>%
        as_tibble() %>%
        select(cat,x,y,time,var1.pred) %>%
        rename(value=var1.pred,date=time,location=cat) %>%
        arrange(date,location)

# Create WASA-SED input for P and save in directory "clim4wasa" as "rain_daily.dat"

long2WASA_P(long,clim4wasa)



#--------------------------------------------------------------------------------------
# Temperature
#--------------------------------------------------------------------------------------

# Read data and convert unit from K to Degrees Celsius
air=readRDS(paste0(myproj,'air.rds')) %>% mutate(value=(value-273.15)) 
head(air)

air <- air %>%
  group_by(lon,lat,day=floor_date(time,"day")) %>%
  summarise(value=mean(value),var=first(var))

# Reproject rds-data and name grid centroids (by location)
obs_xy <- air %>%
    st_as_sf(coords=c(1,2)) %>%   
  st_set_crs(4326) %>%               # LatLon
  st_transform(crs=epsg) %>%         # Transform LatLon to planar coordinate system (epsg) for interpolation
  as(.,"Spatial") %>%       
  as_data_frame() %>%
  rename(x=coords.x1,y=coords.x2)



# Define location for inverse distance weighting (idw)
locations <- obs_xy %>%              # Centroids coordinates (x,y) of NCEP data grid  
  distinct(x,y) %>%                  # Select x,y-combinations occuring only once
  mutate(locations=seq(1,nrow(.)))   # Create location-ID number (1 to last entry of coordinates)

# Combine tables "obs_xy" and "locations"; attach column with location-ID    
obs_xy = obs_xy %>% left_join(.,locations,by=c('x','y')) %>% rename(time=day)

# Interpolate NCEP data to subbas-centroids using inverse distance weighting (idw)
# Note: idw based on x,y (not yet altitude)
idwlist <- list()
j <- 0
for(dt in unique(obs_xy$time))
{
  print(paste0("idw interpolation of ",as_datetime(dt)))            
  j <- j+1
  dat <- filter(obs_xy,time==dt)       # Filter for daily timesteps & obs-points
  gs <- gstat(formula=value~1,locations=~x+y,data=select(dat,x,y,value)) # Create geostatistical model (linear), depending on x,y
  idwlist[[j]] <- predict(gs,centroids,debug.level=0) %>% mutate(time=as_datetime(dt),cat=centroids$cat) # Interpolate NCEP data to subbas-centr.
}

# Reformat data to dataframe, rename columns, arrange data
long <- do.call(rbind,idwlist) %>%
  as_tibble() %>%
  select(cat,x,y,time,var1.pred) %>%
  rename(value=var1.pred,date=time,location=cat) %>%
  arrange(date,location)

# Create WASA-SED input for T and save in directory "clim4wasa" as "temperature.dat"

long2WASA_T(long,clim4wasa)



#--------------------------------------------------------------------------------------
# Radiation
#--------------------------------------------------------------------------------------

# Read data
dswrf=readRDS(paste0(myproj,'dswrf.rds')) # unit W/m2, no conversion needed
head(dswrf)

# Reproject rds-data and name grid centroids (by location)
obs_xy <- dswrf %>%
  st_as_sf(coords=c(1,2)) %>%   
  st_set_crs(4326) %>%               # LatLon
  st_transform(crs=epsg) %>%         # Transform LatLon to planar coordinate system (epsg) for interpolation
  as(.,"Spatial") %>%       
  as_data_frame() %>%
  rename(x=coords.x1,y=coords.x2)

# Define location for inverse distance weighting (idw)
locations <- obs_xy %>%              # Centroids coordinates (x,y) of NCEP data grid  
  distinct(x,y) %>%                  # Select x,y-combinations occuring only once
  mutate(locations=seq(1,nrow(.)))   # Create location-ID number (1 to last entry of coordinates)

# Combine tables "obs_xy" and "locations"; attach column with location-ID    
obs_xy = obs_xy %>% left_join(.,locations,by=c('x','y'))

# Interpolate NCEP data to subbas-centroids using inverse distance weighting (idw)
# Note: idw based on x,y (not yet altitude)
idwlist <- list()
j <- 0
for(dt in unique(obs_xy$time))
{
  print(paste0("idw interpolation of ",as_datetime(dt)))            
  j <- j+1
  dat <- filter(obs_xy,time==dt)       # Filter for daily timesteps & obs-points
  gs <- gstat(formula=value~1,locations=~x+y,data=select(dat,x,y,value)) # Create geostatistical model (linear), depending on x,y
  idwlist[[j]] <- predict(gs,centroids,debug.level=0) %>% mutate(time=as_datetime(dt),cat=centroids$cat) # Interpolate NCEP data to subbas-centr.
}

# Reformat data to dataframe, rename columns, arrange data
long <- do.call(rbind,idwlist) %>%
  as_tibble() %>%
  select(cat,x,y,time,var1.pred) %>%
  rename(value=var1.pred,date=time,location=cat) %>%
  arrange(date,location)

# Create WASA-SED input for R and save in directory "clim4wasa" as "radiation.dat"

long2WASA_R(long,clim4wasa)



#--------------------------------------------------------------------------------------
# Humidity
#--------------------------------------------------------------------------------------

# Read data and convert unit from K to Degrees Celsius
rhum=readRDS(paste0(myproj,'rhum.rds')) # unit %, no conversion needed 
head(rhum)

# Reproject rds-data and name grid centroids (by location)
obs_xy <- rhum %>%
  st_as_sf(coords=c(1,2)) %>%   
  st_set_crs(4326) %>%               # LatLon
  st_transform(crs=epsg) %>%         # Transform LatLon to planar coordinate system (epsg) for interpolation
  as(.,"Spatial") %>%       
  as_data_frame() %>%
  rename(x=coords.x1,y=coords.x2)

# Define location for inverse distance weighting (idw)
locations <- obs_xy %>%              # Centroids coordinates (x,y) of NCEP data grid  
  distinct(x,y) %>%                  # Select x,y-combinations occuring only once
  mutate(locations=seq(1,nrow(.)))   # Create location-ID number (1 to last entry of coordinates)

# Combine tables "obs_xy" and "locations"; attach column with location-ID    
obs_xy = obs_xy %>% left_join(.,locations,by=c('x','y'))

# Interpolate NCEP data to subbas-centroids using inverse distance weighting (idw)
# Note: idw based on x,y (not yet altitude)
idwlist <- list()
j <- 0
for(dt in unique(obs_xy$time))
{
  print(paste0("idw interpolation of ",as_datetime(dt)))            
  j <- j+1
  dat <- filter(obs_xy,time==dt)       # Filter for daily timesteps & obs-points
  gs <- gstat(formula=value~1,locations=~x+y,data=select(dat,x,y,value)) # Create geostatistical model (linear), depending on x,y
  idwlist[[j]] <- predict(gs,centroids,debug.level=0) %>% mutate(time=as_datetime(dt),cat=centroids$cat) # Interpolate NCEP data to subbas-centr.
}

# Reformat data to dataframe, rename columns, arrange data
long <- do.call(rbind,idwlist) %>%
  as_tibble() %>%
  select(cat,x,y,time,var1.pred) %>%
  rename(value=var1.pred,date=time,location=cat) %>%
  arrange(date,location)

# Create WASA-SED input for T and save in directory "clim4wasa" as "humidity.dat"

long2WASA_H(long,clim4wasa)



