
require(sp)
require(sf)
require(dplyr)
require(lubridate)
require(readr)
require(tidyr)
require(gstat)

prate=readRDS('~/proj/idw/prate.rds') %>% mutate(value=value*3600*24)

head(prate)

## the UTM code for Karun basin in Iran is N39. In epsg:
epsg=32639

obs_xy <- prate %>%
    st_as_sf(coords=c(1,2)) %>%   
    st_set_crs(4326) %>% # lat long
    st_transform(crs=epsg) %>% # transform to planar coordinate system for interpolation
    as(.,"Spatial") %>%
    as_data_frame() %>%
    rename(x=coords.x1,y=coords.x2)

locations <- obs_xy %>%
    distinct(x,y) %>%
    mutate(locations=seq(1,nrow(.)))
    
obs_xy = obs_xy %>% left_join(.,locations,by=c('x','y'))

require(gstat)

centroids <- read_tsv("~/proj/idw/centroids.txt") %>%
    st_as_sf(coords=c(1,2)) %>%   
    st_set_crs(4326) %>%
    st_transform(crs=epsg) %>%
    as(.,"Spatial") %>%
    as_tibble() %>%
    rename(x=coords.x1,y=coords.x2)


idwlist <- list()
j <- 0
for(dt in unique(obs_xy$time))
{
    print(paste0("idw interpolation of ",as_datetime(dt)))            
    j <- j+1
    dat <- filter(obs_xy,time==dt)
    gs <- gstat(formula=value~1,locations=~x+y,data=select(dat,x,y,value))
    idwlist[[j]] <- predict(gs,centroids,debug.level=0) %>% mutate(time=as_datetime(dt),cat=centroids$cat)
}

long <- do.call(rbind,idwlist) %>%
        as_tibble() %>%
        select(cat,x,y,time,var1.pred) %>%
        rename(value=var1.pred,date=time,location=cat) %>%
        arrange(date,location)



require(devtools)
install_github('jmigueldelgado/wasa.ops')
require(wasa.ops)

long2WASA_P(long,'~/proj/idw')
