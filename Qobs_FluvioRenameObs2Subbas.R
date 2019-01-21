#Fluvio Observation Data
# rename to subbasins (subbas_cleanAnne@LUMP)

library(sf)
library(dplyr)
#Abflussstationen und Subbasins (shapefile) importieren

subbas<-st_read("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Obs_Fluvio/CleanedSubbasMap")

outlets<-st_read("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Obs_Fluvio/OutletPoints")



st_within(outlets,subbas)

readRDS()