# Copyright (C) 2019 José Miguel Delgado, Anne Müller 

# Fluvio Observation Data from Rio Sao Francisco, Brazil, as provided by ANA
# rename to subbasins (subbas_cleanAnne@LUMP)

library(sf)
library(dplyr)
#Abflussstationen und Subbasins (shapefile) importieren

subbas<-st_read("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Obs_Fluvio/CleanedSubbasMap")

outlets<-st_read("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Obs_Fluvio/OutletPoints")



st_within(outlets,subbas)

readRDS()