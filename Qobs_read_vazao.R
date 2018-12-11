# Script for reading in discharge data (fluvio data) and sediment from ANA Hidroweb
# and collect multiple txt-files into one, aggregated by date

# Data structure: Folder with different files of VAZAO for each station

library(sf)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)


## take fluvio files
setwd("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/Abfluss/")
files <- list.files(pattern=".txt")

## read data using loop and convert
DF <- NULL
for (f in files) {
dat <- read.table(f, header=TRUE,sep=";",skip=16,dec=",") %>%
  as_tibble() %>%                         #convert to dataframe
  filter(MediaDiaria==1) %>%              #start with column named "MediaDiaria"
  rename(location=`X..EstacaoCodigo`) %>%
  select(-ends_with("Status"),Data) %>%   #select all columns ending with "Status" and column "Data"(=date)
  gather(dom,value,Vazao01:Vazao31) %>%   #Gather takes multiple columns and collapses into key-value pairs
  mutate(date=ymd(as.character(Data))+as.numeric(substr(dom,6,7))-1,value=round(value,1)) %>%   #mutate adds new variables and preserves existing
  select(date,value) %>%
  group_by(date) %>%                      #group data by "date" column
  slice(1) %>%
  ungroup

#DF <- rbind(DF, dat)
dat=data.frame(dat)
DF=data.frame(DF)
colnames(dat)[2] = str_replace(f, ".txt", "") #rename columns with file name

if (ncol(DF) == 0) {
  DF=dat 
  } else { 
    DF<- merge(DF,dat,by="date", all=TRUE)
    }
}


write.csv(DF,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/Fluvio_timeseries_ANA.txt")

summary(DF)



## take sediment files
setwd("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/Sediment/")
files <- list.files(pattern=".txt")

## read data using loop and convert
## MUSS NOCH AN STRUKTUR DER SED.DATEN ANGEPASST WERDEN! AH ZEILE 58
SED <- NULL
for (f in files) {
  sdat <- read.table(f, header=TRUE,sep=";",skip=14,dec=",") %>%
    as_tibble() %>%
    filter(MediaDiaria==1) %>%
    rename(location=`X..EstacaoCodigo`) %>%
    select(-ends_with("Status"),Data) %>%
    gather(dom,value,Vazao01:Vazao31) %>%
    mutate(date=ymd(as.character(Data))+as.numeric(substr(dom,6,7))-1,value=round(value,1)) %>%
    select(date,value) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup
  
  #DF <- rbind(SED, sdat)
  sdat=data.frame(sdat)
  SED=data.frame(SED)
  colnames(dat)[2] = str_replace(f, ".txt", "")
  
  if (ncol(SED) == 0) {
    SED=sdat 
  } else { 
    SED<- merge(SED,sdat,by="date", all=TRUE)
  }
}


write.csv(SED,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/Sed_timeseries_ANA.txt")

summary(SED)




