# Script for reading in discharge data (fluvio data)
# and collect multiple txt-files into one, aggregated by date

# Data structure: Folder with different txt-files of discharge for each station, Date of different time lengths
# Header: Date (YYYY/MM/DD) Q_mean_m3s (mean discharge in m3/s)
# Data encoding: best in UTF-8, but this script can also handle UCS-2LE


  library(dplyr)
  library(lubridate)
  library(readr)
  library(sf)
  library(stringr)
  library(tidyr)


## select fluvio files

  setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/all_daily_txt/")
  files <- list.files(pattern=".txt")
  files
  write.csv(files,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/filenames.txt")
  #files <-c("CheshmehNaz-VanakSemirom_21-770-Discharge.txt","BandGhadimi_21-765-Discharge.txt")
  #files="BandGhadimi_21-765-Discharge.txt"
  #Encoding(files) #"unknown" if type is UCS-2LE
  

## read data using loop and convert

  DF <- NULL
  for (f in files) {        #loop over all files
    
      dat=tryCatch(         # fix problem with data encoding using tryCatch
            {
              dat<-read.table(f, header=TRUE,sep="",dec=".",na.strings=c("","0","NA"), fileEncoding="UTF-8",fill=T,skipNul=T,blank.lines.skip=TRUE)
            },
            
            error=function(err) {
              message(paste("| Error: read.table failed for  ",f,"|"))
              ##activate orginal error message if needed
              # message("Original error message:")
              # message(err)
              # message("                    ")
              message("Check data structure!")
              #return(NA)                     #choose a return value in case of error
            },
            
            warning=function(war) {           #if warning occurs, repeat read.table with encoding UCS-2LE
              message(paste("Warning:  ",f,"  data encoding is not UTF-8!"))
              ##activate orginal warning message if needed
              # message("Original warning message:")
              # message(war)
              # message("                    ")
              message("Repeated read.table with encoding UCS-2LE")  
              dat<-read.table(f, header=TRUE,sep="",dec=".",na.strings=c("","0","NA"),fileEncoding="UCS-2LE",fill=T,skipNul=T,blank.lines.skip=TRUE)
              # return(head(dat,n=2L))  #show first 2 lines of data
              # return(dat)
            }
          )
    
      dat <-  as_tibble(dat) %>%                           #convert to dataframe
        mutate(date=ymd(as.character(Date))) %>%  #Mutate adds new variables and preserves existing
        select(date,Q_mean_m3s) %>%
        group_by(date) %>%                        #group data by "date" column
        slice(1) %>%
        ungroup
      
      dat=data.frame(dat)
      DF=data.frame(DF)
      colnames(dat)[2] = str_replace(f, ".txt", "") #rename columns with file name
      
      if (ncol(DF) == 0) {
        DF=dat 
      } else { 
        DF<- merge(DF,dat,by="date", all=TRUE)
      }
    
  }   #end loop



## Save data
# Important: discharge=0 was set to NA!
    write.csv(DF,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all-stations.txt")


## Create nice summary table
    # Important: discharge=0 was set to NA!
    # DF = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all-stations.txt",
    #   header = TRUE,  sep = ",", dec = ".",na.strings ="NA")
    
    #Data 01/01/1980-21/09/2016
    DF = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_1980-2016_subbas.csv",
    header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
    summary(DF)
    
    obs <- DF[, -(1)]  
    sumstat = do.call(cbind, lapply(obs, summary))
    sumstat = round(sumstat, digits=2)
    
    Fraction_NA = round(colMeans(is.na(obs)), digits=2)
    Percentage_NA = round(Fraction_NA*100, digits=2)
    
    sumstat = rbind(sumstat,Fraction_NA,Percentage_NA)
    #str(sumstat)
    
    #summary in transposed matrix (rows=stations, columns=statistics)
    sumat=as.matrix(sumstat)
    sumat=t(sumat)
    sumat            

    #save
    #write.csv(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/summary-Q_daily_all-stations.txt")
    #data 1980-2016
    write.csv(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/summary-Q_daily_all_01-01-1980--21-09-2016.txt")
    
    

