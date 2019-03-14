# Script for reading in discharge data (fluvio data)
# and collect multiple txt-files into one, aggregated by date

# Data structure: Folder with different txt-files of discharge for each station, Date of different time lengths
# Header: Date (YYYY/MM/DD) Q_mean_m3s (mean discharge in m3/s)
# Data encoding: best in UTF-8, but this script can also handle UCS-2LE

## Load packages ####

  library(dplyr)
  library(lubridate)
  library(readr)
  library(sf)
  library(stringr)
  library(tidyr)


## Select fluvio files ####

  setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/all_daily_txt/")
  files <- list.files(pattern=".txt")
  files
  write.csv(files,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/filenames.txt")
  #files <-c("CheshmehNaz-VanakSemirom_21-770-Discharge.txt","BandGhadimi_21-765-Discharge.txt")
  #files="BandGhadimi_21-765-Discharge.txt"
  #Encoding(files) #"unknown" if type is UCS-2LE
  

## Read data using loop and convert ####

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

    # Warning can be ignored: "Warning messages: x: x failed to parse."

## Save all obs data ####
    # Important: discharge=0 was set to NA!
    # Save with original station names
    write.table(DF,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all-stations.txt",row.names=F,sep=";",quote=F)

    # Manually add 1st line with subbas-ID instead of station names! Needed for next step
    
## Save data in format "discharge_obs_24.txt", as needed by WASA-SED ####
    # Important: dataobs-file 1st line has to contain subbas-ID
    # Format of header/column names: "date sub1 sub2 ..."
    
    #place to save file
    setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/")
    #read data

    # read file to reformat
    # Data needs to be continuous! Check script Qobs-mod-summary... to create cont. time series 
    dataobs=read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all_subID.csv",
                  header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
    #dataobs=obscon
    str(dataobs)  # check structure
    head(dataobs) # check header
   
    # reformat date columns of dataobs
    library(tidyr)

    # separate() creates different columns for year,month and day
    # in case of daily data
    dataobs=separate(data=dataobs,col = "date",into = c("year","month","day")) # for hourly data: c("year","month","day","hour")
    dataobs=as.data.frame(append(dataobs, 0, after = 3)) #append 0-column for hours
    
    # rename date columns
    library(plyr)
    dataobs=data.frame(dataobs)
    dataobs=rename(dataobs, c("year"="YYYY", "month"="MM","day"="DD","X0"="HH")) # "Error: All arguments must be named" -> Solution: library(plyr) after library(dplyr)
  
    # sort subbasin columns, ascending order: sub 1 ... sub n
    ## first we need to exclude the date columns
    newobs=subset(dataobs, select=c("YYYY","MM","DD","HH")) #seperate date columns
    subx <- dataobs[, -c(1:4)] #extract subbas data
    ##rename subbasins to numbers only
    names(subx) = sub(pattern="sub", replacement="", x=names(subx))
    #names(subx) = sub(pattern="X", replacement="", x=names(subx)) #in case subbas-name starts with X
    
    ##reformat names from "character" to "integer" and sort subbas columns by header name
    colorder=sort.int(as.integer(names(subx)), index.return = T)$ix
    subx=subx[,colorder]
    ## combine date columns and sorted subbas columns
    newobs=cbind(newobs,subx)
    ## check data head & structure
    head(newobs)
    str(newobs)
    
    #Save as "discharge_obs_24.txt"
    #Afterwards manually attach the header!
    write.table(newobs,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/discharge_obs_24.txt", sep="\t",row.names=F,quote=F) 

    ##Data to insert as header in "discharge_obs_24.txt"
    mat=matrix(c("YYYY","YYYY","YYYY", #first column
                 "MM","MM","MM",  
                 "DD","DD","DD",
                 "HH","HH","HH")
               , nrow = 3, ncol = 4)
    df=data.frame(mat)
    df=rename(df, c("X1"="discharge [m3/s]", "X2"=".","X3"="..","X4"="..."))
    df 
    #Export df as "header" file
    write.table(df,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/header.txt", sep="\t",row.names=F,quote=F) 
    
    #Copy and paste manually to first lines of "discharge_obs_24.txt"
    #Note: header has 4 lines, then obs-data starts
    
    
    # write.table_with_header <- function(x, file, header){
    #   cat(header, '\n',  file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/header.txt")
    #   write.table(x, file, append = T, sep="\t",row.names=F,quote=F)
    # }
    # 
    # write.table_with_header(x=,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/discharge_obs_24.txt", header) 
    # 
    
    # my.write <- function(x, file, header, f = write.csv, ...){
    #   # create and open the file connection
    #   datafile <- file(file, open = 'wt')
    #   # close on exit
    #   on.exit(close(datafile))
    #   # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
    #   if(!missing(header)) writeLines(header,con=datafile)
    #   # write the file using the defined function and required addition arguments  
    #   f(x, datafile,...)
    # }
    
              
## Check, if date is continuous ####
    
# read in your data  
checkfile=read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all_subbas.csv",
           header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
head(checkfile)
length(checkfile$date)
   # Example: 
      #length(checkfile$date)
      #[1] 23376
# create an artificial time series of dates you wish and test its length
testdate = seq(from = as.Date("1950-09-23"), to = as.Date("2016-09-21"), by = 'day')
length(testdate)

difference=length(testdate)-length(checkfile$date)
difference

#1951-09-23 until 1953-09-23 
test= seq(from = as.Date("1951-09-24"), to = as.Date("1953-09-22"), by = 'day')
length(test)

## Create continous time with Skript Qobs-mod_summary-NAcheck-plots.R ! ####

## Create nice summary table ####
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
    write.table(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/summary-Q_daily_all-stations.txt",sep=";",quote=F)
    #data 1980-2016
    #write.csv(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/summary-Q_daily_all_01-01-1980--21-09-2016.txt")
    
    

