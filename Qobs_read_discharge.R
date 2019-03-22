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


## Select obs-discharge files ####

  setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/Qobs-processed/Qobs-daily-txt/")
  files <- list.files(pattern=".txt")
  files
  write.table(files,file="D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/Qobs-processed/Qobs-filenames.txt",quote=F,col.names=F,row.names=F)
  #files <-c("CheshmehNaz-VanakSemirom_21-770-Discharge.txt","BandGhadimi_21-765-Discharge.txt")
  #files="BandGhadimi_21-765-Discharge.txt"
  #Encoding(files) #"unknown" if type is UCS-2LE
  

## Read data using loop and convert ####
  #Caution: Tab-separated data in txt-files assumed (works with sep="" = all blank space operators)
  #Missing data are set to NA: blank space "", "0"-values or "NA"

  DF <- NULL
  for (f in files) {        #loop over all files
    
      dat=tryCatch(         # fix problem with data encoding using tryCatch
            {
              dat<-read.table(f, header=TRUE,sep="",dec=".",na.strings=c("","0","NA","*","-9999.00","-9999","NaN"), fileEncoding="UTF-8",fill=T,skipNul=T,blank.lines.skip=TRUE)
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
              dat<-read.table(f, header=TRUE,sep="",dec=".",na.strings=c("","0","NA","*","-9999.00","-9999","NaN"),fileEncoding="UCS-2LE",fill=T,skipNul=T,blank.lines.skip=TRUE)
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

## Save all Qobs data in 1 file with station names####
    # Important: discharge=0 was set to NA!
    # Save with original station names
    write.table(DF,file="D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/Qobs-processed/Qobs_daily_all_statnames.txt",row.names=F,sep=";",quote=F)

    
## Rename column headers (station names) to subbas-ID####
  # Create a rename-file with 1 column, containing the following entries:
  # 1st entry "station name" (with quotation marks)
  # 2nd = (no quotation marks) 
  # 3rd subbas-ID (no quotation marks) 
  # Example:
  # "date"="date" 
  # "ArabHasan_21443"=4
  # Caution: "date"=date" is need to not rename the date column from DF
  
  # Read in the rename-file
  renamefile="D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Rename-file-statname-subbasID.txt"
  renamedf=read.table(renamefile, header = F,colClasses = "character", check.names=F,sep=c("=",",")) #data frame
  renamedf      #str(renamedf)
  # Create a rename vector
  library(plyr)
  as.matrix(renamedf)
  DFren=DF
  renamevect <- as.vector(renamedf$V2)
  names(renamevect)<- as.vector(renamedf$V1)
  renamevect
  # Rename data frame with all obs data from station name to subbas ID
  DFsub=rename(DFren, renamevect)
  DFsub
  str(DFsub)
  
## Check, if date of Qobs is continuous ####
  
  checkfile=DFsub
  
  # OR read in your data  
  # checkfile=read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all_subbas.csv",
  #                      header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
  
  head(checkfile)

  # Get start and end date of Qobs
  minQdate=min(checkfile$date)
  maxQdate=max(checkfile$date)
  minQdate
  maxQdate
  
  # Create a continuous time series of minimum and maximum date of Qobs and test its length
  #date = seq(from = as.Date(minQdate), to = as.Date(maxQdate), by = 'day')
  # Or specify a certain length (e. g., like in climate data)
  date = seq(from = as.Date("1950-01-01"), to = as.Date("2018-12-31"), by = 'day')
  
  #length(d)
  d=NULL                    #create empty variable
  d$date = data.frame(date) #attach date
  d=data.frame(d)           #convert to datafram
  
## Create continous time with Skript Qobs-mod_summary-NAcheck-plots.R ! ####

  #compare number of rows your data should have (d) with row numbers in your obs-data (obsraw)    
  if(nrow(d)>nrow(checkfile)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(checkfile),"missing entries.")) else message("Your Obs-data is continous.")
  
  #If continuous, go to line 99
  
  #merge continuous date with obs-data and fill gaps with NA:
  obscon <-  as_tibble(checkfile) %>%         #convert to dataframe
    mutate(date=ymd(as.character(date)))      #convert "date" from character to date format
  obscon=merge(d, obscon, by="date", all=T)   #merge continous date with obs-data and fill gaps with NA
  obscon= data.frame(obscon)
  
  #compare number of rows your data should have (d) with row numbers in your obs-data (obscon)    
  if(nrow(d)>nrow(obscon)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(obscon),"missing entries.")) else message("Your Obs-data is continous.")
 

  ## Save data in format "discharge_obs_24.txt" for WASA-SED validation####
    # Important: dataobs-file 1st line has to contain subbas-ID
    # Format of header/column names: "date sub1 sub2 ..."
    
    # Read Qobs
    # Data needs to be continuous! See above.
    dataobs=obscon
    # OR read file to reformat
    # dataobs=read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all_subID.csv",
    #               header = TRUE,  sep = ";", dec = ".",na.strings ="NA")

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
    names(subx) = sub(pattern="X", replacement="", x=names(subx)) #in case subbas-name starts with X
    #names(subx) = sub(pattern="sub", replacement="", x=names(subx))
    
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
    
    #place to save file
    setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/")
    
    write.table(newobs,file="discharge_obs_24.txt", sep="\t",row.names=F,quote=F) 

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
    #Saved under directroy "setwd()", to check apply "getwd()"
    write.table(df,file="header.txt", sep="\t",row.names=F,quote=F) 
    
    #Copy and paste manually to first lines of "discharge_obs_24.txt"
    #Note: header has 4 lines, then obs-data starts
    
    

## Create nice summary table ####
    # Important: discharge=0 was set to NA!
    
    # DF = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all-stations.txt",
    #   header = TRUE,  sep = ",", dec = ".",na.strings ="NA")
    #Data 01/01/1980-21/09/2016
    # DF = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_1980-2016_subbas.csv",
    # header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
    
    DF2=newobs
    str(DF2)
    summary(DF2)
    
    obs <- DF2[, -(1:4)]  #remove first 4 columns YYYY, MM, DD, HH
    str(obs)

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

    #Save file with dynamic file name
    setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/")
    getwd()
        #start date of Qobs cont data
          hDF2=head(DF2[,1:3],n=1) 
          # hDF2=as.matrix(hDF2)
          # hDF2=as.character(as.vector(hDF2[,1:3]))
          hDF2$min <- as.character(interaction(hDF2,sep="-"))
          min=hDF2$min
        #end date
          tDF2=tail(DF2[,1:3],n=1) 
          tDF2$max <- as.character(interaction(tDF2,sep="-"))
          max=tDF2$max
          #tDF2=as.matrix(tDF2)
          #tDF2=as.character(as.vector(tDF2[,1:3]))
    
    write.table(sumat,file=paste0("sumstat_Qobscont-dly_",min,"to",max,".txt"),sep=";",quote=F)
    

