# Script for reading in precipitation observation data (Pobs)
# and collect multiple txt-files into one, aggregated by date

# Copyright (C) 2019 Anne Müller, José Miguel Delgado 

# Requirements:
# Data structure: Folder with different txt-files of P for each station, Date of different time lengths
# Data file:  Header: Date (YYYY/MM/DD) P_mm (daily rainfall)
#             Empty line at end of file!
# Data encoding: best in UTF-8, but this script can also handle UCS-2LE

## 0.1) Load packages ####

#list = c("dplyr", "lubridate","readr","sf","stringr", "tidyr","tidyverse")
#install.packages(list)

  library(dplyr)
  library(lubridate)
  library(readr)
  library(sf)
  library(stringr)
  library(tidyr)
  #library(magrittr)



## 0.2) Select obs-precipitation files ####

#setwd("~/Desktop/ObsRainfall_DataCollection4BA/P_mm_Stationen_test2")
setwd("E:/Anne/__UP__/_BA&Projekt-Betreuung/2019-5_BA_JulianeGeißler_KarunDezNS-Analyse/P_proc/P_mm_Stationen/")


files <- list.files(pattern=".txt")
files
#files= c("AbBarik-21-065.txt","AbooEshagh_21-653.txt")
#files= ("AbBarik-21-065.txt")
#f="AbBarik-21-065.txt"

# Optional: try to guess file encoding
    #guess_encoding(files, n_max = 1000)
    #Encoding(files) #"unknown" if type is UCS-2LE


## 0.3) Export file names & create rename file for interpol-Script (rainfall interpolation to subbas centroids) ####
fnames=files
fnames= sub(pattern=".txt", replacement="", x=fnames) #save list without ".txt" in filenames
#write.table(fnames,file="_filenames.dat",quote=T,col.names=F,row.names=F)

# Create empty data frame
DFnames = NULL 
DFnames$fnames=fnames # add column with original file names
DFnames

library(stringr)
sn=fnames # create short form of file names
sn1=str_sub(sn, 1, 1) # get first character of file name
sn3=str_sub(sn, -3, -1) # get last 3 characters
snames=paste0(sn1,sn3, collapse = NULL) # combine short name string

DFnames$snames=snames # add column with short file names
DFnames

# Create "Y", "M", "D" for rename file (shall not be renamed)
mat=matrix(c("Y","M","D", #first column
             "Y","M","D")
           , nrow = 3, ncol = 2)
mat=data.frame(mat)
names(mat)=c("fnames","snames")
#str(mat) #data type has to be equal for rbind
#str(DFnames)
mat$fnames=as.character(mat$fnames) #reformat to character
mat$snames=as.character(mat$fnames)

DFrename=rbind(mat, DFnames) #data frame for renaming
DFrename

#Save as rename file
write.table(DFrename,file="_rename4interpol.dat",quote=T,col.names=F,row.names=F,sep="=")


## 1) Read data using loop and convert ####
  #Caution: 
  #   Tab-separated data in txt-files assumed (works with sep="" = all blank space operators)
  #   Error fix: Make sure, last line of data-file is empty!
  
  #Missing data are set to NA: blank space "", "0"-values or "NA"

  DF <- NULL
  for (f in files) {        #loop over all files
    
      dat=tryCatch(         # fix problem with data encoding using tryCatch
            {
              dat<-read.table(f, header=TRUE,sep="",dec=",",na.strings=c("","NA","*","-9999.00","-9999","NaN"), fileEncoding="UTF-8",fill=T,skipNul=T,blank.lines.skip=TRUE)
              #dat<-read.table(f, header=TRUE,sep="",dec=".",na.strings=c("","0","NA","*","-9999.00","-9999","NaN"), fileEncoding="UTF-8",fill=T,skipNul=T,blank.lines.skip=TRUE)
              # dat$Date=as.Date(dat$Date,"%d.%m.%Y")
             #dat$Date <- format(as.Date(dat$Date, format = "%d.%m.%Y"), "%Y/%m/%d")
              },
            
            error=function(err) {
              message(paste("| Error: read.table failed for  ",f,"|; check if last line of file is empty (clear end-of-file)."))
              ##activate orginal error message if needed
              # message("Original error message:")
              # message(err)
              # message("                    ")
              message("Check data structure!")
              #return(NA)                     #choose a return value in case of error
            },
            
            warning=function(war) {           #if warning occurs, repeat read.table with encoding UCS-2LE
              message(paste("Warning:  ",f,"  data encoding is not UTF-8."))
              ##activate orginal warning message if needed
              # message("Original warning message:")
              # message(war)
              # message("                    ")
              message("Successfully repeated read.table with encoding UCS-2LE. Nothing to worry.")  
              dat<-read.table(f, header=TRUE,sep="",dec=",",na.strings=c("","NA","*","-9999.00","-9999","NaN"),fileEncoding="UCS-2LE",fill=T,skipNul=T,blank.lines.skip=TRUE)
             # dat$Date <- format(as.Date(dat$Date, format = "%d.%m.%Y"), "%Y/%m/%d")
                #   dat$Date=as.Date(dat$Date,"%d.%m.%Y")
              # return(head(dat,n=2L))  #show first 2 lines of data
              # return(dat)
            }
          )
    
      dat <-  as_tibble(dat) %>%                           #convert to dataframe
        mutate(date=ymd(as.character(Date))) %>%  #Mutate adds new variables and preserves existing
        select(date,P_mm) %>%
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
  

DF #show grouped Pobs data
  

  
## 2) Check, if date column in DF is continuous & create continous time series ####
  
  checkfile=DF
  
  # OR read in your data, as created by step 1)  
  # checkfile=read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all_subbas.csv",
  #                      header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
  
  head(checkfile)
  
# Get start and end date of Pobs
  minPdate=min(checkfile$date)
  maxPdate=max(checkfile$date)
  minPdate
  maxPdate
  
# For comparison: create a continuous date time series from min. to max. date of Pobs and test its length
  date = seq(from = as.Date(minPdate), to = as.Date(maxPdate), by = 'day')
  # Or specify a certain length (e. g., like in climate data)
  #date = seq(from = as.Date("1950-01-01"), to = as.Date("2018-12-31"), by = 'day')
  
  d=NULL                    #create empty variable
  d$date = data.frame(date) #attach date
  d=data.frame(d)           #convert to datafram  
  #str(d) 
  #length(d)


# Create continous Pobs time series 
  
  #compare number of rows your data should have (d) with row numbers in your obs-data    
  if(nrow(d)>nrow(checkfile)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(checkfile),"missing entries.")) else message("Your Obs-data is continous.")
  
  #If discontinous, merge continuous date with obs-data and fill gaps with NA:
  obscon <-  as_tibble(checkfile) %>%         #convert to dataframe
    mutate(date=ymd(as.character(date)))      #convert "date" from character to date format
  obscon=merge(d, obscon, by="date", all=T)   #merge continous date with obs-data and fill gaps with NA
  obscon= data.frame(obscon)
  
  #compare number of rows your data should have (d) with row numbers in your obs-data (obscon)    
  if(nrow(d)>nrow(obscon)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(obscon),"missing entries.")) else message("Your Obs-data is continous.")
  
 obscon 
    
# Save continous Pobs data in 1 file with station names
    # Important: discharge=0 was set to NA!
    # Save with original station names
    write.table(obscon,file="_Pobscon_allstations.dat",row.names=F,sep=";",quote=F)

## 3) Create nice summary statistic table ####
    # Important: precipitation=0 was set to NA! in step 1)
    
    # DF2 = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_all-stations.txt",
    #   header = TRUE,  sep = ",", dec = ".",na.strings ="NA")
    #Data 01/01/1980-21/09/2016
    # DF2 = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_1980-2016_subbas.csv",
    # header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
    
    DF2=obscon # output of step 2), data format: 1 date column, rest station data columns
    str(DF2)
    summary(DF2)
    
    obs <- DF2[, -1]  #remove first date column
    #obs <- DF2[, -(1:4)]  #remove first 4 columns YYYY, MM, DD, HH
    str(obs)
    sumstat=NULL
    sumstat = do.call(cbind, lapply(obs, summary))
    sumstat = round(sumstat, digits=1)
    
    Fraction_NA = round(colMeans(is.na(obs)), digits=3)
    Percentage_NA = round(Fraction_NA*100, digits=2)
    
    sumstat = rbind(sumstat,Fraction_NA,Percentage_NA)
    #str(sumstat)
    
    #summary in transposed matrix (rows=stations, columns=statistics)
    sumat=as.matrix(sumstat)
    sumat=t(sumat)
    sumat            
    
#Save summary file with dynamic file name
    #setwd("D:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/MeteoHydroObs/Discharge/")
    getwd()
  #start date of Qobs cont data
    hDF2=head(DF2[,1],n=1) 
    min=hDF2
  #end date
    tDF2=tail(DF2[,1],n=1) 
    max=tDF2
    
  write.table(sumat,file=paste0("_sumstat_Pobscon_",min,"to",max,".dat"),sep=";",quote=F)
 
  
  plot(x=obscon$date,y=obscon$AbasAbad_21928)   
  plot(x=obscon$date,y=obscon$AbBarik21065)
  

  
## 4) Save data in format for "interpol" script "rainfall24_data.out" (P station interpolation to subbas cetroids) ####

  # Format of header/column names: "Y	M	D	A051	A137 ..."
    
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
    
# rename date columns
    library(plyr)
    dataobs=data.frame(dataobs)
    dataobs=rename(dataobs, c("year"="Y", "month"="M","day"="D")) # "Error: All arguments must be named" -> Solution: library(plyr) after library(dplyr)
  
# Rename column headers (station names) to short form 
    # Create a rename-file with 1 column - see step 0.3) - containing the following entries:
    # 1st entry "station name" (with quotation marks)
    # 2nd = (no quotation marks) 
    # 3rd short ID (with quotation marks, e.g. KarunDez: first letter & last 3 digits of station code) 
    # Example:
      # "Y"="Y"  # Caution: "Y"=Y" is needed to not rename the date columns
      # "M"="M"
      # "D"="D"
      # "AbasAbad_21928"="A928"
    
    
    # Read in the rename-file
    getwd()
    renamefile="_rename4interpol.dat"
    renamedf=read.table(renamefile, header = F,colClasses = "character", check.names=F,sep=c("=",",")) #data frame
    renamedf      #str(renamedf)
    # Create a rename vector
    library(plyr)
    as.matrix(renamedf)
    DFren=dataobs
    renamevect <- as.vector(renamedf$V2)
    names(renamevect)<- as.vector(renamedf$V1)
    renamevect
    # Rename data frame with all obs data from station name to subbas ID
    DFinterpol=rename(DFren, renamevect)
    DFinterpol
    #str(DFinterpol)

    # Replace "NA" by "-999"
    DFinterpol[is.na(DFinterpol)] <- -999
    DFinterpol
    
#Save as "rainfall24_data.out"
    #place to save file
    getwd()
    #setwd("...") #adjust path if needed
    
    write.table(DFinterpol,file="rainfall24_data.out", sep="\t",row.names=F,quote=F) 

    
    
## 5) Save Output of "interpol" script as WASA-SED Time_series Input "rain_daily.dat" ####
    
# 5.0) Reformat & select time period ####
    
        # Data need to be continous! (See step 2) 
    
    # Read in result "rainfall24.out"
    resint="e:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/Interpolation_P-stationdata/interpol_Till/_result4wasa/raw_rainfall24.out"
    res=read.table(resint, header=TRUE,sep="",dec=".",na.strings=c("","NA","*","-999.00","-9999.00","-9999","NaN"), fileEncoding="UTF-8",fill=T,skipNul=T,blank.lines.skip=TRUE)
    str(res)
    #rename columns
    names(res)= sub(pattern="_var01", replacement="", x=names(res))
    
    library(dplyr)
    
    # Create 1 date column
    res$date <- as.Date(paste(res$day,res$month,res$year, sep=".")  , format = "%d.%m.%Y" ) 
    
  # A) All data: reformat data - use res2 for whole time period in step 5.1)
    res2 <- as_tibble(res) %>%
      mutate(Date=strftime(date,"%d%m%Y"),index=row_number(date)) %>% #Mutate adds new var.&preserves existing: convert date format (to char string), add index row nr.
      select(-year,-month,-day,-date)  %>%
      select(Date,index,everything()) #move columns "Date" & "index" to front
    
      res2=data.frame(res2) #res2 contains ALL data of interpol, with row index
      str(res2)
      names(res2)= sub(pattern="stn", replacement="", x=names(res2))
      
  # B) Data subset for certain time period of interpol
      ressub <- as_tibble(res) %>%
        filter(date>="1980-01-01") %>% #select time period   #filter(date>="2015-09-04" & date<="2015-09-18")
        mutate(Date=strftime(date,"%d%m%Y"),index=row_number(date)) %>%
        select(-year,-month,-day,-date)  %>%
        select(Date,index,everything())
    
      ressub=data.frame(ressub)
      names(ressub)= sub(pattern="stn", replacement="", x=names(ressub))
    
 # 5.1)  Save as WASA-SED Time_series Input "rain_daily.dat"
      
# Chose result data
      # rawres=res2   #whole interpol timer period
      rawres=ressub   #time subset of interpol timer period
  
      # extract subbas data
      subx <- as_tibble(rawres) %>%
          select(-Date,-index) %>%
          mutate_if(is.numeric, round, 1) #round data to 1 decimal place
      
      subx=data.frame(subx)
  
# !! Caution: Set all NA to 0 (no NA excepted in WASA input; only do if ok with your data)    
      subx[is.na(subx)] <- 0
      subx    
      
  # sort subbasin columns, ascending order: sub 1 ... sub n  
      #rename subbasins to numbers only
      names(subx) = sub(pattern="X", replacement="", x=names(subx)) #in case subbas-name starts with X
      names(subx) = as.integer(names(subx)) #reformat names from "character" to "integer"
      #sort subbas columns by header name
      colorder=sort.int(as.integer(names(subx)), index.return = T)$ix
      subx=subx[,colorder]
  
  # select Date & index column
      datind=subset(rawres, select=c("Date","index")) 
      
  # # combine date & index columns and sorted subbas columns
  #     newres=cbind(datind,subx)
  #     ## check data head & structure
  #     head(newres)
  #     #str(newres)
      
# Save P data as WASA input
     
    setwd("E:/Anne/_SaWaM_Data_/2_KarunDez/ClimMeteoHydro-data/Interpolation_P-stationdata/interpol_Till/_result4wasa")
    address=getwd()
    try(system(paste0("mkdir ",address)))
    try(system(paste0("rm ",address,"/rain_daily.dat")))
    fileConn <- file(paste0(address,"/rain_daily.dat"),"a")
    cat("Daily total precipitation [mm] for each subasin, ordered according to Map-IDs","Date\t\tSubbasin-ID.", file = fileConn, sep = "\n")

    dfObj1=datind
    dfObj2=subx
    
    dfObj <- bind_cols(dfObj1,dfObj2)
    
    HEADER <- c("0","0",colnames(dfObj2))
    cat(HEADER, file = fileConn, sep = "\t")
    cat("\n",file = fileConn, sep = "")
    
    write.table(dfObj,file = fileConn, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE, fileEncoding = "UTF-8")
    close(fileConn)  
    
    
    
    
    
    