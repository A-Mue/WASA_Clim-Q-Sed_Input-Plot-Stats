#
# Analysis of observed river discharge data 
#

# Copyright (C) 2019 Anne Müller

#### Content ####

# 0) Initialisation                      (Lines)
# I) Summary and check for NA-values
# II) Visualisation                      (114)
      # - Time series plots              (118)
      # - Plot obs/mod with water extraction ()
      # - Plot obs Q und NAs             (431)
      # - Cumulated sums                 (466)
      # - Monthly/yearly sums            (520)
      # - Auto-plotting for many subbas  (570)

# Data required:
    # - WASA-SED Input, Time_series, discharge_obs_24.txt 
    # -                              rain_daily.dat            
    # - Time period
    # - Subbas IDs
#
#________________________________________________________________________________

#### 0) Initialisation ####
#___________________________________________

  library(dplyr)
  library(lubridate)
  library(readr)
  library(sf)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  
  working_dir="E:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED/"
  thread_dir="2-2/"
  wasa_input_dir=paste(working_dir, thread_dir, "Input/Time_series/",sep="")
  wasa_output_dir=paste(working_dir, thread_dir, "Output-a/",sep="")

# Read precipitation data used for modelling (same length as mod-data)
  P_file="rain_daily.dat"
  P_rawdat = read.table(paste(wasa_input_dir, P_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
  P_rawdat = data.frame(P_rawdat)  # "raw data"
  P_dat <- P_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)
  names(P_dat) = sub(pattern="^X", replacement="rain_", x=names(P_dat))
  P_dat
  
# Read modelled river discharge
  modfile="River_Flow.out" # Subbasnames mit X vor Zahl
  moddata = read.table(paste(wasa_output_dir, modfile, sep=""),header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))
  #head(moddata)
  modraw = data.frame(moddata) # raw data of observation, as dataframe
  mod <- modraw[, -(1:2)]      # leave out first 2 columns (Year, Simulation day)
  names(mod) = sub(pattern="^X", replacement="mod_", x=names(mod))  
  mod
  nrow(mod)
  #mod$mod3
  head(modraw[,1:2],n=1) #look at start date
  tail(modraw[,1:2],n=1) #look at end date
  
# Read observed river discharge (continous time series without missing date needed!)
  #obsraw=data.frame(dataobs)
  obsfile="discharge_obs_24.txt"
  obsdata = read.table(paste(wasa_input_dir, obsfile, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=4)
  obsraw = data.frame(obsdata) # raw data of observation, as dataframe
  tail(obsraw)
  names(obsraw) = sub(pattern="^X", replacement="obs_", x=names(obsraw))
  obsraw
  nrow(obsraw)
  nrow(P_dat)
  
# Fix discontinous observation data: create continous date, merge with obs-data & fill missing values with NA:
  
  #create artificial time series with length of mod discharge data 
  #find start & end of mod data
    hmod=head(modraw[,1:2],n=1) #look at start date
    tmod=tail(modraw[,1:2],n=1) #look at end date
    hmod
    tmod
    #convert day of year to date
    modstart=as.Date(hmod$day-1, origin = paste(hmod$year,"-01-01",sep="")) # -1, because the function starts counting with 0
    modend=as.Date(tmod$day-1, origin = paste(tmod$year,"-01-01",sep="")) 
    modstart
    modend
  
    # With length of P data: (not that useful, since WASA climate input can be longer than simulation period in do.dat)
    # head(P_rawdat[,1],n=1) #look at start date
    # tail(P_rawdat[,1],n=1) #look at end date
  
  #Date sequence from mod-start date to mod-end date
  date = seq(from = as.Date(modstart), to = as.Date(modend), by = 'day') #str(date)
  d=NULL                    #create empty variable
  d$date = data.frame(date) #attach date
  d=data.frame(d)           #convert to datafram
  
  #compare number of rows your data should have (d) with row numbers in your obs-data (obsraw)    
  if(nrow(d)>nrow(obsraw)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(obsraw),"missing entries.")) else message("Your Obs-data is continous.")
  #If continuous, you can still do this, because date-column is created:
  
      #merge continuous date with obs-data and fill gaps with NA:
      obscon <-  as_tibble(obsraw) %>%            #convert to dataframe
        unite(date, YYYY, MM, DD, sep="-") %>%    #combine 3 columns (YYYY,MM,DD) into 1 date (YYYY-MM-DD)
        mutate(date=ymd(as.character(date))) %>%  #convert "date" from character to date format
        select(-one_of("HH"))                     #drop column "HH"
      obscon=merge(d, obscon, by="date", all=T)   #merge continous date with obs-data and fill gaps with NA
      obscon= data.frame(obscon)
      
      #compare number of rows your data should have (d) with row numbers in your obs-data (obscon)    
      if(nrow(d)>nrow(obscon)) message(paste("Obs-data is discontinous and has",nrow(d)-nrow(obscon),"missing entries.")) else message("Your Obs-data is continous.")
  
  #compare number of rows of mod and obs-data   
  if(nrow(mod)!=nrow(obscon)) message(paste("Caution: No modelling data for whole obs-period, with",nrow(obscon)-nrow(mod),"missing mod-values.")) else message("Your Obs-data is continous.")
  
  # Save continous obs-file ####
  #write.table(obscon,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge_cont_1950-01-01-2018-11-30.txt",sep=";",row.names=F,quote=F)
  
  # Read continous obs-data from file ####
  #obscon = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge_cont_1950-01-01-2018-11-30.txt", header = TRUE,  sep = ";", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"))
  

#### I) NA and summary statistics ####
#___________________________________________

# Summary
# summary(obs)      # Summary for all observation data
# summary(obs$X30)  # Summary for single subbasin

#### Summary statistics #### 

# Read cont. obs-data from file (must be continous!)
  #obscon = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge_cont_1950-01-01-2018-11-30.txt", header = TRUE,  sep = ";", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"))
  
  
  obs=obscon[,-1] #take continous obs-data from step 0)
  sumstat = do.call(cbind, lapply(obs, summary))
  sumstat = round(sumstat, digits=3)
  
  Fraction_NA = round(colMeans(is.na(obs)), digits=3)
  Percentage_NA = round(Fraction_NA*100, digits=3)
  
  sumstat = rbind(sumstat,Fraction_NA,Percentage_NA)
  sumstat
  
  #summary in transposed matrix (rows=stations, columns=statistics)
  sumat=as.matrix(sumstat)
  sumat=t(sumat)
  sumat   
  
# Adjust file name, time period & save  
  #write.table(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/SummaryStats_obs-discharge_cont_1950-01-01-2018-11-30.txt",quote=F,sep=";")
  #write.table(sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/SummaryStats_Fluvio.txt",quote=F,sep=";")


# Sao Francisco: Find start and end date of existing data (no NAs)  
  # obs1=data.frame(obsraw$date)
  # obs1$VAZAO=obsraw$VAZAO_49705000 #manuell StationsIDs ändern
  # obs1=na.omit(obs1)
  # head(obs1)
  # tail(obs1)
  

#### (Number of NAs) ####

#length(which(is.na(obsdata)))              # Number of all NAs - not very helpful
    length_23=length(which(is.na(obs$obs_23))) # Number of NAs of single subbasin
# Number of data in each row
    nrow=nrow(obs)
# Fraction of NA for Subbas 23
    percNA_23=length_23/nrow
# Fraction of NAs for all Subbas
    round(colMeans(is.na(obs)), digits=3) # Multiply by 100 for %


#__________________________________________________________________________________________________
#### II) Visualisation ####
#__________________________________________________________________________________________________
  

####  Time series plots of obs, mod discharge and P #### 
#______________________________________________________

  # for simple plot: add date column with as.POSIXct) 
      # datain$datenum=as.POSIXct(ISOdate(datain$YYYY, datain$MM, datain$DD, datain$HH, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
      # #if (any(is.null(datain$datenum))) stop(paste0("Date conversion problem in ",obsfile))
      # w <- options("warn")
      # options(warn = -1) #disable warnings
      # target_cols = as.numeric(sub(pattern="X", replacement="", x=names(datain))) %in% subbas_id
      # options(w) # reset
      # target_cols[ncol(datain)]=TRUE #datenum (last column) is needed anyway
    
   
#### >data subset ####
        
#compare number of rows of mod and obs-data   
    if(nrow(mod)!=nrow(obscon)) message(paste("Caution: No modelling data for whole obs-period, with",nrow(obscon)-nrow(mod),"missing mod-values.")) else message("Your Obs-data is continous.")
    
#crop obs-data and P to length of mod-data
    #find start & end of mod data
    hmod=head(modraw[,1:2],n=1) #look at start date
    tmod=tail(modraw[,1:2],n=1) #look at end date
    hmod
    tmod
    #convert day of year to date
    modstart=as.Date(hmod$day-1, origin = paste(hmod$year,"-01-01",sep="")) # -1, because the function starts counting with 0
    modend=as.Date(tmod$day-1, origin = paste(tmod$year,"-01-01",sep="")) 
    modstart
    modend
    
    # P data subset
    hP=head(P_rawdat[,1],n=1) #look at start date
    tP=tail(P_rawdat[,1],n=1) #look at end date
    hP
    tP

    #convert day of year to date - doesn't work so far
    # Pstart=as.Date(hmod$day-1, origin = paste(hmod$year,"-01-01",sep="")) # -1, because the function starts counting with 0
    # Pend=as.Date(tmod$day-1, origin = paste(tmod$year,"-01-01",sep="")) 

    #!! Manually change according to head & tail
    P_rawdat$date=seq(from = as.Date("1950-01-01"), to = as.Date("2018-12-31"), by = 'day')
    #P_rawdat$date=seq(from = as.Date(Pstart), to = as.Date(Pend), by = 'day')

    #subset P with modstart/end
    Psubset <- P_rawdat[P_rawdat$date >= modstart & P_rawdat$date <= modend,] 
    #drop date columns for plotting
    Psubset=subset(Psubset, select = -date)
    Psubset=Psubset[,-(1:2)]
    names(Psubset) = sub(pattern="^X", replacement="rain_", x=names(Psubset))
    Psubset
    
    # obs data
    obscon$date=as.Date(obscon$date)
    str(obscon)
    obsubset <- obscon[obscon$date >= modstart & obscon$date <= modend,] 
    obsubset=obsubset[,-1]
    
    
#### >select subbasins  ####
    
    #UpperSF
    #all subbas with obs data at outlet
    #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81) 
    # IDs Kopfeinzugsgebiete (Obs Data am Outlet)
    #37,38,49,51,57,65,75,76,80,81
    # IDs keine Kopf-EZG (Obs Data am Outlet) 
    #30,31,33,35,36,61,63,64,71,78
    
    #KarunDez
    #KopfEZG
    #Karun 31 #Dez 35,36,59
    #NichtKopfEZG ohne Damm
    #Karun 18,20,81,95,102 #Dez 60
    #NichtKopfEZG MIT upstream Damm
    #Dez 40,41,55,58,61
     
# To dynamically sample column by subbas-number
    
    Pdatasource="NCEP"  #for title in figure; precipitation data source
    
    # adjust subbas-number i
    i=59   
    #subbas=95#61#58#55#41#40#60#102#95#81#20#18#59#36#35#31
    
    #KopfEZG & nichtStrat
    # 3 19 24 27 30 39 46 50 52 54 57 67 69
    #70 71 72 73 74 77 79 83 86 87 89 92 93 94 97
    #100 106 108 109 113 115 116
    
    #NichtKopf & nichtStrat
    #2 17 21 23 25 28
    #47 51 53 65 66 68 76 78 80 82 84
    #98 104 105 107 110 111 112 114
  
  # Create plot
      if=T{     
    subbas=i
    # select obs discharge
    var1=paste0("obs_",i,sep="") #head(datain[var]) #str(discharge_obs)
    df1=obsubset[var1] #or df1=obscon[var1] for whole timespan of P data
    discharge_obs=as.numeric(df1[,1]) #extract only values without column name
    #length(which(is.na(discharge_obs))) # Number of NAs of single subbasin
    
    # select modelled River flow
    var2=paste0("mod_",i,sep="")
    df2=mod[var2]
    discharge_mod=as.numeric(df2[,1])   
  
    # select precipitation
    var3=paste0("rain_",i,sep="")
    df3=Psubset[var3] #or df3=P_dat[var2] for whole timespan of P data
    P_obs=as.numeric(df3[,1])  
    
    date=seq(from = as.Date(modstart), to = as.Date(modend), by = 'day')
    datain=NULL
    datain=cbind(discharge_mod,discharge_obs,P_obs)
    datain=data.frame(datain)
    datain$date=seq(from = as.Date(modstart), to = as.Date(modend), by = 'day')
    #str(datain)
    
# create vector of NA dates for plotting
  NA_obs=which(is.na(datain$discharge_obs))
    #NA_datenum=datain[NA_obs,]["datenum"]   # select dates, where obs has NA values, as.POSIX
  NA_date=datain[NA_obs,]["date"]   # as.Date 
  # ACHTUNG: in NA_datenum$datenum und NA_date$date stehen 2 Variablen in der Spalte datenum/date
  #head(NA_date)
  #str(NA_date)
  
# Simple plot (using as.POSIXct date)
    # plot(datain$datenum, discharge_obs, type = "l", col="blue", 
    #      xlab="Years", ylab="Observed river discharge [mm/day]",
    #      main = paste0(thread_dir,"; observed discharge subbas_", subbas ))
    # abline(v = c(NA_datenum$datenum), col="red")

#### Plot OBS & MOD Riverflow and obs Precipitation ####

#### >>Plot gesamter Modell-Zeitraum ####
    
  l=NA_date$date
  
        # #find start & end of mod data
        # hmod=head(modraw[,1:2],n=1) #look at start date
        # tmod=tail(modraw[,1:2],n=1) #look at end date
        # hmod
        # tmod
        # #convert day of year to date
        # modstart=as.Date(hmod$day-1, origin = paste(hmod$year,"-01-01",sep="")) # -1, because the function starts counting with 0
        # modend=as.Date(tmod$day-1, origin = paste(tmod$year,"-01-01",sep="")) 
        # modstart
        # modend
    dstart=modstart #start date of mod-data
    dend=modend   #end date of mod-data
    dstart
    dend
    xaxisend=as.Date("2019-12-31") #manually change the end of x-axis
    
    p1=ggplot()+
      #ggtitle(paste0("Subbas ",subbas, ", ",dstart," to ",dend)) + 
      #ggtitle(paste0("Subbas ",subbas, ", Modelled (green) and observed (blue) river discharge Q [m³/s],",dstart," to ",dend," (red: NA in obs. discharge)")) + 
      theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year", y = "Q [m³/s]") +
    # Skalierung x-Achse & Beschriftung
      #scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2018-11-30"), by="1 year"), date_labels="%Y") +
      scale_x_date(breaks = seq(dstart, xaxisend, by="1 year",limits = c(min, xaxisend)), date_labels="%Y",expand = c(0, 1)) +
      #scale_x_date(breaks = seq(dstart, dend, by="1 year"), date_labels="%Y",expand = c(0, 0)) +
      
      #scale_x_date(breaks = seq(dstart, dend, by="1 day"), date_labels="%d") +
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
    # NA kennzeichnen
      geom_vline(aes(xintercept = l, colour="NA in Q_obs"),show.legend=F)+#colour="#FC9999")+
      geom_line(data = datain,aes(x=date, y=discharge_obs, colour="Q_obs")) +
      geom_line(data = datain,aes(x=date, y=discharge_mod, colour="Q_mod")) +
      scale_colour_manual("", 
                values = c("Q_mod"="green","Q_obs"="blue","NA in Q_obs"="#FC9999"),#)+#,
                guide = guide_legend(override.aes = list(
                size=c(5,1,1)))) #+ theme(legend.position = c(0.9,0.9))#c(0.85,0.9))
    #print(p1)
  
     p2=ggplot()+geom_line(data = datain,
                       aes(x=date, y=P_obs,colour="P_NCEP"),size=1) +
      ggtitle(paste0("Subbas ",subbas, ", ",dstart," to ",dend)) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "", y = "P [mm]") +
      #ggtitle(paste0("Subbas ",subbas, ", Observed precipitation P [mm],",dstart," to ",dend,", data source: ", Pdatasource)) + theme(plot.title = element_text(hjust=0, size=10)) +
      #labs(x = "Year", y = "P [mm]") +
      # Skalierung x-Achse & Beschriftung
      # scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
      #              date_labels="%Y", position = "top") + 
      scale_x_date(breaks = seq(dstart, xaxisend, by="1 year",limits = c(min, xaxisend)), date_labels="%Y",expand = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      scale_colour_manual("", breaks = "P_NCEP",values = "lightblue") +
      #theme(legend.position = c(0.9,0.9)) + #c(0.85,0.9)) 
      scale_y_reverse()
    #print(p2)
    
     #multiplot(p2, p1, cols=1)    # zum Initialisieren siehe unten
     #m=multiplot(p2, p1, cols=1)
  
     g=NULL
     ## convert plots to gtable objects
     library(gtable)
     library(grid) # low-level grid functions are required
     g1 <- ggplotGrob(p1)
     #g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
     g2 <- ggplotGrob(p2)
     g <- rbind(g2, g1, size="first") # stack the two plots
     g$widths <- unit.pmax(g2$widths, g1$widths) # use the largest widths
     # center the legend vertically
     #g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
     grid.newpage()
     grid.draw(g)    
      }
   
         
  # Save image
 
     ggsave("D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED_Results/multiplot-obs-mod-Pm.pdf", width = 20, height = 20, units = "cm")
     
     if=T{
     mypath <-file.path("D:","Anne","_SaWaM_Data_","2_KarunDez","WASA-SED_Results", paste("obs-mod-P_",subbas,".jpeg",sep=""))
     jpg(file=mypath)
     #mytitle = paste("obs-mod-P_", subbas, ".jpeg", sep="")
     multiplot(p2, p1, cols=1)
     dev.off()
     }
     
     # delete files with base::unlink()
     # unlink("mtcars.pdf")
     # unlink("mtcars.png")
    
    
#### >>Plot für bestimmte Jahre ####
    l=NA_date$date
    p3=ggplot()+
      ggtitle(paste0("Subbas ",subbas, ", Modelled (green) and observed (blue) river discharge Q [m³/s], 01-01-2002 to 31-12-2003 (red: NA in obs. discharge)")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year-Month", y = "Q [m³/s]") +
      # Skalierung x-Achse & Beschriftung
      scale_x_date(limits=c(as.Date("1980-01-01"), as.Date("2018-11-30")), #limits = breaks !
                   breaks = seq(as.Date("1980-01-01"), as.Date("2018-11-30"), by="month"), 
                   date_labels="%Y-%m") +   
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      # NA kennzeichnen
      geom_vline(xintercept = l, colour="#FC9999")+  #l$date, um Datumsvektor zu isolieren  
      #geom_vline(xintercept = l$date, colour="#FC9999")+  #l$date, um Datumsvektor zu isolieren  
      geom_line(data = datain,
                aes(x=date, y=discharge_mod, colour="discharge_mod")) +
      geom_line(data = datain,
                aes(x=date, y=discharge_obs, colour="discharge_obs")) +
      scale_colour_manual("", 
                          breaks = c("discharge_mod","discharge_obs"),
                          values = c("green","blue")) +
      theme(legend.position = c(0.85,0.9))
      print(p3)
    
    p4=ggplot()+geom_line(data = datain,
                          aes(x=date, y=P_obs), colour="lightblue") +
      ggtitle(paste0("Subbas ",subbas, ", Observed Precipitation [mm], 01-01-2002 to 31-12-2003")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year-Month", y = "P [mm]") +
      # Skalierung x-Achse & Beschriftung
      scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                   breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                   date_labels="%Y-%m", position = "top") +  
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      scale_y_reverse()
      
    multiplot(p4, p3, cols=1)
    
    
  
#### >Plot only MOD ####
    
    
      # Plot gesamter Zeitraum
      l=NA_date$date
      
      p5=ggplot()+
        ggtitle(paste0("Subbas ",subbas, ", Modelled river discharge Q [m³/s], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year", y = "Q [m³/s]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                     date_labels="%Y") +  
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        geom_line(data = datain,
                  aes(x=date, y=discharge_mod), colour="green") 

      
      
      p6=ggplot()+geom_line(data = datain,
                            aes(x=date, y=P_obs), colour="lightblue") +
        ggtitle(paste0("Subbas ",subbas, ", Observed Precipitation [mm], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year", y = "P [mm]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                     date_labels="%Y", position = "top") + 
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        scale_y_reverse()
      
      multiplot(p6, p5, cols=1)    # zum Initialisieren siehe unten
      
      
      
      #### >>Plot MOD für bestimmte Jahre ####
      
      l=NA_date$date
      p7=ggplot()+
        ggtitle(paste0("Subbas ",subbas, ", Modelled river discharge Q [m³/s], 01-01-2002 to 31-12-2003")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year-Month", y = "Q [m³/s]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                     breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                     date_labels="%Y-%m") +   
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        geom_line(data = datain,
                  aes(x=date, y=discharge_mod), colour="green") 
      
      
      p8=ggplot()+geom_line(data = datain,
                            aes(x=date, y=P_obs), colour="lightblue") +
        ggtitle(paste0("Subbas ",subbas, ", Observed Precipitation [mm], 01-01-2002 to 31-12-2003")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year-Month", y = "P [mm]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                     breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                     date_labels="%Y-%m", position = "top") +  
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        scale_y_reverse()
      
      multiplot(p8, p7, cols=1)
    
    
     
      
      
      
#### Cum Summen discharge_mod gegen kum. Summen rain_obs ####    
      # ggplot(data = datain, aes(x=cumsum(P_obs), y=cumsum(discharge_mod))) + 
      #   geom_line() + 
      #   geom_point(colour="blue")+
      #   ggtitle(paste0("Subbas ",subbas, ", Cum. sums of modelled river discharge Q and obs. precip. P, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=8)) +
      #   labs(x = "Cum P [mm]", y = "Cum Q [m³/s]") +
      #   #coord_cartesian(ylim=c(0,12e+05)) +
      #   #scale_x_continuous(limits=c(0,18000), breaks=seq(0, 25000, 2000))+
      #   geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
      
      
#### Cum Summe mit Konst. Entnahme ####
      #500
      #1000
      
      
      dis_500=discharge_mod-500
      dis_1000=discharge_mod-1000
      
      
#### Kum. Summen discharge_mod gegen Zeit ####
      ggplot()+
        ggtitle(paste0("Subbas ",subbas, ", Cum. sum of modelled river discharge Q [m³/s], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year", y = "Q [m³/s]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                     date_labels="%Y") + 
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(discharge_mod), colour="discharge_mod")) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(dis_500), colour="discharge_mod - 500 m³/s")) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(dis_1000), colour="discharge_mod - 1000 m³/s")) +
        scale_colour_manual("", 
                            breaks = c("discharge_mod","discharge_mod - 500 m³/s","discharge_mod - 1000 m³/s"),
                            values = c("green","orange","red")) +
        theme(legend.position = c(0.5,0.9))
      
      
      
#### Kum. Summen discharge_mod best. Jahr ####
      ggplot()+
        ggtitle(paste0("Subbas ",subbas, ", Cum. sum of modelled river discharge Q [m³/s], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year", y = "Q [m³/s]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                     breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                     date_labels="%Y-%m") +
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(discharge_mod)), colour="green")    
    
      ggplot()+
        ggtitle(paste0("Subbas ",subbas, ", Cum. sum of modelled river discharge Q [m³/s], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
        labs(x = "Year", y = "Q [m³/s]") +
        # Skalierung x-Achse & Beschriftung
        scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                     breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                     date_labels="%Y-%m") +
        theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(discharge_mod), colour="discharge_mod")) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(dis_500), colour="discharge_mod - 500 m³/s")) +
        geom_line(data = datain,
                  aes(x=date, y=cumsum(dis_1000), colour="discharge_mod - 1000 m³/s")) +
        scale_colour_manual("", 
                            breaks = c("discharge_mod","discharge_mod - 500 m³/s","discharge_mod - 1000 m³/s"),
                            values = c("green","orange","red")) +
        theme(legend.position = c(0.5,0.9))  

      
      
           
#_________________________    
    
    # Multiple plot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    #
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      numPlots = length(plots)
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      if (numPlots==1) {
        print(plots[[1]])
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
 # _______________________________
    
#### Plot OBS data & NA (using as.Date) ####
# Load packages & data
    library(readr)
    library(ggplot2)
    
# Read in data
    # Read continous obs-data from file ####
    obscon = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge_cont_1950-01-01-2018-11-30.txt", header = TRUE,  sep = ";", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"))
    obscon=data.frame(obscon)
    obscon$date=as.Date(obscon$date)
    datain = obscon 
        #OLD:
            # obsdata = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_1980-2016_subbas.csv",
            #                 header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
            # obsraw = data.frame(obsdata) # raw data of observation, as dataframe
            # # for ggplot, date column must be in format YYYY-MM-DD (if not, see above) and of type "Date"
            # # str(datain)
            # obsraw <- obsraw[, -(1)]     # remove date-column 
            # datain=obsraw
            # names(datain) = sub(pattern="sub", replacement="obs_", x=names(datain))
            # 
            # # for ggplot: add date column with as.Date
            # date = seq(from = as.Date("1980-01-01"), to = as.Date("2016-09-21"), by = 'day')
            # #datain$date = data.frame(date) #verursachte Fehler "Column `x` must be a 1d atomic vector or a list"
            # datain$date = date
            # head(datain)
    
    # select river discharge obs data
    dstart=as.Date(head(datain$date,n=1))
    dend=as.Date(tail(datain$date,n=1))
    
    # To dynamically sample column by subbas-number
    i=14       # adjust subbas-number
    
    subbas=i
    var=paste0("obs_",i,sep="")
    df=datain[var]
    discharge_obs=as.numeric(df[,1]) #extract only values without column name
 
    #subbas = 2
    #discharge_obs = datain$obs_2 # obs River flow
    
    # create vector of NA dates for plotting
    NA_obs=which(is.na(discharge_obs))
    #NA_datenum=datain[NA_obs,]["datenum"]   # select dates, where obs has NA values, as.POSIX
    NA_date=datain[NA_obs,]["date"]   # as.Date 
    #str(NA_date) #str(datain) #head(NA_date)
    
  # Plot gesamter Zeitraum
  l=as.Date(NA_date$date)    #head(l)

  ggplot()+geom_line(data = datain,
                     aes(x=date, y=discharge_obs, group=1), colour="blue") +
    ggtitle(paste0("Subbas ",subbas, ", Observed river discharge Q [m³/s],",dstart," to ",dend," (red: NA)")) + 
    theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Year", y = "Q [m³/s]") +
    # Skalierung x-Achse & Beschriftung
    scale_x_date(breaks = seq(dstart,dend, by="1 year"), date_labels="%Y") +
    #scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2016-09-21"), by="1 year"), date_labels="%Y") +  
    theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
    # NA kennzeichnen
    geom_vline(xintercept = l, colour="red")  #l$date, um Datumsvektor zu isolieren
  #ggsave("/tmp/plt.png", width = 16, height = 9, dpi = 120)
  
  # Plot für bestimmte Jahre
  l=NA_date$date
  ggplot()+geom_line(data = datain,
                     aes(x=date, y=discharge_obs), colour="blue") +
    ggtitle(paste0("Subbas ",subbas, ", Observed river discharge Q [m³/s], 01-01-1980 to 31-12-2013 (red: NA)")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Year", y = "Q [m³/s]") +
    # Skalierung x-Achse & Beschriftung
    scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                 breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="3 months"), 
                 date_labels="%Y-%m") +  
    theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
    # NA kennzeichnen
    geom_vline(xintercept = l$date, colour="red")  #l$date, um Datumsvektor zu isolieren
  
  
  #__________________________________________________________________
    
       
    
#### Kumulierte Summen ####
#___________________________
  
  # Plot kumulierte NS - Obs-summen -> Trendänderungen? 
  # Ziel: Auswahl Zeitraum --> KOPFEZG
      
      YYYY = datain$YYYY
      MM = datain$MM
      DD = datain$DD
      
  #______________________________________________     
  # select subbasin for discharge and rain obs 
  #______________________________________________ 
  
  #subbas =   #Kopf-EZG 37,38,49,51,57,65,75,76,80,81
              #keine Kopf-EZG 30,31,33,35,36,61,63,64,71,78
  #discharge_obs = datain$obs_76
  rain_obs = P_dat$rain_80
  
  discharge_obs =round(discharge_obs, digits = 0)
  
  
  dis_rain=cbind(YYYY,MM,DD,discharge_obs, rain_obs, datain$date)
  dis_rain=data.frame(dis_rain)
  #tail(dis_rain) 
  #length(dis_rain$discharge_obs)
  
  # eliminate rows of the data frame "dis_rain", where column "discharge_obs" has NA-values (see: https://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame)
  dis_rain=dis_rain[complete.cases(dis_rain), ]
  
  
#### >Kum. Summen discharge_obs gegen date ####
  ggplot(dis_rain, aes(x=date, y=cumsum(discharge_obs))) + 
    geom_line() + 
    geom_point(colour="blue")+
    ggtitle(paste0("Subbas ",subbas, ", Cumulated sums of obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Year", y = "Cum Q [m³]") +
    scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                 date_labels="%Y") +  
    theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
    geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
  
  
#### >Kum. Summen discharge_obs gegen kum. Summen rain_obs ####   
  ggplot(dis_rain, aes(x=cumsum(rain_obs), y=cumsum(discharge_obs))) + 
    geom_line() + 
    geom_point(colour="blue")+
    ggtitle(paste0("Subbas ",subbas, ", Cumulated sums of obs. daily precipitation P and obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Cum P [mm]", y = "Cum Q [m³]") +
    #scale_x_continuous(limits=c(0,18000), breaks=seq(0, 25000, 2000))+
    geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
  

  
#### Monthly/yearly sums ####
#___________________________

  #_______________________________________________     
  # select subbasin for discharge and rain obs 
  #_______________________________________________ 
  subbas = 81#37,38,49,51,57,65,75,76,80,
  discharge_obs = datain$obs_81
  rain_obs = P_dat$rain_81

    
    discharge_obs =round(discharge_obs, digits = 0)
    YYYY = datain$YYYY
    MM = datain$MM
    DD = datain$DD
    
    dis_rain=cbind(YYYY,MM,DD,discharge_obs, rain_obs)
    dis_rain=data.frame(dis_rain)
    str(dis_rain)
    
# For both discharge and rain, aggregate monthly/yearly sums
    monthsum=aggregate(data=datain,cbind(discharge_obs, rain_obs) ~ MM+YYYY,sum) # aggregate "Act_ETP" by "month", apply function "sum" 
    monthsum

    yearsum=aggregate(data=datain,cbind(discharge_obs, rain_obs) ~ YYYY,sum)
    yearsum
  
  # For discharge, aggregate monthly/yearly sums
    # yearsum_dis=aggregate(data=datain,discharge_obs ~ YYYY,sum)
    # yearsum_dis 


#### >Plot monthly sums ####
#____________________________   

    ggplot()+ geom_point(data = monthsum,
              aes(x=rain_obs, y=discharge_obs, colour=YYYY)) +
      ggtitle(paste0("Subbas ",subbas, ", Monthly sums of obs. daily precipitation P and obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "P [mm/year]", y = "Q [mm/year]") + 
      scale_colour_gradient() + 
      # Skalierung x-Achse
      scale_x_continuous(limits=c(-20,400), breaks=seq(0, 400, 100)) + # breaks:ticks from 0-400, every 100
      scale_y_continuous(limits=c(0,2500), breaks=seq(0,2500, 500))+ 
      #geom_text(data = monthsum, aes(x=rain_obs, y=discharge_obs, label=MM),  size=1.8, angle=90, hjust=0, nudge_y =100 )+ 
      labs(colour="Year")
  
    
#### >Plot yearly sums ####
#__________________________  
    
  ggplot()+geom_point(data = yearsum,
                     aes(x=rain_obs, y=discharge_obs, colour=YYYY)) +
    ggtitle(paste0("Subbas ",subbas, ", Yearly sums of obs. daily precipitation P and obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "P [mm/year]", y = "Q [mm/year]")+ 
    # Skalierung x-Achse
    scale_x_continuous(limits=c(500,2500), breaks=seq(500, 2500, 250)) + # breaks:ticks from 500-2500, every 250
    scale_y_continuous(limits=c(2000,12500), breaks=seq(2000,12500, 500)) + 
    geom_text(data = yearsum, aes(x=rain_obs, y=discharge_obs, label=YYYY),  size=1.8, angle=90, hjust=0, nudge_y =100 ) +
    labs(colour="Year")
    
    # , hjust="inward", vjust=0, nudge_x =0, nudge_y =0 
    #hjust: hoch-runter, vjust: re-li,
    #  nudge_y: y-parallel hoch(positiv)-runter(negativ)
    
    
    
# Auto-plotting for many subbasins
#____________________________________    
