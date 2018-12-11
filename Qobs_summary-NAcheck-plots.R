#
# Analysis of observed river discharge data 
# ----------------------------------------------
# Content
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
# Anne Mueller
#________________________________________________________________________________

# 0) Initialisation

  library(readr)
  library(ggplot2)
  
  working_dir = "D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/WASA-SED/"
  thread_dir="1-1-4c/"
  wasa_input_dir=paste(working_dir, thread_dir, "Input/Time_series/",sep="")
  wasa_output_dir=paste(working_dir, thread_dir, "Output/",sep="")
  
  # Read observed river discharge
  obsfile="discharge_obs_24.txt"
  obsdata = read.table(paste(wasa_input_dir, obsfile, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=4)
  obsraw = data.frame(obsdata) # raw data of observation, as dataframe
  obs <- obsraw[, -(1:4)]      # leave out first 4 columns (YY MM DD HH)
  datain=obsraw
  names(datain) = sub(pattern="^X", replacement="obs_", x=names(datain))
  
  # Read precipitation data
  P_file="rain_daily.dat"
  P_rawdat = read.table(paste(wasa_input_dir, P_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
  P_rawdat = data.frame(P_rawdat)  # "raw data"
  P_dat <- P_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)
  names(P_dat) = sub(pattern="^X", replacement="rain_", x=names(P_dat))
  
  # for ggplot: add date column with as.Date
  date = seq(from = as.Date("1980-01-01"), to = as.Date("2013-12-31"), by = 'day')
  datain$date = data.frame(date)
  
  # Modelled river discharge
  modfile="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/WASA-SED/1-1-4c/Output/River_Flow.out" # Subbasnames mit X vor Zahl
  moddata = read.table(file=modfile,header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))
  #head(moddata)
  modraw = data.frame(moddata) # raw data of observation, as dataframe
  mod <- modraw[, -(1:2)]      # leave out first 2 columns (Year, Simulation day)
  #datain=obsraw
  names(mod) = sub(pattern="^X", replacement="mod_", x=names(mod))  
  mod

  
  
#--------------------------------------------------------------------------------
# I) NA and summary statistics
#--------------------------------------------------------------------------------

# Summary
# summary(obs)      # Summary for all observation data
# summary(obs$X30)  # Summary for single subbasin

# Summary statistics
#-------------------------
  obsraw = read.table("D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/Fluvio_timeseries_ANA.txt", header = TRUE,  sep = ",", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"))
  obsraw = data.frame(obsraw)
  obs <- obsraw[, -(1:2)]  
  
  sumstat = do.call(cbind, lapply(obs, summary))
  sumstat = round(sumstat, digits=3)
  
  Fraction_NA = round(colMeans(is.na(obs)), digits=3)
  Percentage_NA = round(Fraction_NA*100, digits=3)
  
  sumstat = rbind(sumstat,Fraction_NA,Percentage_NA)

# Adjust file name  
  write.csv(sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-2_ExtendedSFRB/AbflussSediment_ANA/alle/SummaryStats_Fluvio.txt")


# Find start and end date of existing data (no NAs)  
  obs1=data.frame(obsraw$date)
  obs1$VAZAO=obsraw$VAZAO_49705000 #manuell StationsIDs ändern
  obs1=na.omit(obs1)
  head(obs1)
  tail(obs1)
  

# (Number of NAs)
#-------------------------
#length(which(is.na(obsdata)))              # Number of all NAs - not very helpful
    length_30=length(which(is.na(obs$X30))) # Number of NAs of single subbasin
# Number of data in each row
    nrow=nrow(obs)
# Fraction of NA for Subbas 30
    percNA_30=length_30/nrow
# Fraction of NAs for all Subbas
    round(colMeans(is.na(obs)), digits=3) # Multiply by 100 for %




#--------------------------------------------------------------------------------
# II) Visualisation
#--------------------------------------------------------------------------------
  

# Time series plots of obs, mod discharge and P
#_________________________________________________

  # for simple plot: add date column with as.POSIXct) 
      # datain$datenum=as.POSIXct(ISOdate(datain$YYYY, datain$MM, datain$DD, datain$HH, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
      # #if (any(is.null(datain$datenum))) stop(paste0("Date conversion problem in ",obsfile))
      # w <- options("warn")
      # options(warn = -1) #disable warnings
      # target_cols = as.numeric(sub(pattern="X", replacement="", x=names(datain))) %in% subbas_id
      # options(w) # reset
      # target_cols[ncol(datain)]=TRUE #datenum (last column) is needed anyway


  #---------------------------------      
  # select subbasins
        #all subbas with obs data at outlet
            #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81) 
        # IDs Kopfeinzugsgebiete (Obs Data am Outlet)
            #37,38,49,51,57,65,75,76,80,81
        # IDs keine Kopf-EZG (Obs Data am Outlet) 
            #30,31,33,35,36,61,63,64,71,78
    subbas = 11
  # select river discharge obs data
    discharge_obs = datain$obs_11   # obs River flow
  #---------------------------------
  
    discharge_mod=mod$mod_11    # modelled River flow
    P_obs=P_dat$rain_11
  
  # create vector of NA dates for plotting
  NA_obs=which(is.na(discharge_obs))
    #NA_datenum=datain[NA_obs,]["datenum"]   # select dates, where obs has NA values, as.POSIX
  NA_date=datain[NA_obs,]["date"]   # as.Date 
  # ACHTUNG: in NA_datenum$datenum und NA_date$date stehen 2 Variablen in der Spalte datenum/date
  #head(NA_date)
  
# Simple plot (using as.POSIXct date)
    # plot(datain$datenum, discharge_obs, type = "l", col="blue", 
    #      xlab="Years", ylab="Observed river discharge [mm/day]",
    #      main = paste0(thread_dir,"; observed discharge subbas_", subbas ))
    # abline(v = c(NA_datenum$datenum), col="red")



# Plot obs & mod Riverflow plus obs Precipitation

    # Plot gesamter Zeitraum
    l=NA_date$date

    p1=ggplot()+
      ggtitle(paste0("Subbas ",subbas, ", Modelled (green) and observed (blue) river discharge Q [m³/s], 01-01-1980 to 31-12-2013 (red: NA in obs. discharge)")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year", y = "Q [m³/s]") +
      # Skalierung x-Achse & Beschriftung
      scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                   date_labels="%Y") +  
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      # NA kennzeichnen
      geom_vline(xintercept = l$date, colour="#FC9999")+  #l$date, um Datumsvektor zu isolieren  
      geom_line(data = datain,
                aes(x=date, y=discharge_mod, colour="discharge_mod")) +
      geom_line(data = datain,
                aes(x=date, y=discharge_obs, colour="discharge_obs")) +
      scale_colour_manual("", 
                          breaks = c("discharge_mod","discharge_obs"),
                          values = c("green","blue")) +
      theme(legend.position = c(0.85,0.9))
    
  
    p2=ggplot()+geom_line(data = datain,
                       aes(x=date, y=P_obs), colour="lightblue") +
      ggtitle(paste0("Subbas ",subbas, ", Observed Precipitation [mm], 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year", y = "P [mm]") +
      # Skalierung x-Achse & Beschriftung
      scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                   date_labels="%Y", position = "top") + 
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      scale_y_reverse()
  
     multiplot(p2, p1, cols=1)    # zum Initialisieren siehe unten
 
    
    
    # Plot für bestimmte Jahre
    l=NA_date$date
    p3=ggplot()+
      ggtitle(paste0("Subbas ",subbas, ", Modelled (green) and observed (blue) river discharge Q [m³/s], 01-01-2002 to 31-12-2003 (red: NA in obs. discharge)")) + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Year-Month", y = "Q [m³/s]") +
      # Skalierung x-Achse & Beschriftung
      scale_x_date(limits=c(as.Date("2002-01-01"), as.Date("2003-12-31")), #limits = breaks !
                   breaks = seq(as.Date("2002-01-01"), as.Date("2003-12-31"), by="month"), 
                   date_labels="%Y-%m") +   
      theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
      # NA kennzeichnen
      geom_vline(xintercept = l$date, colour="#FC9999")+  #l$date, um Datumsvektor zu isolieren  
      geom_line(data = datain,
                aes(x=date, y=discharge_mod, colour="discharge_mod")) +
      geom_line(data = datain,
                aes(x=date, y=discharge_obs, colour="discharge_obs")) +
      scale_colour_manual("", 
                          breaks = c("discharge_mod","discharge_obs"),
                          values = c("green","blue")) +
      theme(legend.position = c(0.85,0.9))
    
    
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
    
    
  
# Plot ONLY mod
    
    
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
      
      
      
      # Plot MOD für bestimmte Jahre
      
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
    
    
     
      
      
      
      # Kum. Summen discharge_mod gegen kum. Summen rain_obs   
      # ggplot(data = datain, aes(x=cumsum(P_obs), y=cumsum(discharge_mod))) + 
      #   geom_line() + 
      #   geom_point(colour="blue")+
      #   ggtitle(paste0("Subbas ",subbas, ", Cum. sums of modelled river discharge Q and obs. precip. P, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=8)) +
      #   labs(x = "Cum P [mm]", y = "Cum Q [m³/s]") +
      #   #coord_cartesian(ylim=c(0,12e+05)) +
      #   #scale_x_continuous(limits=c(0,18000), breaks=seq(0, 25000, 2000))+
      #   geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
      
      
      #Cum Summe mit Konst. Entnahme
      #500
      #1000
      
      
      dis_500=discharge_mod-500
      dis_1000=discharge_mod-1000
      
      
      
      
      # Kum. Summen discharge_mod gegen Zeit
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
      
      
      
      # Kum. Summen discharge_mod best. Jahr
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
    
    
#----------------------------------------------
# Ggplot of obs data & NA (using as.Date)
    # Load packages & data
    library(readr)
    library(ggplot2)
    obsdata = read.table("D:/Anne/_SaWaM_Data_/2_KarunDez/MeteoHydro-Station-data/HydrometricStations_Anne/obs-discharge/Q_daily_1980-2016_subbas.csv",
                    header = TRUE,  sep = ";", dec = ".",na.strings ="NA")
    obsraw = data.frame(obsdata) # raw data of observation, as dataframe
    # for ggplot, date column must be in format YYYY-MM-DD (if not, see above) and of type "Date"
    # str(datain)
    obsraw <- obsraw[, -(1)]     # remove date-column 
    datain=obsraw
    names(datain) = sub(pattern="sub", replacement="obs_", x=names(datain))

    # for ggplot: add date column with as.Date
    date = seq(from = as.Date("1980-01-01"), to = as.Date("2016-09-21"), by = 'day')
    #datain$date = data.frame(date) #verursachte Fehler "Column `x` must be a 1d atomic vector or a list"
    datain$date = date
    head(datain)
    
    # select river discharge obs data
    subbas = 115
    discharge_obs = datain$obs_115 # obs River flow

    # create vector of NA dates for plotting
    NA_obs=which(is.na(discharge_obs))
    #NA_datenum=datain[NA_obs,]["datenum"]   # select dates, where obs has NA values, as.POSIX
    NA_date=datain[NA_obs,]["date"]   # as.Date 
    #str(NA_date) #str(datain)
    
  # Plot gesamter Zeitraum
  l=NA_date$date    #head(l)
  ggplot()+geom_line(data = datain,
                     aes(x=date, y=discharge_obs), colour="blue") +
    ggtitle(paste0("Subbas ",subbas, ", Observed river discharge Q [m³/s], 1980-01-01 to 2016-09-21 (red: NA)")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Year", y = "Q [m³/s]") +
    # Skalierung x-Achse & Beschriftung
    scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2016-09-21"), by="1 year"), 
                 date_labels="%Y") +  
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
  
  
  #--------------------------------------------------------------------------------- 
    
       
    
# Kumulierte Summen
#___________________________
  
  # Plot kumulierte NS - Obs-summen -> Trendänderungen? 
  # Ziel: Auswahl Zeitraum --> KOPFEZG
      
      YYYY = datain$YYYY
      MM = datain$MM
      DD = datain$DD
      
  #-----------------------------------------------      
  # select subbasin for discharge and rain obs 
  #--------------------------------- -------------
  
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
  
  
# Kum. Summen discharge_obs gegen date
  ggplot(dis_rain, aes(x=date, y=cumsum(discharge_obs))) + 
    geom_line() + 
    geom_point(colour="blue")+
    ggtitle(paste0("Subbas ",subbas, ", Cumulated sums of obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Year", y = "Cum Q [m³]") +
    scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
                 date_labels="%Y") +  
    theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
    geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
  
  
# Kum. Summen discharge_obs gegen kum. Summen rain_obs   
  ggplot(dis_rain, aes(x=cumsum(rain_obs), y=cumsum(discharge_obs))) + 
    geom_line() + 
    geom_point(colour="blue")+
    ggtitle(paste0("Subbas ",subbas, ", Cumulated sums of obs. daily precipitation P and obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=10)) +
    labs(x = "Cum P [mm]", y = "Cum Q [m³]") +
    #scale_x_continuous(limits=c(0,18000), breaks=seq(0, 25000, 2000))+
    geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line
  

  
# Monthly/yearly sums
#___________________________

  #-----------------------------------------------      
  # select subbasin for discharge and rain obs 
  #--------------------------------- -------------
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


# Plot monthly sums
#------------------------------    

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
  
    
# Plot yearly sums
#------------------------------ 
    
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
