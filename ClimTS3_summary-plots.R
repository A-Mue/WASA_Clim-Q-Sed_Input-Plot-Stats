#
# Summarise and analyse climate time series
#
# (WASA-SED Input, Time_series)
# 
# Copyright (C) 2019 Anne M�ller 
#________________________________________________________________________________

library(readr)

working_dir="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/WASA-SED/"
thread_dir="1-1-4/"
wasa_input_dir=paste(working_dir, thread_dir, "Input/Time_series/",sep="")

T_file="temperature.dat"
P_file="rain_daily.dat"
Rad_file="radiation.dat"
Hum_file="humidity.dat"

#-----------------------------------------------------------------#
#### Temperature - Daily average temperature [degrees Celsius] ####
#-----------------------------------------------------------------#

T_rawdat = read.table(paste(wasa_input_dir, T_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
T_rawdat = data.frame(T_rawdat)  # "raw data"
T_dat <- T_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)

# Summary
# summary(T_dat)      # Summary for all observation data
# summary(T_dat$X30)  # Summary for single subbasin

# Summary statistics
T_sumstat = do.call(cbind, lapply(T_dat, summary))
T_sumstat = round(T_sumstat, digits=0)
row.names(T_sumstat)=c("T Min","T 1st Q","T Median","T Mean","T 3rd Q","T Max")
T_sumstat     # T in degrees Celsius, daily

# Output in file 
# write.csv(T_sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate_Time_series/SummaryStats_T.txt")


#------------------------------------------------------#
#### Precipitation - Daily total precipitation [mm] ####
#------------------------------------------------------#

P_rawdat = read.table(paste(wasa_input_dir, P_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
P_rawdat = data.frame(P_rawdat)  # "raw data"
P_dat <- P_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)

# Summary statistics
P_sumstat = do.call(cbind, lapply(P_dat, summary))
P_sumstat = round(P_sumstat, digits=0)
row.names(P_sumstat)=c("P Min","P 1st Q","P Median","P Mean","P 3rd Q","P Max")
P_sumstat     # P in mm, daily

# Output in file 
# write.csv(P_sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate_Time_series/SummaryStats_P.txt")


#--------------------------------------------------#
#### Radiation - Daily average radiation [W/m2] ####
#--------------------------------------------------#

Rad_rawdat = read.table(paste(wasa_input_dir, Rad_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
Rad_rawdat = data.frame(Rad_rawdat)  # "raw data"
Rad_dat <- Rad_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)

# Summary statistics
Rad_sumstat = do.call(cbind, lapply(Rad_dat, summary))
Rad_sumstat = round(Rad_sumstat, digits=0)
row.names(Rad_sumstat)=c("Rad Min","Rad 1st Q","Rad Median","Rad Mean","Rad 3rd Q","Rad Max")
Rad_sumstat


# Output in file 
write.csv(Rad_sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate&Fluvio_Time_series/SummaryStatistics_Time_series/SummaryStats_Rad_new.txt")

# selected subbas
Rad_selstat= Rad_sumstat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")] 
write.csv(Rad_selstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate&Fluvio_Time_series/SummaryStatistics_Time_series/SummaryStats_Rad_new_selsubbas.txt")

#---------------------------------------------#
#### Humidity - Daily average humidity [%] ####
#---------------------------------------------#

Hum_rawdat = read.table(paste(wasa_input_dir, Hum_file, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
Hum_rawdat = data.frame(Hum_rawdat)  # "raw data"
Hum_dat <- Hum_rawdat[, -(1:2)]      # leave out first 2 columns (Date in format DDMMYYYY, Day of simulation period)

# Summary statistics
Hum_sumstat = do.call(cbind, lapply(Hum_dat, summary))
Hum_sumstat = round(Hum_sumstat, digits=0)
row.names(Hum_sumstat)=c("Hum Min","Hum 1st Q","Hum Median","Hum Mean","Hum 3rd Q","Hum Max")
Hum_sumstat

# Output in file 
# write.csv(Hum_sumstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate_Time_series/SummaryStats_Hum.txt")


#----------------------------------------------------------#
#### Summary statistics of all climate time series data ####
#----------------------------------------------------------#

# For all subbasins (Note: rbind = bind by rows; cbin = bind by columns)
Clim_sumstat=rbind(T_sumstat,P_sumstat,Rad_sumstat,Hum_sumstat) 
Clim_sumstat                           

# For selected subbasins 
Clim_selstat= Clim_sumstat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")]                    
Clim_selstat

# Output in file 
write.csv(Clim_selstat,file="D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/Climate_Time_series/SummaryStats_ClimateSelectedSubbas.txt")


#---------------------#
#### Visualisation ####
#---------------------#

# Boxplots ####
#-------------#
# all data
T_box=boxplot(T_dat, notch = T, ylab="Daily average T [°C]", par(cex.lab=1), par(cex.axis=1), las=2)
P_box=boxplot(P_dat, notch = T, ylab="Daily total P [mm]", las=2)   # funktioniert nicht gut, da viele 0-Werte
Rad_box=boxplot(Rad_dat, notch = T, ylab="Daily average radiation [W/m²]", las=2)
Hum_box=boxplot(Hum_dat, notch = T, ylab="Daily average humidity [%]", las=2)

# subset of selected subbasins
T_sel=T_dat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")]
T_selbox=boxplot(T_sel,notch = T, ylab="Daily average T [°C]", las=2)

P_sel=P_dat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")]
P_selbox=boxplot(P_sel,notch = T, ylab="Daily total P [mm]", las=2)

Rad_sel=Rad_dat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")]
Rad_selbox=boxplot(Rad_sel,notch = T, ylab="Daily average radiation [W/m²]", las=2)

Hum_sel=Hum_dat[, c("X30","X31","X33","X35","X36","X37","X38","X49","X51","X57","X61","X63","X64","X65","X71","X75","X76","X78","X80","X81")]
Hum_selbox=boxplot(Hum_sel,notch = T, ylab="Daily average humidity [%]", las=2)


# Time series curves ####
#-----------------------#
# Precipitation

# idea: for 1 subbas, curves of monthly sums of precipitation, Jan-Dec for all years of climate data

require(ggplot2)
require(scales)

P_datedat <- P_rawdat[, -2]      # leave out 2nd column (Day of simulation period)
# P_subbas=cbind(P_datedat$X0, P_datedat$X11) # date and P time series for 1 subbas

subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81)

P_datain=P_datedat
  P_datain$datenum=as.POSIXct(ISOdate(P_datain[,1] %% 1e4, P_datain[,1] %/% 1e4 %% 1e2, P_datain[,1] %/% 1e6, hour=0, min = 0, sec = 0, tz = "GMT"), tz = "GMT")
  if (any(is.null(P_datain$datenum))) stop(paste0("Date conversion problem in ",obsfile))
  P_datain[,1]=NULL #delete obsolete columns 
  w <- options("warn")
  options(warn = -1) #disable warnings
  target_cols = as.numeric(sub(pattern="X", replacement="", x=names(P_datain))) %in% subbas_id
  options(w) # reset
  target_cols[ncol(P_datain)]=TRUE #datenum (last column) is needed anyway
  names(P_datain) = sub(pattern="^X", replacement="rain_", x=names(P_datain))
 
 # units = c(units, rep("mm", sum(target_cols))) #collect units for each column
  
subbas=11  # select subbas

rain = P_datain$rain_11
  
plot(P_datain$datenum, rain, type = "l", main = paste0(thread_dir,"; rain subbas_", subbas ))
  

# Monthly / yearly sums ####

# create variables of month, year (and week) of each observation:
P_datain$day <- as.Date(cut(P_datain$datenum, breaks = "day"))
P_datain$month <- as.Date(cut(P_datain$datenum, breaks = "month"))
P_datain$year <- as.Date(cut(P_datain$datenum, breaks = "year"))
#P_datain$week <- as.Date(cut(atain$datenum, breaks = "week", start.on.monday = FALSE)) # changes weekly break point to Sunday

# Aggregate monthly/yearly sums
monthsum_P_11=aggregate(data=P_datain,rain_11 ~ month,sum) # aggregate "rain" by "month", apply function "sum" 
monthsum_P_11

yearsum_P_11=aggregate(data=P_datain,rain_11 ~ year,sum)
yearsum_P_11


# time series graph by month:
ggplot(data = P_datain,
       aes(month, rain)) +
  ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Monthly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Year", y = "P, monthly sums of daily values [mm]") +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line", colour="steelblue") +  # or geom="bar", "point"
  scale_x_date(date_breaks='1 year', date_minor_breaks="3 months", date_labels ="%m-%Y") +
  theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5, size=6))


# bar graph by year:
ggplot(data =  yearsum_P_11,
       aes(x=yearsum_P_11$year, y=yearsum_P_11$rain_11)) +
  ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Yearly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Year", y = "P, yearly sums of daily values [mm]") +
  # Skalierung x-Achse & Beschriftung
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by="1 year"), 
               labels=date_format("%Y")) +  
  theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
  # Balken & Beschriftung
  geom_bar(stat="identity", fill="steelblue")+  
  geom_text(aes(label=yearsum_P_11$rain_11), angle = 90, vjust=0.3, hjust=1.5, size=4, colour="white") #vjust=horizontale Ausrichtung (re-li), hjust=vertikale Ausrichtung (h?her-tiefer)



# identify (fuer Abfrage/Beschriftung einzelner Werte in plot)


# this time series: 34 years
# c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
# "1990","1991","1992","1993", "1994","1995","1996","1997","1998","1999",
# "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
# "2010","2011","2012","2013")


#### Stacked time series curves ####
#----------------------------------#

library(ggplot2)

df=data.frame(P_datain$rain_11)
colnames(df)="rain_11"
df$Date=as.Date(cut(P_datain$datenum, breaks = "day"))
df$Month <- format(as.Date(P_datain$day), "%m")   # add column of month (01 to 12)
df$Year <- format(as.Date(P_datain$day), "%Y")    # add column of year 
df

#head(df)     #zeigt erste Zeilen eines Dataframes
#remove(df)   #l?scht die Variable aus "Data" in R

# Aggregate monthly/yearly sums, msum/ysum (for "monthsum"/"yearsum" are already specified above)
msum_P_11=aggregate(data=df,rain_11 ~ Month + Year,sum) %>% mutate(Month=as.integer(Month)) # aggregate "rain" by "month", apply function "sum" 
msum_P_11


# LINES plot, stacked, coloured (legend=different years) for each month (=x) -> Besser f?r Ganglinie im Jahresverlauf
ggplot(data=msum_P_11)+geom_line(aes(x=Month, y=rain_11,  colour=Year))+
      ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Monthly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
      labs(x = "Month", y = "P, monthly sums of daily values [mm]") +
      scale_x_discrete(limits=c(1:12))
      #scale_fill_gradientn(colours=rainbow(4))  #scale_fill ... f?r kontinuierl. Daten
  

# POINT plot (legend=different years) for each month (=x) -> Spannweite NS/Monat ?ber ges. Zeitraum
newmsum_P_11=msum_P_11 %>% mutate(Year=as.integer(Year))

#Blauschattierung f?r Jahre
ggplot(data=newmsum_P_11)+geom_point(aes(x=Month, y=rain_11,  colour=Year))+
  ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Monthly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Month", y = "P, monthly sums of daily values [mm]") +
  scale_x_discrete(limits=c(1:12))

#Regenbogen f?r Jahre
ggplot(data=newmsum_P_11)+geom_point(aes(x=Month, y=rain_11,  colour=Year))+
  ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Monthly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Month", y = "P, monthly sums of daily values [mm]") +
  scale_x_discrete(limits=c(1:12)) +
  scale_colour_gradientn(colours=rainbow(4))  #scale_colour/scale_fill ... f?r diskrete/kontinuierl. Daten


  