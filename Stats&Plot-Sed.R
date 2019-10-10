#
# Plot observed and modelled sediment 
#
#
# Copyright (C) 2019 Anne Müller
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

working_dir="D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED/"
thread_dir="2-1/"
outfolder="Output-f/"  #"Output/"

wasa_input_dir=paste(working_dir, thread_dir, "Input/Time_series/",sep="")
wasa_output_dir=paste(working_dir, thread_dir, outfolder,sep="")


#Run 1) Read Modelled River sediment load in ton/timestep - WITHOUT SNOW ROUTINE
modsedfile="River_Sediment_total.out" # Subbasnames mit X vor Zahl
modseddata = read.table(paste(wasa_output_dir, modsedfile, sep=""),header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))
#head(modseddata)
modsedraw = data.frame(modseddata) # raw data of observation, as dataframe
modsed <- modsedraw[, -(1:3)]      # leave out first 2 columns (Year, Simulation day, Time step)
names(modsed) = sub(pattern="^X", replacement="modRiverSedLoad_", x=names(modsed))  
modsed
nrow(modsed)
#mod$mod3

#Run 2) Read Modelled River sediment load in ton/timestep - WITH SNOW ROUTINE
wasa_output_dir2="D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED/2-2/Output-a/"
modsedfile2="River_Sediment_total.out" # Subbasnames mit X vor Zahl
modseddata2 = read.table(paste(wasa_output_dir2, modsedfile2, sep=""),header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))
modsedraw2 = data.frame(modseddata2) # raw data of observation, as dataframe
modsed2 <- modsedraw2[, -(1:3)]      # leave out first 2 columns (Year, Simulation day, Time step)
names(modsed2) = sub(pattern="^X", replacement="modsnowRiverSedLoad_", x=names(modsed2))  
modsed2
nrow(modsed2)


#Find start & end date of sed modelling
hsedmod=head(modsedraw[,1:2],n=1) #look at start date
tsedmod=tail(modsedraw[,1:2],n=1) #look at end date
hsedmod
tsedmod
#convert day of year to date
sedmodstart=as.Date(hsedmod$Day-1, origin = paste(hsedmod$Year,"-01-01",sep="")) # -1, because the function starts counting with 0
sedmodend=as.Date(tsedmod$Day-1, origin = paste(tsedmod$Year,"-01-01",sep="")) 
sedmodstart
sedmodend

#append date column to modsed
modsed$date = seq(from = as.Date(sedmodstart), to = as.Date(sedmodend), by = 'day') #str(modsed)
modsed2$date = seq(from = as.Date(sedmodstart), to = as.Date(sedmodend), by = 'day') #str(modsed)

##initialise mulitplot function
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   numPlots = length(plots)
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   if (numPlots==1) {
#     print(plots[[1]])
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }


#__________________________________________________________________________________________________
#### I) NA and summary statistics ####
#__________________________________________________________________________________________________

# To be done for observed sediment

###Summary statistics ####
# Modelled data
modsed  #without snow
modsed2 #with snow

moddata=modsed2#modsed
statmod=moddata[,-which(names(moddata) == "date")] #exclude "date" column

# Sort data in ascending order by subbas-ID
##rename subbasins to numbers only
names(statmod) = sub(pattern="modsnowRiverSedLoad_", replacement="", x=names(statmod)) # for modsed2
#names(statmod) = sub(pattern="modRiverSedLoad_", replacement="", x=names(statmod)) #for modsed

##reformat names from "character" to "integer" and sort subbas columns by header name
colorder=sort.int(as.integer(names(statmod)), index.return = T)$ix
statmod=statmod[,colorder]

sumstat = do.call(cbind, lapply(statmod, summary))
sumstat = round(sumstat, digits=3)

#Fraction_NA = round(colMeans(is.na(obs)), digits=3)
#Percentage_NA = round(Fraction_NA*100, digits=3)
#sumstat = rbind(sumstat,Fraction_NA,Percentage_NA)

# Reformat all to exponential/scientific number format (e=10^)
sumstat=format(sumstat, scientific=T) # 1e3 = 1000

#summary in transposed matrix (rows=stations, columns=statistics)
sumat=as.matrix(sumstat)
sumat=t(sumat)
sumat   

# Adjust file name, time period & save  
write.table(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED_Results/Sediment/SummaryStats_modsedsnow_2-2a_1980-01-01-2018-12-31.txt",quote=F,sep=";")

#write.table(sumat,file="D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED_Results/Sediment/SummaryStats_modsed_2-1f_1980-01-01-2018-12-31.txt",quote=F,sep=";")


#__________________________________________________________________________________________________
#### II) Visualisation ####
#__________________________________________________________________________________________________

### Timeseries mod & obs sediment ####

# To dynamically sample column by subbas-number
i=102       # adjust subbas-number

datain=modsed
subbas=i
var=paste0("modRiverSedLoad_",i,sep="")
df=datain[var]
ModRivSedL=as.numeric(df[,1]) #extract only values without column name

datain2=modsed2
subbas=i
var2=paste0("modsnowRiverSedLoad_",i,sep="")
df2=datain2[var2]
ModsnowRivSedL=as.numeric(df2[,1])

#Start and end date of sed modelling (see 0) Initialisation)
sedmodstart
sedmodend

#Example - get obs Sed data
    # Sed data strategic
    # KopfEZG
    # Str 31 K
    # Str 35 D
    # Str 36 D
    # Str 59 D
    # 
    # NichtKopfEZG ohne Dammeinfluss
    # Str 18 K
    # Str 20 K
    # Str 60 D
    # Str 81 K
    # Str 95 K
    # Str 102 K

seddatadir="D:/Anne/_SaWaM_Data_/2_KarunDez/Sediment/SedData/Sedtxt/"
obsedfile="KareBastBridge_21931.txt"
#obsedfile95="Armand_21231.txt"
#obsedfile81="Patave_21215.txt"
#obsedfile60="DoroodMarbareh_21279.txt"
#obsedfile20="Kata_21223.txt"
#obsedfile18="Moorez_21476.txt"
#obsedfile59="SepidDashtZaz_21287.txt"
#obsedfile36="DoKoohe_21453.txt"
#obsedfile35="ZoorAbad_21455.txt"
#obsedfile31="DashteBozorg_21441.txt"
#Caution! Sed data have to be tab-separated; else, NAs are introduced in wrong column
#Value 0 is set to NA
obseddata= read.table(paste(seddatadir, obsedfile, sep=""),header = T,skip = 0,na.strings = c("0","-9999.00","-9999","NA","NaN","Inf","-Inf","*"),
                       sep="\t",fill=T,skipNul=T,blank.lines.skip=TRUE)
#obsseddata[253,]

obsedraw = data.frame(obseddata) # raw data of observation, as dataframe
obsed = obsedraw     
names(obsed) = sub(pattern="Date", replacement="date", x=names(obsed))  #rename date column to match model output
names(obsed) = sub(pattern="SedLoad_ton.day", replacement="ObsRivSedL", x=names(obsed))
obsed
obsed$date=as.Date(obsed$date)
str(obsed)
library(scales)

xaxisend=as.Date("2019-12-31") #manually change the end of x-axis

s1=ggplot()+
  geom_line(data = datain2,aes(x=date, y=ModsnowRivSedL, group=1, colour="Mod snow")) +
  geom_line(data = datain,aes(x=date, y=ModRivSedL, group=1, colour="Mod")) +
  ggtitle(paste0("Subbas ",subbas, ", Modelled & observed river sediment load RivSedL [t/day],",sedmodstart," to ",sedmodend)) + 
  theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Year", y = "RivSedL [t/day]") +
  # Skalierung x-Achse & Beschriftung
  scale_x_date(breaks = seq(sedmodstart, xaxisend, by="1 year",limits = c(min, xaxisend)), date_labels="%Y",expand = c(0, 1)) +
  #scale_x_date(breaks = seq(sedmodstart,sedmodend, by="1 year"), date_labels="%Y") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 90,  hjust=0.5, vjust = 0.5)) +
  # Plot obs Sed
  #as line
  # geom_line(data=obsed,
  #           aes(x=obsed$date, y=obsed$ObsRivSedL, group=1), colour="black")
  #as points: shape= 1 o, 2 Triangle, 3 +, 4 x, 15 filled square, 16 filled dot, 
  geom_point(data=obsed, aes(x=obsed$date, y=obsed$ObsRivSedL, colour="Obs"),shape=3) + 
  scale_colour_manual("", 
                    #breaks = c("ModRivSedL", "ObsRivSedL"),
                    values = c("Mod"="orange", "Mod snow"="#642EFE","Obs"="black"),
                    guide = guide_legend(override.aes = list(
                      linetype = c("dashed","dashed", "blank"), #"solid"
                      #shape = c(NA,NA,16))))
                      shape = c(NA,NA,3))))
print(s1)

    #https://stackoverflow.com/questions/26587940/ggplot2-different-legend-symbols-for-points-and-lines

### Histogramm of obs sed ####
hist(diff(as.numeric(obsed$date))) #Abstand Messungen -> Histogramm wie oft Sed gemessen wurde

### Plot obsSedConc vs QobsSed ####
plot(obsed$ObsRivSedL/obsed$Discharge_m3.s,obsed$Discharge_m3.s) #plot zur Überprüfung ob lineare Beziehung zw. Q und Sed -> dann Interpolation / Sed-rating-curve mögl




### Monthly sums of mod sediment (x: Months, y: SedLoad, stacked curves by year)####
# To dynamically sample column by subbas-number
i=102       # adjust subbas-number

datain=modsed
subbas=i
var3=paste0("modRiverSedLoad_",i,sep="")
df=datain[var3] #select data of subbas i
str(df)

#OR For Mod Sed with Snow
# datain=modsed2
# subbas=i
# var4=paste0("modsnowRiverSedLoad_",i,sep="")
# df=datain[var4]


#Start and end date of sed modelling (see 0) Initialisation)
sedmodstart
sedmodend

#Example - get obs Sed data
# Sed data strategic
# KopfEZG
# Str 31 K
# Str 35 D
# Str 36 D
# Str 59 D
# 
# NichtKopfEZG ohne Dammeinfluss
# Str 18 K
# Str 20 K
# Str 60 D
# Str 81 K
# Str 95 K
# Str 102 K

library(scales)

xaxisend=as.Date("2019-12-31") #manually change the end of x-axis

#### Stacked time series curves ####
#----------------------------------#

df$Month <- as.numeric(format(as.Date(datain$date), "%m"))   # add column of month (01 to 12)
df$Year <- as.numeric(format(as.Date(datain$date), "%Y"))    # add column of year 
str(df)
#remove(df) #remove from R data

# Aggregate monthly/yearly sums, msum/ysum
msum=aggregate(data=df,df[,1] ~ Month + Year,sum) %>% mutate(Month=as.integer(Month)) # aggregate [1st column of df = ModSed] by "month", apply function "sum" 

# LINES plot, stacked, coloured (legend=different years) for each month (=x) -> Besser für Ganglinie im Jahresverlauf
ggplot(data=msum)+
  geom_point(aes(x=Month, y=msum[, 3],  colour=Year))+
  ggtitle(paste0("Subbas ",subbas, ", Monthly sums of daily modelled river sediment load RivSedL [t/day], ",var3,", ",sedmodstart," to ",sedmodend))+ theme(plot.title = element_text(hjust=0, size=10)) +
  labs(x = "Month", y = "RivSedL [t/month]") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  scale_x_discrete(limits=c(1:12))+
  scale_colour_gradientn(colours=rainbow(4))
#scale_fill_gradientn(colours=rainbow(4))  #scale_fill ... für kontinuierl. Daten



#Regenbogen für Jahre
# ggplot(data=newmsum_P_11)+geom_point(aes(x=Month, y=rain_11,  colour=Year))+
#   ggtitle("WASA-SED Input, Subbas 11, Precipitation P, Monthly sums of daily values, 01-01-1980 to 31-21-2013") + theme(plot.title = element_text(hjust=0, size=10)) +
#   labs(x = "Month", y = "P, monthly sums of daily values [mm]") +
#   scale_x_discrete(limits=c(1:12)) +
#   scale_colour_gradientn(colours=rainbow(4))  #scale_colour/scale_fill ... für diskrete/kontinuierl. Daten
