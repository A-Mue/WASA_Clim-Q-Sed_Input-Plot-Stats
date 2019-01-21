#
# Calculation of runoff coefficients (rc) for   I) certain time periods 
#                                              II) non headwater subbas (Nicht-KopfEZG)
# ------------------------------------------------------------------------------------------

# Content
# 0) Initialisation                                      (Lines)
# I) Calculate rc for certain time span                    (54)
# II) rc for non headwater catchments                     (177)
# - Visualisation cumulated sums                          (196)

# Data required:
# - WASA-SED Input, Time_series, discharge_obs_24.txt 
# -                              rain_daily.dat            
#
# - Time period
# - Subbas IDs (subbas delivering water to non-headwater subbas -> from routing.dat)
#
# Anne Mueller
#________________________________________________________________________________

# 0) Initialisation

library(readr)
library(playwith)
library(ggplot2)

run_dir = "D:/Anne/_SaWaM_Data_/1_SaoFrancisco/1-1_UpperSFRB/WASA-SED/"
thread_dir="1-1-4c/"
wasa_input_dir=paste(run_dir, thread_dir, "Input/Time_series/",sep="")

# Read observed river discharge
obsfile="discharge_obs_24.txt"
obsdata = read.table(paste(wasa_input_dir, obsfile, sep=""), header = TRUE,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=4)
obsraw = data.frame(obsdata) # raw data of observation, as dataframe
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
str(datain)

# To find time periods for rc calculation, see script "Summary_NA-check_discharge_obs_24.R --> II) Visualisation


#--------------------------------------------------------------------------------
# I) Calculate rc for certain time span
#--------------------------------------------------------------------------------

# Select subbasin
    #all subbas with obs data at outlet
        #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81) 
    # IDs Kopfeinzugsgebiete (Obs Data am Outlet)
        #37,38,49,51,57,65,75,76,80,81
    # IDs keine Kopf-EZG (Obs Data am Outlet) 
        #30,31,33,35,36,61,63,64,71,78
subbas = 81
# Select river discharge obs & rain data
discharge_obs = datain$obs_81
rain_obs = P_dat$rain_81

#---------------------------------

if(T)   #auf T setzen um gesamten Block für dieses Subbas auszuführen
{
dis_rain=cbind(discharge_obs, rain_obs, datain$date)
dis_rain=data.frame(dis_rain)

# Eliminate rows of "dis_rain", where column "discharge_obs" has NA-values (see: https://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame)
  dis_rain=dis_rain[complete.cases(dis_rain), ]

# Convert unit of discharge_obs from m³/s to mm/d
  #From "explore_wasa_results.R"
      setwd("D:/Anne/Wasa_Visualisierung")  
      source(paste0("read_wasa_func.R"))             # use this function from WASA Visualisation
      #source("D:/Anne/Wasa_Visualisierung/read_wasa_func.R")    
      ctrl_params = parse_wasa_ctrl_params(wasa_param_file=paste0(run_dir, thread_dir, "Output/parameter.out"))
      #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81)                                   
      subbas_id=subbas                                     
  
      res = read_wasa_results(ctrl_params,components_list=c(
        #"River_Flow"     
        "daily_water_subbasin"
      ), subbas_id=subbas_id) 
      
      #read observations
      obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), # an dieser Stelle bleibt "River_Flow, auch wenn "daily water subbas" ausgewählt
                              wasa_input_dir=ctrl_params$input_dir)
      t_res = as.numeric(difftime(res[[1]][2], res[[1]][1], units="sec")) #temporal resolution in seconds
      data_cols = dimnames(res[[2]])[[2]] #names of WASA result columns
      
      #compute runoff coefficients
      mod_df=data.frame(datenum=res$datevec, res$result_array) #convert to dataframe                                     
      obs_mod = merge(mod_df, obs) #merge observations and model results
      area = attr(res[[2]], "subbas_area") #subbasin area
      
dis_rain$discharge_obs= dis_rain$discharge_obs * t_res / (area*1e6) * 1e3  #convert discharge from m³/s to mm
  sum(dis_rain$discharge_obs)
  
# Collect data for certain time span
  timefunc <- function(x,y){dis_rain[dis_rain$date >= x & dis_rain$date <= y,]}
}
  
# 1 Time span
#--------------------------------
  start1 <- as.Date("2000-01-01")    # start date (chosen for subbas 38)
  end1 <- as.Date("2006-12-31")      # end date
  tspan1 <- timefunc(start1,end1)    # new data frame for certain time span 1
  sumdis1=sum(tspan1$discharge_obs)
  sumrain1=sum(tspan1$rain_obs)
  rc1=sumdis1/sumrain1
  rc1
  
  
  
# 3 Time spans
#--------------------------------
# Time span 1
  start1 <- as.Date("1980-01-01")    # start date (chosen for subbas 38)
  end1 <- as.Date("1998-12-31")      # end date
  tspan1 <- timefunc(start1,end1)    # new data frame for certain time span 1
  sumdis1=sum(tspan1$discharge_obs)
  sumrain1=sum(tspan1$rain_obs)
  rc1=sumdis1/sumrain1
  #str(tspan1)
    
# Time span 2
  start2 <- as.Date("1999-01-01")
  end2 <- as.Date("2008-06-30")
  tspan2 <- timefunc(start2,end2)
  sumdis2=sum(tspan2$discharge_obs)
  sumrain2=sum(tspan2$rain_obs)
  rc2=sumdis2/sumrain2
  
# Time span 3
  start3 <- as.Date("2008-07-01")
  end3 <- as.Date("2013-12-31")
  tspan3 <- timefunc(start3,end3)
  sumdis3=sum(tspan3$discharge_obs)
  sumrain3=sum(tspan3$rain_obs)
  rc3=sumdis3/sumrain3

tspansummary=data.frame(Time_span=c(1:3),
                        Start_date=c(start1,start2,start3),
                        End_date=c(end1,end2,end3),
                        Sum_disobs_mm=round(c(sumdis1,sumdis2,sumdis3),digits = 0),
                        Sum_rain_mm=c(sumrain1,sumrain2,sumrain3),
                        rc=round(c(rc1, rc2, rc3),digits = 3))  
tspansummary



# 2 Time spans
#--------------------------------
# Time span 1
start1 <- as.Date("1980-01-01")    # start date
end1 <- as.Date("2007-12-31")      # end date
tspan1 <- timefunc(start1,end1)    # new data frame for certain time span 1
sumdis1=sum(tspan1$discharge_obs)
sumrain1=sum(tspan1$rain_obs)
rc1=sumdis1/sumrain1
#str(tspan1)

# Time span 2
start2 <- as.Date("2008-01-01")
end2 <- as.Date("2013-12-31")
tspan2 <- timefunc(start2,end2)
sumdis2=sum(tspan2$discharge_obs)
sumrain2=sum(tspan2$rain_obs)
rc2=sumdis2/sumrain2

tspansummary=data.frame(Time_span=c(1:2),
                        Start_date=c(start1,start2),
                        End_date=c(end1,end2),
                        Sum_disobs_mm=round(c(sumdis1,sumdis2),digits = 0),
                        Sum_rain_mm=c(sumrain1,sumrain2),
                        rc=round(c(rc1, rc2),digits = 3))  
tspansummary



#--------------------------------------------------------------------------------
# II) Calculate rc for non-headwater subbas
#--------------------------------------------------------------------------------

# 1
#--------------
  # complete time period, sum of P (no NA in discharge_obs of non-headwater subbas)
  # rain_obs = P_dat$rain_71  # change number manually
  # sum(rain_obs)

# 2
#--------------
  # if non-headwater subbas has NA in discharge_obs:
  # only P sum of upstream subbas, where discharge_obs of non-headwater subbas has data 
  
  #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81)

#2 subbas
  #subbas_id=c(31,32)
#3 subbas
  #subbas_id=c(30,44,45) # same order as in P_sel !! 
                        # 1. subbas-ID = non-headwater catchment (accordingly, rain_1 and discharge_obs)

  subbas_id=c(71,74,75,76,77,78,79,80,81,82,83,84,85)
  subbas=71 # ID of non-headwater subbas
  
  discharge_obs = datain$obs_71 # discharge obs of non-headwater subbas
  
  #P_dat # all P data
  # P_sel=cbind(rain_1=P_dat$rain_64,          
  #               rain_2=P_dat$rain_65)
   
  # P_sel=cbind(rain_1=P_dat$rain_61,          # select P for non-headwater subbas & upstream subbas
  #               rain_2=P_dat$rain_67,
  #               rain_3=P_dat$rain_68)
 
  P_sel=cbind(rain_1=P_dat$rain_71,
              rain_2=P_dat$rain_74,
              rain_3=P_dat$rain_75,
              rain_4=P_dat$rain_76,
              rain_5=P_dat$rain_77,
              rain_6=P_dat$rain_78,
              rain_7=P_dat$rain_79,
              rain_8=P_dat$rain_80,
              rain_9=P_dat$rain_81,
              rain_10=P_dat$rain_82,
              rain_11=P_dat$rain_83,
              rain_12=P_dat$rain_84,
              rain_13=P_dat$rain_85)

  
      sum(discharge_obs, na.rm = T) # sum of obs discharge, exclude NA
      #1739675
  
  # select river discharge obs
  

  # create vector with subbas areas

      # Convert unit of discharge_obs from m³/s to mm/d
      #From "explore_wasa_results.R"
      setwd("D:/Anne/Wasa_Visualisierung")  
      source(paste0("read_wasa_func.R"))             # use this function from WASA Visualisation
      #source("D:/Anne/Wasa_Visualisierung/read_wasa_func.R")    
      ctrl_params = parse_wasa_ctrl_params(wasa_param_file=paste0(run_dir, thread_dir, "Output/parameter.out"))
      #subbas_id=c(30,31,33,35,36,37,38,49,51,57,61,63,64,65,71,75,76,78,80,81)                                   
      subbas_id=subbas_id                                     
      
      res = read_wasa_results(ctrl_params,components_list=c(
        #"River_Flow"
        "daily_water_subbasin"
      ), subbas_id=subbas_id)
      
      #read observations
      obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), # an dieser Stelle bleibt "River_Flow, auch wenn "daily water subbas" ausgewählt
                              wasa_input_dir=ctrl_params$input_dir)
      t_res = as.numeric(difftime(res[[1]][2], res[[1]][1], units="sec")) #temporal resolution in seconds
      data_cols = dimnames(res[[2]])[[2]] #names of WASA result columns
      
      #compute runoff coefficients
      mod_df=data.frame(datenum=res$datevec, res$result_array) #convert to dataframe                                     
      obs_mod = merge(mod_df, obs) #merge observations and model results
      area = attr(res[[2]], "subbas_area") #subbasin area
  
      area     # Vektor mit Flächen
      area[1]  # 1. Eintrag des Vektors
      area[2]
      area[3]
      
  # convert P from mm to m³/s, including subbas area
      # dis_rain$discharge_obs= dis_rain$discharge_obs * t_res / (area*1e6) * 1e3  #convert discharge from m³/s to mm
      # X[mm/d]   = X[m³/s] * 86400[sec] / (area*1e6) * 1e3                        #t_res=86400 for daily time steps
      # X[m³/s] = X[mm/d]  / 86400 * (area*1e6) / 1e3      
      
  a1=((area[1])*1e6) /86400 / 1e3    # factor for conversion
  a2=((area[2])*1e6) /86400 / 1e3 
  a3=((area[3])*1e6) /86400 / 1e3 
  a4=((area[4])*1e6) /86400 / 1e3 
  a5=((area[5])*1e6) /86400 / 1e3 
  a6=((area[6])*1e6) /86400 / 1e3 
  a7=((area[7])*1e6) /86400 / 1e3
  a8=((area[8])*1e6) /86400 / 1e3
  a9=((area[9])*1e6) /86400 / 1e3
  a10=((area[10])*1e6) /86400 / 1e3
  a11=((area[11])*1e6) /86400 / 1e3    
  a12=((area[12])*1e6) /86400 / 1e3 
  a13=((area[13])*1e6) /86400 / 1e3 
  
  
  
  # P_con [m³/s] converted from mm/d
        P_sel=as.matrix(P_sel)
     
    #P_con=t(t(P_sel) * c(a1,a2))
    #P_con=t(t(P_sel) * c(a1,a2,a3))
    #P_con=t(t(P_sel) * c(a1,a2,a3,a4,a5))
  P_con=t(t(P_sel) * c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13))
      
        P_con=data.frame(P_con)
        P_con=round(P_con, digits = 0)


  # Combine discharge and rain data
        # YYYY = datain$YYYY
        # MM = datain$MM
        # DD = datain$DD
        
        discharge_obs =round(discharge_obs, digits = 0)
      
        #dis_rain=cbind(YYYY,MM,DD,datain$date,discharge_obs,P_con)
        dis_rain=cbind(datain$date,discharge_obs,P_con)
      
  #dis_rain$rain_cum=dis_rain$rain_1 + dis_rain$rain_2
  #dis_rain$rain_cum=dis_rain$rain_1 + dis_rain$rain_2 + dis_rain$rain_3  # add column with cumulated P of all selected subbas
  dis_rain$rain_cum=dis_rain$rain_1 + dis_rain$rain_2 + dis_rain$rain_3 +  dis_rain$rain_4 + dis_rain$rain_5 + dis_rain$rain_6   + dis_rain$rain_7 + dis_rain$rain_8 + dis_rain$rain_9 + dis_rain$rain_10 + dis_rain$rain_11 + dis_rain$rain_12 + dis_rain$rain_13
        
        dis_rain=data.frame(dis_rain)
        dis_rain
        #str(dis_rain)

  # Eliminate rows of "dis_rain", where column "discharge_obs" has NA-values (see: https://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame)
        dis_rain=dis_rain[complete.cases(dis_rain), ]
        rain_obs = dis_rain$rain_cum

    
# Kum. Summen discharge_obs gegen kum. Summen rain_obs   
    ggplot(dis_rain, aes(x=cumsum(rain_cum), y=cumsum(discharge_obs))) + 
      geom_line() + 
      geom_point(colour="blue")+
      ggtitle(paste0("Non-headw. subbas ",subbas, ", Cum. sums of obs. daily precip. P (incl. upstream subbas) and obs. daily river discharge Q, 01-01-1980 to 31-12-2013")) + theme(plot.title = element_text(hjust=0, size=8)) +
      labs(x = "Cum P [m³/s]", y = "Cum Q [m³/s]") +
      #coord_cartesian(ylim=c(0,12e+05)) +
      #scale_x_continuous(limits=c(0,18000), breaks=seq(0, 25000, 2000))+
      geom_smooth(method = "lm", colour="darkred")  #adds linearly smoothed line


start1 <- as.Date("1980-01-01")    # start date
end1 <- as.Date("2013-12-31")      # end date

      P=sum(dis_rain$rain_cum)
      d=sum(dis_rain$discharge_obs)
      P
      d
      
      rc=d/P                            # runoff coefficient


      rc_summary=data.frame(Start_date=start1,
                              End_date=end1,
                              Sum_disobs_m3=d,
                              Sum_rain_m3=P,
                              rc=round(rc,digits = 3)) 
      rc_summary


#Rundungsfehler
#1590477/513999
#1590554/513999
