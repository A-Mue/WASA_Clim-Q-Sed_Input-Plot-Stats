#
# Calculate extra-terrestrial radiation, monthly mean daily values [Wm-2]
# for WASA-SED Input extraterrestrial_radiation.dat
# 
# Source code: Package "sirad"
# 
# Anne Müller
#________________________________________________________________________________


install.packages("sirad")
library("sirad")

#-------------------------------------
# Function 'extrat' 
#-------------------------------------

# calculates hourly and daily extraterrestrial solar radiation for a given time and location.
# Usage:
# extrat(i, lat)  # i = day number in the year (julian day), lat = latitude in radians

# Example:

# extraterrestrial radiation and daylength for 1 January and latitude 55 degrees
extrat(dayOfYear("2011-01-01"), radians(55))

# Result = List of 3 elements:
#   ExtraTerrestrialSolarRadiationDaily: daily sum of extraterrestrial radiation [MJm-2]
#   TerrestrialSolarRadiationHourly: vector of length 24 of hourly sums of extraterrestrial radiation [MJm-2]
#   DayLength: day length in hours



#-------------------------------------------------
# Calculation for several dates (WASA-SED Input)
#-------------------------------------------------

# Latitude in degrees
lat_deg<-(32)  # - South/ + North (-19.5 = 19.5 degrees South)
# Dates, e.g. 15th of each month of the year
dates<-c("2011-01-15","2011-02-15","2011-03-15","2011-04-15","2011-05-15","2011-06-15","2011-07-15","2011-08-15","2011-09-15","2011-10-15","2011-11-15","2011-12-15")

# Calculate extraterrestrial radiation in [MJm-2]
exra_MJ_dates<-extrat(dayOfYear(dates),radians(lat_deg))
exra_MJ_dates$ExtraTerrestrialSolarRadiationDaily #Daily extraterrestrial radiation in [MJm-2]

# Conversion to [Wm-2] as needed for WASA-SED
exra_W_dates<-(exra_MJ_dates$ExtraTerrestrialSolarRadiationDaily)*(10^6)/86400 #for average watts of 1 day, multiply by 10^6 to get J, divide by 86400 s/day
# exra_W_dates #Daily extraterrestrial radiation in [Wm-2] of specified dates

# Result
round(exra_W_dates, digits = 0) #Rounded to 0 digits, Daily extraterrestrial radiation in [Wm-2] of specified dates

# Add these values manually to your file "extraterrestrial_radiation.dat"

#-------------------------------------
# Calculation for one date
#-------------------------------------

# Latitude in degrees
lat_deg<-(-19.5)  # -19.5 degrees = 19.5 degrees South
# Date
date<-"2011-12-15"

# Calculate extraterrestrial radiation in [MJm-2]
exra_MJ<-extrat(dayOfYear(date),radians(lat_deg))
exra_MJ$ExtraTerrestrialSolarRadiationDaily #Daily extraterrestrial radiation in [MJm-2]

# Conversion to [Wm-2] as needed for WASA-SED
exra_W<-(exra_MJ$ExtraTerrestrialSolarRadiationDaily)*(10^6)/86400 #for average watts of 1 day, multiply by 10^6 to get J, divide by 86400 s/day
# exra_W #Daily extraterrestrial radiation in [Wm-2]

# Result
round(exra_W, digits = 0) #Rounded to 0 digits, Daily extraterrestrial radiation in [Wm-2]

