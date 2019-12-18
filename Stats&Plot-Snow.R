#
# Analyse modelled snow 
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

working_dir="E:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED/"
thread_dir="2-2/"
outfolder="Output-a/"  

#wasa_input_dir=paste(working_dir, thread_dir, "Input/Time_series/",sep="")
wasa_output_dir=paste(working_dir, thread_dir, outfolder,sep="")


# Read snowCover.out
scovfile="snowCover.out"
scovdata = read.table(paste(wasa_output_dir, scovfile, sep=""),header = T,skip = 1,na.strings = c("NaN","Inf","-Inf","*"))

# show first entries
scovdata=data.frame(scovdata)
head(scovdata)

# Calculate statistics of snow cover ####
min(scovdata$value)
mean(scovdata$value)
median(scovdata$value)
max(scovdata$value)

# only for snow cover
snowcover=scovdata$value
snowcover=data.frame(snowcover)
summary(snowcover) #problem with dataframe size


# Scatterplot Matrix ####
  # Give the chart file a name.
  #png(file = "scatterplot_matrices.png")

# Plot the matrices between 4 variables giving 12 plots.
# One variable with 3 others and total 4 variables.

pairs(~Subbasin+LU+TC+value,data = scovdata, # "pairs" plots Scatterplot matrix
      main = "Scatterplot Matrix")

  # Save the file.
  #dev.off()


#LU115 <- which(scovdata$Subbasin == 115)


# Weitere Skripte sh. E:\Anne\wasaSnow.zip\wasaSnow\CalibWASA\R

