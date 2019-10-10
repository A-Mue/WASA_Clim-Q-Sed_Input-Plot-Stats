# Copyright (C) 2019 Anne Müller 


#Show snow routine output ####

#Plot Snow water equivalent ####
#Problem if snow water equiv. accumulates over time (for all or certain TC)
#Check with this plot
filedir="D:/Anne/_SaWaM_Data_/2_KarunDez/WASA-SED/2-2/Output-a0/snowWaterEquiv.out"

file=read.table(filedir, header = F,  sep = "\t", dec = ".", na.strings = c("-9999.00","-9999","NA","NaN"), skip=2)
head(file)
SnowWatEquiv=file$V7
plot(x=c(1:length(SnowWatEquiv)), SnowWatEquiv, type="l")

##To know the current storage capacity
memory.limit()
## To increase the storage capacity
memory.limit(size=30000)
plot(x=c(1:3),y=c(1:3),type="l")
