###############################JENNY MUNOZ#####################################
###############################################################################
###########Foraging ecology of residents and migrants##########################

#Assumptions": 

# Install: 
install.packages("car")
install.packages("vegan")
install.packages("AICcmodavg")

# Loading packages --------------------------------------------------------
##general packages  and visualization
library(lattice)
library(ggplot2)
library(car)
library(visreg)

#########################################################################################################


#Visualizing the data


fusca<-read.csv(file.choose("fusca.csv"), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
head(fusca)
str(fusca)

fusca$Flock.size.Species.number.<- as.numeric(fusca$Flock.size.Species.number.)
fusca$Flock.size.Birds.number.<- as.numeric(fusca$Flock.size.Birds.number.)  
fusca$Seasonality<- as.character(fusca$Seasonality)   
str(fusca)   