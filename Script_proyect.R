###########################################################################################
##Proyecto de Ecologia de mosquitos usando R 
## R-code for Correlation analysis
## Juan D. Sanchez
## PROGRAM NAME: 
## PROGRAM FUNCTIONS: 
##
## last update:  Febrero 28 de 2017

###################################################################################

#Assumptions": Correlation requires two continuos variables

#check the R version
sessionInfo()





# Loading libraries and packages --------------------------------------------------------
# Install if required : 
install.packages("car")
install.packages("vegan")
install.packages("ggplot") 
install.packages("lattice")

##general packages for data plotting and visualization
library(lattice)
library(ggplot2)
library(car)
library(vegan)
#########################################################################################################


# Reading the data -------------------------------------------

# Read the data from the file, 
datamosquitos<-read.csv(file.choose("abmossec"), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#View the first few lines
head(datamosquitos)
summary(datamosquitos)         #to see if the data seems ok
str(datamosquitos)             #to see if the variable names and type are correct
which(is.na(datamosquitos)==T) # checking that there are  not missing values


# Visualizing the data and assumptions (e.g normality) -----------------------
# two ways of doing plots
#1
plot(datamosquitos$mosquitos~datamosquitos$bosque) 
#2
plot(mosquitos~bosque, data=datamosquitos)
plot(mosquitos~s_descubierto, data=datamosquitos)
plot(mosquitos~c_agua, data=datamosquitos)

# Exploring the distribution of the data

# The data in the database is small, so I guess you will have more data,
#Explore the variables of interest and see how is their distribution and normality

hist(datamosquitos$mosquitos) #Distribution

qqnorm(datamosquitos$mosquitos) #normality
qqline(datamosquitos$mosquitos, lty=2)

shapiro.test(datamosquitos$mosquitos) # iF P<0.05 data deviate from normality,





