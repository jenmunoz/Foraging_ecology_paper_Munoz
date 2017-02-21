###############################JENNY MUNOZ#####################################
###############################################################################
###########Foraging ecology of residents and migrants##########################

#Assumptions": 
#check the R version
sessionInfo()
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
library(dplyr)
library(warbleR)
library(vegan)
library(permute)

#########################################################################################################


# Read the data from the file, View the first few lines
foraging<-read.csv(file.choose("foraging_all_ species_coffee_2011"), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
head(foraging)
summary(foraging)
str(foraging)
which(is.na(foraging)==T)

#Filter the data using dplyr for each species
foraging %>%
  filter(species == "Setophaga fusca")

fusca<-filter(foraging,species=="Setophaga fusca")
canadensis<-filter(foraging, species=="Cardelina canadensis")
cerulea<-filter(foraging,species=="Setophaga cerulea")
peregrina<-filter(foraging,species=="Oreothlypis peregrina")
guira<-filter(foraging,species=="Hemithraupis guira")
chrysops<-filter(foraging,species=="Zimmerius chrysops")
pitiayumi<-filter(foraging,species=="Parula pitiayumi")


# visualizing the data for each species


## boxplot with stripchart on the back
#Migrants
#Setophaga fusca
boxplot(fusca$foragingrate~fusca$sociality, main='fusca',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(fusca$foragingrate~fusca$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Setophaga cerulea
boxplot(cerulea$foragingrate~cerulea$sociality, main='cerulea',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(cerulea$foragingrate~cerulea$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Cardelina canadensis
boxplot(canadensis$foragingrate~canadensis$sociality, main='canadensis',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(canadensis$foragingrate~canadensis$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Oreothlypis peregrina
boxplot(peregrina$foragingrate~peregrina$sociality, main='peregrina',ylab='Capture rate/min',col="white",ylim=c(0,25))
stripchart(peregrina$foragingrate~peregrina$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,25))
#Residents
#Hemithraupis guira
boxplot(guira$foragingrate~guira$sociality, main='guira',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(guira$foragingrate~guira$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Zimmerius chrysops
boxplot(chrysops$foragingrate~chrysops$sociality, main='chrysops',ylab='Capture rate/min',col="white",ylim=c(0,10))
stripchart(chrysops$foragingrate~chrysops$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,10))
#Parula pitiayumi
boxplot(pitiayumi$foragingrate~pitiayumi$sociality, main='pitiayumi',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(pitiayumi$foragingrate~pitiayumi$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))




# Variables as factor

foraging$flocksizespecies<- as.numeric(foraging$flocksizespecies)
foraging$Gender<- as.factor(foraging$Gender)
#Linearfixed model
lm<-lm(Foraging.rate ~ Flocksize+Gender+ Flocksize:Gender, data=foraging)
plot(lm)

#Transforming the response variable to met assumptions
foraging$Logforaging <- log((foraging$Foraging.rate)+1)

# visualizing the data
plot(foraging$Flocksize,foraging$Logforaging, 
     pch = as.numeric(foraging$Gender), col = c(foraging$Gender))


#Lineal fixed model including  gender, and the interaction
linear2<-lm(Logforaging ~ Gender+Flocksize + Flocksize:Gender, data=foraging)
plot(linear2)
#Lineal fixed model including the factor gender
linear1<-lm(Logforaging ~ Gender+Flocksize, data=foraging)
#Lineal simple fixed model
linear<-lm(Logforaging ~ Flocksize, data=foraging)

#Exploring the explanatory power of both models
# Anova type =2 where hierarchy matter but order do not.

library(car)

Anova(linear2,type=2)

#The parameter estimates for the model including the interaction

summary (linear2)

# But after the Anova I knew that
#the simpler model (linear) does not have signicicantly lower explanatoty power
summary (linear)

#Plot the model including both factors and the interaction term.
plot (foraging$Flocksize,foraging$Logforaging, 
      pch = as.numeric(foraging$Gender), col = c(foraging$Gender),
      ylab="Foraging rate (cap/min)",xlab="Flock size (Number of species)")

groups <- levels(foraging$Gender)                     
for(i in 1:length(groups)){
  xi <- foraging$Flocksize[foraging$Gender==groups[i]]                  
  yhati <- fitted(linear2)[foraging$Gender==groups[i]]       
  lines(xi[order(xi)], yhati[order(xi)],col=as.numeric(i)) }


# Plot the simpler model

plot (foraging$Flocksize,foraging$Logforaging, 
      pch = as.numeric(foraging$Gender), col = c(foraging$Gender),
      ylab="Foraging rate (cap/min)",xlab="Flock size (Number of species)")

groups <- levels(foraging$Gender)                     
for(i in 1:length(groups)){
  xi <- foraging$Flocksize[foraging$Gender==groups[i]]                  
  yhati <- fitted(linear)[foraging$Gender==groups[i]]       
  lines(xi[order(xi)], yhati[order(xi)],col=as.numeric(i)) }

### plots foraging rate and flock size
plot(fusca$flocksizespecies,fusca$foragingrate, 
     pch = as.numeric(fusca$gender), col = c(fusca$gender))

plot(fusca$foragingrate,fusca$flocksizespecies)

