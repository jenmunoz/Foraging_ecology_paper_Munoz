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

#Model selection
install.packages("AICcmodavg")
library(AICcmodavg)
#########################################################################################################


# Reading and cleaning the data -------------------------------------------

# Read the data from the file, View the first few lines
foraging<-read.csv(file.choose("foraging_all_ species_coffee_2011"), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
head(foraging)
summary(foraging)
str(foraging)
which(is.na(foraging)==T)

##Prepring the da Variables as factor
### convert variables 
foraging$foragingrate<- as.numeric(foraging$foragingrate)
foraging$sociality<- as.factor(foraging$sociality)
foraging$flocksizespecies<- as.numeric(foraging$flocksizespecies)

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


# Visualizing the data ----------------------------------------------------

# visualizing the data for each species
## boxplot with stripchart on the back for foraging data
#Migrants
#Setophaga fusca
boxplot(fusca$foragingrate~fusca$sociality, main='Setophaga fusca',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(fusca$foragingrate~fusca$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Setophaga cerulea
boxplot(cerulea$foragingrate~cerulea$sociality, main='Setophaga cerulea',ylab='Capture rate/min',col="white",ylim=c(0,20), width=c(1.0, 1.0))
stripchart(cerulea$foragingrate~cerulea$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Cardelina canadensis
boxplot(canadensis$foragingrate~canadensis$sociality, main='Cardelina canadensis',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(canadensis$foragingrate~canadensis$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Oreothlypis peregrina
boxplot(peregrina$foragingrate~peregrina$sociality, main='Oreothlypis peregrina',ylab='Capture rate/min',col="white",ylim=c(0,25))
stripchart(peregrina$foragingrate~peregrina$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,25))
#Residents
#Hemithraupis guira
boxplot(guira$foragingrate~guira$sociality, main='Hemithraupis guira',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(guira$foragingrate~guira$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Zimmerius chrysops
boxplot(chrysops$foragingrate~chrysops$sociality, main='Zimmerius chrysops',ylab='Capture rate/min',col="white",ylim=c(0,10))
stripchart(chrysops$foragingrate~chrysops$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,10))
#Parula pitiayumi
boxplot(pitiayumi$foragingrate~pitiayumi$sociality, main='Parula pitiayumi',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(pitiayumi$foragingrate~pitiayumi$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))

# Combining the plots in a multiplot graph?

ggplot(foraging, aes(flocksizespecies, foragingrate)) + xlab("X variable name") + ylab("Y variable name") +
  geom_point(col = "black", size = I(2)) +
  geom_smooth(method = lm, size = I(1), se = FALSE, col = "black") +
  facet_wrap(~species, ncol = 0)

##plot a frequency data distribution 
###R##it seems as a poisson 
hist(fusca$foragingrate)
hist(cerulea$foragingrate)
hist(canadensis$foragingrate)
hist(peregrina$foragingrate)
hist(chrysops$foragingrate)
hist(guira$foragingrate)
hist(pitiayumi$foragingrate)


#checking the normality of the data ### seems like it is non-normal
qqnorm(fusca$foragingrate)
qqline(fusca$foragingrate, lty=2)
qqnorm(cerulea$foragingrate)
qqnorm(canadensis$foragingrate)
qqnorm(peregrina$foragingrate)
qqnorm(chrysops$foragingrate)
qqnorm(guira$foragingrate)
qqnorm(pitiayumi$foragingrate)

shapiro.test(flocksd$Number_of_species) # iF P<0.05 daa deviate from normality, all deviate from normality

shapiro.test(fusca$foragingrate)
shapiro.test(cerulea$foragingrate)
shapiro.test(canadensis$foragingrate)
shapiro.test(peregrina$foragingrate)
shapiro.test(chrysops$foragingrate)
shapiro.test(guira$foragingrate)
shapiro.test(pitiayumi$foragingrate)

# test for overdisspersion 
# is the variance is higher that the mean the data is overdispersed
tapply(fusca$foragingrate, fusca$sociality, mean)
tapply(fusca$foragingrate, fusca$sociality, var)

tapply(canadensis$foragingrate, canadensis$sociality, mean)
tapply(canadensis$foragingrate, canadensis$sociality, var)

tapply(cerulea$foragingrate, cerulea$sociality, mean)
tapply(cerulea$foragingrate, cerulea$sociality, var)

tapply(peregrina$foragingrate, peregrina$sociality, mean)
tapply(peregrina$foragingrate, peregrina$sociality, var)

# in chrysops is not overdisspersed, then poisson distribution
tapply(chrysops$foragingrate, chrysops$sociality, mean)
tapply(chrysops$foragingrate, chrysops$sociality, var)

tapply(guira$foragingrate, guira$sociality, mean)
tapply(guira$foragingrate, guira$sociality, var)

tapply(pitiayumi$foragingrate, pitiayumi$sociality, mean)
tapply(pitiayumi$foragingrate, pitiayumi$sociality, var)
# in chrysops is the only speces which data is not overdisspersed, then poisson distribution, for all the other I will apply quasipoisson

##because they deviate from normality, I will run GLM instead of lm
# Data is overdispersed then I will use quasi poisson distribution

# Variables as factor
### convert variables 
foraging$foragingrate<- as.numeric(foraging$foragingrate)
foraging$sociality<- as.factor(foraging$sociality)
foraging$flocksizespecies<- as.numeric(foraging$flocksizespecies)


# Model fit, assumptions and model selection ------------------------------


######Fit the model
###### GLM Generalized linear models ( warning: Sociality need to be a  factor)
#quasipoisson used for over disspersed count data, log link used for count data
# Because there is not AIC for quasipoisson distributions I followed the recommendtions fron (Bolker 2016)
#For migrant species I included sociality as variable, for modl selection
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 
glm2<-update(glm1, . ~ . - dayofseason)
glm3<-update(glm1, . ~ . - sociality)
glm4<-update(glm3, . ~ . - dayofseason)
glmQ1<-update(glm1, family=quasipoisson(link="log"))
glmQ2<-update(glm2, family=quasipoisson(link="log"))
glmQ3<-update(glm3, family=quasipoisson(link="log"))
glmQ4<-update(glm4, family=quasipoisson(link="log"))


summary(glmQ1)
summary(glm3)
summary(glm4)
summary(glm2)


##########################Checking the assumptions of the model################################################
#1 Residuals are normally distributed
fv<-fitted(glm1)    # predicted (fitted) values on original scale
re<-residuals(glm1,type="response")  # residuals
plot(fv,re)     # For constant variance, if homogeneous distrbuted in the graph
qqnorm(re) # to check the normality of the data
qqline(re) # to add the line of normalite
hist(re)         # to see if the residuals distribute normaly

###2 the residuals are  homogeneus  (homosedatic), if they are disperse equal acroos the graph ###oops they are not 
plot(re~glm1$fitted.values)

#then, calculate theta to see if there are overdisoersion, if >1 is overdispersion, the use a quiasipoisson, 
theta<-glm1$deviance/glm1$df.residual
theta
#theta=1.98

#3### not autocorrelation ( correlation between succesive data points)
durbinWatsonTest(glm1)

#Results of the test, if pvalue less than 0.05  there is a significan serial autocorrelation
#durbinWatsonTest(gamdiversity1)
lag Autocorrelation D-W Statistic p-value
1      0.09337523       1.80187    0.16
Alternative hypothesis: rho != 0

#4# the model is not biased by influencial observations
#leverage #not sure how to calculate it
172 is an influencial point
# removing the influencial point
flocksd$Number_of_species[172]
gamdiversity1updated<-update(gamdiversity1,subset=(flocksd$Number_of_species!=6))
##### ploting the new model with out the influencial point
#plot(gamdiversity1updated, pch=19, cex=0.25, col='#FF8000',shade=TRUE, shade.col="grey",se=TRUE)
#plot(gamdiversity1updated, residuals=TRUE, pch=19, cex=0.6, col='navy',shade=TRUE, shade.col="grey",se=TRUE)
#summary(gamdiversity1) 
#summary(gamdiversity1updated) 

################################################################
# MODEL SELECTION ---------------------------------------------------------
######################################################################
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 
glm2<-update(glm1, . ~ . - dayofseason)
glm3<-update(glm1, . ~ . - sociality)
glm4<-update(glm3, . ~ . - dayofseason)
glmQ1<-update(glm1, family=quasipoisson(link="log"))
glmQ2<-update(glm2, family=quasipoisson(link="log"))
glmQ3<-update(glm3, family=quasipoisson(link="log"))
glmQ4<-update(glm4, family=quasipoisson(link="log"))

############EXTRACT THE OVERDISPERSION PARAMETER, also given when run the model, it is different, but it is recommendend the one given in the model
theta<-glm1$deviance/glm1$df.residual
theta

# In the analysis I incorporate the following calculation from Balker 2016, and the overdisppersion number that i gave me
#is the same that the summary of the models gave me too.
dfun<-function(glmQ1){with(glmQ1,sum((weights * residuals^2)[weights > 0])/df.residual)}
dfun(glmQ1)
summary(glmQ1)    #also give me the overdispersion parameter

##### Model selection using  AICcmodavg
library(AICcmodavg)

###Model used  (glm1Q,glm2Q,glm3Q,glm4Q),
aictab(list(glm1,glm2,glm3,glm4),
       modnames=c("Socality+Seasonday",
                  "Sociality",
                  "Seasonday",
                  "~"),
       c.hat=1.97)

summary (glm1)

####Model selection based on QAICc:
#(c-hat estimate = 1.97)

#K  QAICc Delta_QAICc QAICcWt Cum.Wt Quasi.LL
#Socality+Seasonday 4 252.65        0.00       1      1  -122.11
#Sociality          3 263.55       10.90       0      1  -128.65
#Seasonday          3 300.17       47.52       0      1  -146.96
#~                  2 302.35       49.70       0      1  -149.11

### visualizing the  best model fit,  how to interprete this?
visreg(glmQ1, type = "conditional") #using the median for the ther predictors
visreg(glmQ1,"sociality", type = "conditional", xlim=c(0,40), ylim=c(0,30), scale = "response", ylab="",  xlab=NA) 
par(new = TRUE)
plot(foragingrate~sociality, data=fusca, ylab="foraging rate ", xlab="Sociality", main="a) Dendroica fusca ",pch=16, cex=1.3, ylim=c(0,30), xlim=c(0,40))

visreg(glm1, type="conditional", scale="response", ylim=c(0,40), ylab="Foraging rate", pch=1,rug=0, whitespace=0.6, par=TRUE
par(new = TRUE)
stripchart(fusca$foragingrate ~ fusca$sociality, method='jitter', vertical=TRUE, pch=19, cex=0.8, ylim=c(0,40), ylab="", whitespace=0.6)
str(fusca)

# Hypothesis testing ------------------------------------------------------

#####################################################################
#######Testing Hypothesis USING THE BEST MODEL
#are the difference in foraging rate statistically significant? USING
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 
glmQ1<-update(glm1, family=quasipoisson(link="log"))

# In the summary the Estimate std are the stimate parameters in the log scale, 
#the first one is the mean of the first group type, the others are the difference between the first group mean and the other groups,

#anova(model2, test="Chi"), I guess i can use any of the two qasi or poisson since the stimates are the same
Anova(glm1, type = 3, test="F") 
Anova(glmQ1, type = 3, test="F") 

#If I only include sociality in the model
Anova(glm2, type = 3, test="F") 
Anova(glmQ2, type = 3, test="F") 

## f test appropiate for quasipoisson distributions, and type3 anova order orthe term does not matter
# test the null H that there is no differences in the foraging rate  among flocking and non-flocking individuals
#Note: Use anova(test = "F"), ie an F test, if testing hypotheses that use gaussian, quasibinomial or quasipoisson link functions.
#This is because the quasi-likelihood is not a real likelihood and the generalized log-likelihood ratio test is not accurate FROM dolph class
#Anova type=3 rom the car package, in that case order of the terms in the model does not matter and neither does hirarchy

# RESULTS F test if p<0.005 differences between groups are significant
Analysis of Deviance Table (Type III tests)

Response: Similarity
SS    Df      F Pr(>F)
Type.flock 0.17824  2 2.1055 0.1342
Residuals  1.82006 43 

#### years
boxplot(stability2$Similarity ~ stability2$Type.flock, ylab="Stability over years (1-Jaccard similarity index)",xlab="Elevation", xlab='Type of flock', col="light blue",, ylim=c(0,1))
stripchart(stability2$Similarity ~ stability2$Type.flock, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,1))

#### days small sample size
boxplot(stability1$Similarity ~ stability1$Type.flock, ylab='Flock similarity over time', xlab='Type of flock', col="light blue",, ylim=c(0,1))
stripchart(stability1$Similarity ~ stability1$Type.flock, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,1))





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

#####For movement rate

## boxplot with stripchart on the back for foraging data
#Migrants
#Setophaga fusca
boxplot(fusca$movementrate~fusca$sociality, main='fusca',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(fusca$movementrate~fusca$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Setophaga cerulea
boxplot(cerulea$movementrate~cerulea$sociality, main='cerulea',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(cerulea$movementrate~cerulea$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))
#Cardelina canadensis
boxplot(canadensis$movementrate~canadensis$sociality, main='canadensis',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(canadensis$movementrate~canadensis$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Oreothlypis peregrina
boxplot(peregrina$movementrate~peregrina$sociality, main='peregrina',ylab='Capture rate/min',col="white",ylim=c(0,25))
stripchart(peregrina$movementrate~peregrina$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,25))
#Residents
#Hemithraupis guira
boxplot(guira$movementrate~guira$sociality, main='guira',ylab='Capture rate/min',col="white",ylim=c(0,15))
stripchart(guira$movementrate~guira$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,15))
#Zimmerius chrysops
boxplot(chrysops$movementrate~chrysops$sociality, main='chrysops',ylab='Capture rate/min',col="white",ylim=c(0,10))
stripchart(chrysops$movementrate~chrysops$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,10))
#Parula pitiayumi
boxplot(pitiayumi$movementrate~pitiayumi$sociality, main='pitiayumi',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(pitiayumi$movementrate~pitiayumi$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20))

# Foraging rate vs group size
# Combining the plots in a multiplot graph, be aware of the linear model

ggplot(foraging, aes(flocksizespecies, foragingrate)) + xlab("X variable name") + ylab("Y variable name") +
  geom_point(col = "black", size = I(2)) +
  geom_smooth(method = lm, size = I(1), se = FALSE, col = "black") +
  facet_wrap(~species, ncol = 0)
