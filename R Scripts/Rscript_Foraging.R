###############################JENNY MUNOZ#####################################
###############################################################################
## Proyect Foraging ecology of residents and migrants
## R-code for manuscript Generalized linear models
## Jenny Munoz
##
## last update: February 20 2017
################################################################################

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
#library(warbleR)
library(vegan)
library(permute)
library(sjPlot) #Make tables of the glm stimates https://rdrr.io/cran/sjPlot/man/sjt.glm.html
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
foraging$flocksizeind<- as.numeric(foraging$flocksizeind)
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



glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 

##### Evaluate the fit of Models


stripchart (foragingrate~sociality, vertical=TRUE, data=foraging,pch=16,col=c("seagreen4","seagreen2","turquoise","skyblue1","royalblue","darkblue"),method="jitter",jitter=0.1,ylab="",xlab="")
points(c(1,2,3,4,5,6),mean, pch="-",col="red",cex=5)
stripchart(fitted(glm1)~foraging$foraging, vertical=TRUE,add=TRUE, pch="-",cex=2,method="jitter",col="black",na.strings=c("NA",""))
stripchart(fitted(glm2)~foraging$sociality, vertical=TRUE,add=TRUE, pch="-",cex=2, method="jitter",col="tan1")
stripchart(fitted(glm3)~foraging$sociality, vertical=TRUE,add=TRUE, pch="-",cex=2,method="jitter",col="gray")
legend("bottomright", legend=c("additive model","dominance model","genotype model","observed mean"),bty="n",lwd=2,cex=0.8, col=c("black","tan1","gray","red"), lty=c(1,1,1))
dev.off()

pdf(file="figure2.pdf")
stripchart (foragingrate~sociality, vertical=TRUE, data=foraging,method="jitter",jitter=0.1,ylab="foraging rate",xlab="sociality", pch=19,cex=0.8, ylim=c(0,20))
stripchart(fitted(glm1)~foraging$sociality, vertical=TRUE,add=TRUE, pch="-",cex=2,method="jitter",col="black")
points(c(1,2),mean, pch="-",col="red",cex=5)
legend("bottomright", legend=c("fitted","observed mean"),bty="n",lwd=2,cex=0.8, col=c("black","red"), lty=c(1,1,1))
dev.off()

boxplot(fusca$foragingrate~fusca$sociality, main='Setophaga fusca',ylab='Capture rate/min',col="white",ylim=c(0,20))
stripchart(fusca$foragingrate~fusca$sociality, method='jitter', add=TRUE, vertical=TRUE, pch=19, cex=0.8, ylim=c(0,20), method="jitter", col="black")

# Calculating teh explained deviance
pseudo.R2<-(glm1$null.deviance-glm1$deviance)/glm1$null.deviance
pseudo.R2
#pseudoR2() explained deviance)=0.3489

######################################################################################################
#####Visualizing the data using ggplot!!!!

library(ggplot2)

# For fusca
ggplot(fusca, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point() + 
  geom_smooth(method = "lm")

head(fusca)

ggplot(fusca, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizeind)) + 
  geom_smooth(method = "lm")

## The best fitted model 
ggplot(fusca, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizespecies)) + 
  geom_smooth(method = "glm",fullrange=TRUE, method.args=list(family="quasipoisson")) + 
  facet_wrap(~species, scales = "free") + 
  theme_bw()
#ggplot(fusca, aes(x = dayofseason, y = foragingrate, z= sociality, colour = sociality)) +
  geom_point(aes(size = flocksizespecies)) + 
  #geom_smooth(method= "glm", formula = foragingrate ~ sociality + dayofseason, family = quasipoisson(link = "log") + 
  #facet_wrap(~species, scales = "free") 
  
  
ggplot(fusca, aes(x = dayofseason, y = foragingrate, colour=sociality)) +
  geom_point(aes(size = flocksizespecies)) + 
  geom_smooth(method = "glm", family="quasipoisson", formula= foraging~sociality + dayofseason ) + 
  facet_wrap(~species, scales = "free") + 
theme_bw()

geom_smooth(method = "glm", formula = foragingrate~ sociality + dayofseason, method.args=list(family="quasipoisson"),data=fusca) +

plot (fusca$dayofseason,fusca$foragingrate, pch=19,
      ylab="Number of species",xlab="dayofseason", xlim=c(0,40), col=as.numeric(fusca$sociality))
lm2<-lm(foragingrate~dayofseason+sociality, data= foraging) 
abline(lm2)

plot(Numspecies~Mean_canopy_H, data=flocksd)
model<-lm(Numspecies~Mean_canopy_H, data=flocksd)
abline(model, col="red", cex=7)
summary(model)

groups <- levels(as.factor(fusca$sociality))     
for(i in 1:length(groups)){
  xi <- fusca$dayofseason[fusca$sociality==groups[i]]                  
  yhati <- fitted(glm1)[fusca$sociality==groups[i]]       
  lines(xi[order(xi)], yhati[order(xi)],col=as.numeric(i))}


### For all the species
# Smooth linear
ggplot(foraging, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizeind)) + 
  geom_smooth(method = "glm")+ 
  facet_wrap(~species, scales = "free") + 
  theme_bw()

#changing alfa
ggplot(foraging, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizeind), alpha = 0.6) + 
   geom_smooth(method = "lm") + 
  facet_wrap(~species, scales = "free") + 
  theme_bw()

### FREE SMOOTH
ggplot(foraging, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizeind), alpha = 0.6) + 
  # geom_smooth(method = "lm") + 
  geom_smooth() + 
  facet_wrap(~species, scales = "free") + 
  theme_bw()

# With number of species 
ggplot(foraging, aes(x = dayofseason, y = foragingrate, colour = sociality)) +
  geom_point(aes(size = flocksizespecies), alpha = 0.6) + 
  # geom_smooth(method = "lm") + 
  geom_smooth() + 
  facet_wrap(~species, scales = "free") + 
  theme_bw()

############EXTRACT THE OVERDISPERSION PARAMETER, also given when run the model, it is different, but it is recommendend the one given in the model
theta<-glmQ1$deviance/glm1$df.residual
theta

# In the analysis I incorporate the following calculation from Balker 2016, and the overdisppersion number that i gave me
#is the same that the summary of the models gave me too.
dfun<-function(glmQ1){with(glmQ1,sum((weights * residuals^2)[weights > 0])/df.residual)}
dfun(glmQ1)
summary(glmQ1)    #also give me the overdispersion parameter

#
### visualizing the  best model fit,  how to interprete this?
visreg(glmQ1, type = "conditional") #using the median for the ther predictors
visreg(glmQ1,"sociality", type = "conditional", xlim=c(0,40), ylim=c(0,30), scale = "response", ylab="",  xlab=NA) 
par(new = TRUE)
plot(foragingrate~sociality, data=fusca, ylab="foraging rate ", xlab="Sociality", main="a) Dendroica fusca ",pch=16, cex=1.3, ylim=c(0,30), xlim=c(0,40))

#visreg(glm1, type="conditional", scale="response", ylim=c(0,40), ylab="Foraging rate", pch=1,rug=0, whitespace=0.6, par=TRUE
#par(new = TRUE),
stripchart(fusca$foragingrate ~ fusca$sociality, method='jitter', vertical=TRUE, pch=19, cex=0.8, ylim=c(0,40), ylab="", whitespace=0.6)
str(fusca)

# Hypothesis testing ------------------------------------------------------

#####################################################################
#######Testing Hypothesis USING THE BEST MODEL
# Summary of the model allow me to interpret the estimates parameters (e.g effect sizes) in the model and the difference from cero and between them.
# The summary give me the effect sizes. For example when I do summary (glm2, the model including only sociality), I can interprete the Intercept as the log transformation of  the mean of the first level of the factr sociality, 
# Inthis case the intercept is the log (mean capture rate for birds in flocks),  the socialitysolitary is the log Difference of the mean on birds in flocks with the mean of solitary. 
# And the significance p-value tell me if they are different form cero (that is not really interesting0), THE SUMMARY (model) IS MOSTLY TO SEE THE EEFFECT SIZE. From this I can say or example that birds in flock forage n given times, more when they are in flocks.
#significant difference among them. From this I can say or example that birds in flock forage n given times, more when they are in flocks.
# Be aware that interpretation is a bit more complex for the model including sociality and day of the season
# In the summary the Estimate are the stimate parameters in the log scale, 
#the first one is the mean of the first group type, the others are the difference between the first group mean and the other groups,
summary(glm2)

#are the difference in foraging rate statistically significant? USING
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 
glmQ1<-update(glm1, family=quasipoisson(link="log"))

### Anova allow me to do hyphothesis testing
# The ANOVA atable compares the fit of two models, the null model foraging~1 vs foraging~sociality+....
## f test appropiate for quasipoisson distributions, and type3 anova order orthe term does not matter
# test the null H that there is no differences in the foraging rate  among flocking and non-flocking individuals
#Note: Use anova(test = "F"), ie an F test, if testing hypotheses that use gaussian, quasibinomial or quasipoisson link functions.
#This is because the quasi-likelihood is not a real likelihood and the generalized log-likelihood ratio test is not accurate FROM dolph class
#Anova type=3 rom the car package, in that case order of the terms in the model does not matter and neither does hirarchy


#anova(model2, test="Chi") using the "best model" I guess i can use any of the two qasi or poisson since the stimates are the same?
Anova(glm1, type = 3, test="F") 
Anova(glmQ1, type = 3, test="F") 

summary(glm2)

#If I only include sociality in the model
Anova(glm2, type = 3, test="F") 
Anova(glmQ2, type = 3, test="F") 

summary(glmQ2)


#Other way to do this no te the "a"
anova (glm1,glm5, test="F")

## f test appropiate for quasipoisson distributions, and type3 anova order orthe term does not matter

##########################################################################
#Results for manuscript
###########################################################################

#1. Mixed species flocks features

#Filter only the flock data
flocks<-filter(foraging,sociality=="Flock")
flockfeatures<-na.omit(flocks)                             #omit NA values
summarise_each(flockfeatures,funs(mean))                   #Summarise all the columns
summarise(flockfeatures,mean(flocksizespecies))            #Summarise flock size 
summarise(flockfeatures,mean(flocksizeind))                #mean
summarise(flockfeatures,sd(flocksizespecies))              #standard deviation
summarise(flockfeatures,sd(flocksizeind)) 

summarise(flockfeatures,max(flocksizespecies))             #max and mins
summarise(flockfeatures,min(flocksizespecies))
summarise(flockfeatures,max(flocksizeind))
summarise(flockfeatures,min(flocksizeind))

flocksfusca<-filter(fusca,sociality=="Flock")
a<-summarise(flocksfusca,mean(foragingrate))
a

#Normality of the variables
hist(flockfeatures$flocksizespecies)
hist(flockfeatures$flocksizeind)
qqnorm(flockfeatures$flocksizeind) #normality
qqline(flockfeatures$flocksizeind, lty=2)
qqnorm(flockfeatures$flocksizespecies) #normality
qqline(flockfeatures$flocksizespecies, lty=2)

#Correlation test
#The alternative hypothesis of interest is that the flocksize is positively associated with the flockind.
cor.test(flockfeatures$flocksizespecies, flockfeatures$flocksizeind, method="kendall", alternative="greater")

################################################
#2. # Model fit, assumptions and model selection ------------------------------
################################################

######Fit the model
### Using two predictors
###### GLM Generalized linear models ( warning: Sociality need to be a  factor)
#quasipoisson used for over disspersed count data, log link used for count data
# Because there is not AIC for quasipoisson distributions I followed the recommendtions fron (Bolker 2016)
#For migrant species I included sociality as variable, for modl selection
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log")) 
glm2<-update(glm1, . ~ . - dayofseason)
glm3<-update(glm1, . ~ . - sociality)
glm4<-update(glm3, . ~ . - dayofseason) # Null model same than glm9<-glm(foragingrate~1, data =fusca, family=poisson(link="log"))
glmQ1<-update(glm1, family=quasipoisson(link="log"))
glmQ2<-update(glm2, family=quasipoisson(link="log"))
glmQ3<-update(glm3, family=quasipoisson(link="log"))
glmQ4<-update(glm4, family=quasipoisson(link="log"))

#glm5<-glm(foragingrate~1, data =fusca, family=poisson(link="log")) Null model
summary(glmQ1)
summary(glm3)
summary(glm4)
summary(glm9)

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
#lag Autocorrelation D-W Statistic p-value
#1      0.09337523       1.80187    0.16
#Alternative hypothesis: rho != 0

#4# the model is not biased by influencial observations
#leverage #not sure how to calculate it
#172 is an influencial point
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
# With two predictors
glm1<-glm(foragingrate~sociality+dayofseason, data =fusca,family=poisson(link="log"))
glm1<-glm(foragingrate~sociality+dayofseason, data =peregrina,family=poisson(link="log"))
glm1<-glm(foragingrate~sociality+dayofseason, data =canadensis,family=poisson(link="log"))
glm1<-glm(foragingrate~sociality+dayofseason, data =cerulea,family=poisson(link="log"))

glm1<-glm(foragingrate~sociality+dayofseason, data =chrysops,family=poisson(link="log"))
glm1<-glm(foragingrate~sociality+dayofseason, data =guira,family=poisson(link="log"))
glm1<-glm(foragingrate~sociality+dayofseason, data =pitiayumi,family=poisson(link="log"))

glm2<-update(glm1, . ~ . - dayofseason) #social context
glm3<-update(glm1, . ~ . - sociality) #day of season
glm4<-update(glm3, . ~ . - dayofseason)
glmQ1<-update(glm1, family=quasipoisson(link="log"))
glmQ2<-update(glm2, family=quasipoisson(link="log"))
glmQ3<-update(glm3, family=quasipoisson(link="log"))
glmQ4<-update(glm4, family=quasipoisson(link="log"))

glm5<-glm(foragingrate~1, data =fusca, family=poisson(link="log")) #Null model equivalent to glm2 long version

#Interesting models
glm6<-glm(foragingrate~sociality+dayofseason-1, data=fusca, family=poisson(link="log"))# Give me the log estimates of each group directly
glm7<-update(glm1, . ~ . + dayofseason:sociality) # including an interaction in the model
summary (glm6)
summary(glmQ1)

# Summary of the model allow me to interpret the estimates of the parameters (e.g effect sizes) in the model and the difference from cero and between them.
# The summary give me the effect size. 
summary(glm2)

#####overdisperssion parameters 
dfun<-function(glmQ1q){with(glmQ1,sum((weights * residuals^2)[weights > 0])/df.residual)}
dfun(glmQ1)

summary(glmQ1)

#fusca=1.97
#cerulea=2.22
#peregrina=2.27
#canadensis=1.56
#pitiayumi=1.67
#guira=2.92
#chrysops= 


#### Model selection using  AICcmodavg
library(AICcmodavg)

###Model used  (glm1Q,glm2Q,glm3Q,glm4Q), for all the species which have overdispersed datta
aictab(list(glm1,glm2,glm3,glm4),
       modnames=c("Socialcontext+Dayofseason",
                  "Socialcontext",
                  "Dayofseason",
                  "Intercept"),
       c.hat=1.67)

summary (glmQ2)

#### To make the table for AIC cfor species that are not overdisperssed

aictab(list(glm1,glm2,glm3,glm4),
       modnames=c("Socialcontext+Dayofseason",
                  "Socialcontext",
                  "Dayofseason",
                  "Intercept"))


#######SUMMARY TABLE
#A beautiful table in html for the parameters of glm models
#For the parameter stimates we need to consider the models with the Quasipoisson distribution, the points estimates are identical
# to the model with poisson distribution but the standard error and confidence intervals are wider!
sjt.glm(glmQ1,glmQ2,glmQ3,
        depvar.labels = c("Model1: Socialcontext+Dayofseason","Model2:Socialcontext","Model3:Dayofseason"),
        pred.labels = NULL,
        show.aicc=TRUE, 
        show.family=TRUE, 
        group.pred = FALSE,
        exp.coef = FALSE,  # if true Regression and cof.intervals are exponentiaded st.error are not in the unstransformed scale
        p.numeric=TRUE,
        robust = TRUE,
        show.se = TRUE,
        show.r2=TRUE,
        show.dev=TRUE,
        show.chi2=TRUE,
        cell.spacing = 0.001,
        sep.column = FALSE)

# A pvalue less than 0.05 indicates a good model fit 
# Summary of any model 
summary (glm2)
###To underestand the summary(glm) and the table with the estimate for sethophaga fusca.
#Let's start form the model that only include sociality as the only predictor (Flock and Solitaryare the leves). In this one the Intercept is the mean of the first level of the factor, in flocks, but it is in the log scale mean 5.25 log(5.25 =1.66) 
# next predictor is sociality(solitary) and is calculated as the difference of the logs of flock and solitary. log flock mean(5.25)log - LOG solitary mean =(1.65). In other words 1.65-0.5=1.15. If we exponentiate that value exp(1.15) that means that a fusca individual in flocks
# will eat 3.15 times more than solitary * 0.03 times the day of the season. Or what is the same a  solitary individual of fusca will eat exp(-1.33)= 0.26 times less what it will eat when in flocks~ 26% less

#The model  will be for fusca f.rate= exp [1.27]* exp[-1.33]*exp[0.03 * #day of the season]



exp(1.15)
log(1.65)-log(5.25)

###################################
# With three predictors
###################################
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =fusca,family=poisson(link="log"))
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =peregrina,family=poisson(link="log"))
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =canadensis,family=poisson(link="log"))
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =cerulea,family=poisson(link="log"))

glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =chrysops,family=poisson(link="log"))
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =guira,family=poisson(link="log"))
glm1a<-glm(foragingrate~sociality+dayofseason+flocksizespecies, data =pitiayumi,family=poisson(link="log"))

glm2a<-update(glm1a, . ~ . - sociality) # dayofseason + flocksize
glm3a<-update(glm1a, . ~ . - dayofseason) #sociality + flocksize
glm4a<-update(glm1a, . ~ . - flocksizespecies) # sociality+dayofseason
glm5a<-update(glm2a, . ~ . - dayofseason) # flocksize
glm6a<-update(glm2a, . ~ . - flocksizespecies) #dayofseason
glm7a<-update(glm4a, . ~ . - dayofseason )  #sociality
glm8a<-update(glm7a, . ~ . - sociality)  #intercept Equivalent to null model glm(foragingrate~1, data =fusca, family=poisson(link="log"))
glmQ1a<-update(glm1a, family=quasipoisson(link="log"))
glmQ2a<-update(glm2a, family=quasipoisson(link="log"))
glmQ3a<-update(glm3a, family=quasipoisson(link="log"))
glmQ4a<-update(glm4a, family=quasipoisson(link="log"))
glmQ5a<-update(glm5a, family=quasipoisson(link="log"))
glmQ6a<-update(glm6a, family=quasipoisson(link="log"))
glmQ7a<-update(glm7a, family=quasipoisson(link="log"))
glmQ8a<-update(glm8a, family=quasipoisson(link="log"))

############EXTRACT THE OVERDISPERSION PARAMETER, also given when run the model, it is different, but it is recommendend the one given in the model
theta<-glm1a$deviance/glm1a$df.residual
theta
# In the analysis I incorporate the following calculation from Balker 2016, and the overdisppersion number that i gave me
#is the same that the summary of the models gave me too.
dfun<-function(glmQ1q){with(glmQ1a,sum((weights * residuals^2)[weights > 0])/df.residual)}
dfun(glmQ1a)
summary(glmQ1a)    #also give me the overdispersion parameter

#####overdisperssion parameters 
#fusca=1.99
#cerulea=2.36
#peregrina=2.33
#canadensis=1.54
#pitiayumi=1.73
#guira=3.08
#chrysops= na data not overdispersed
###############################################################
##### Model selection using  AICcmodavg
library(AICcmodavg)
###Model used in the thesis
aictab(list(glm1a,glm2a,glm3a,glm4a,glm5a,glm6a,glm7a,glm8a),
       modnames=c("Socialcontext+Dayofseason+Flocksize",
                  "Dayofseason+Flocksize",
                  "Socialcontext+Flocksize",
                  "Socialcontext+dayofseason",
                  "Flocksize",
                  "Dayofseason",
                  "Socialcontext",
                  "Intercept"),
       c.hat=3.08) # need to change for the overdispersion for each species
summary (glmQ5a)


############## END OF SCRIPT ##########################################










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
