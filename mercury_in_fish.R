# STA305 | Professor Luai Al Labadi | Final Project
# BY: Tahir Muhammad | 1002537613 & Saurabh Sant | 1002434047

######################
######################
#### Case Study 1 ####
######################
######################

#########################################
######        Course : STA305      ######
###### Final Project : Question (a)######
###### Muhammad Tahir | 1002537613 ######
######  Sant Saurabh | 1002434047  ######
#########################################
#Loading the data
fishdata <- read.csv("Downloads/project_data_mercuryinfish.csv", na="NA")

y <- fishdata$Mercury
type <- fishdata$Type
dam <- fishdata$Dam

#loading the data into a dataframe
dat <- data.frame(y=y, type=type, dam =dam)

#removing any missing data
dat <- dat[-which(dat$type=="."),]
dat <- dat[-which(dat$dam=="."),]

# Look at the data boxplot
boxplot(fishdata$Mercury, main = "Boxplot of the Mercury levels in the lakes" )

#Remove outliers
#We will only keep values between the 25%-1.5*IQR and 75%+1.5*IQR range
#IQR = Q3 - Q1
Q <- quantile(dat$y, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(dat$y)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range

#Delete values greater than 1.29375 so we just delete one row with mercury level of 2.5
dat <- dat[-which(dat$y>up),]
attach(dat)

#Basic descriptive statistics
View(dat)
summary(dat$y)
summary(dat$type)
summary(dat$dam)

summary(dat$type[dat$dam==1])
summary(dat$type[dat$dam==0])

par(mfrow=c(1,2))
hist(dat$y, xlab = "Mercury Levels", main = "Histogram of Mercury Levels", col="grey")
qqnorm(dat$y, main=" Normal Q-Q Plot of Mercury Levels")

# All lakes 
length(dat$y)
mean(dat$y)
sd(dat$y)
summary(dat$y)

# Lake with dams, and without
library(dplyr)
DataWithDam <- filter(dat, dat$dam==1)
length(DataWithDam$y)
mean(DataWithDam$y)
sd(DataWithDam$y)
summary(DataWithDam$y)

# Without dams
DataWithoutDam <- subset(dat, dat$dam==0)
length(DataWithoutDam$y)
mean(DataWithoutDam$y)
sd(DataWithoutDam$y)
summary(DataWithoutDam$y)

par(mfrow=c(2,3))
hist(dat$y, main = " Normal Q-Q Plot for All Lakes", col="red", xlab= "Mercury level")
hist(DataWithDam$y, main = " Normal Q-Q Plot for Lakes with Dam", col = "blue" ,  xlab= "Mercury level")
hist(DataWithoutDam$y, main = " Normal Q-Q Plot for Lakes without Dam", col = "green",  xlab= "Mercury level")
# QQ plots
qqnorm(dat$y, main = " Normal Q-Q Plot for All Lakes")
qqnorm(DataWithDam$y, main = " Normal Q-Q Plot for Lakes with Dam")
qqnorm(DataWithoutDam$y , main = " Normal Q-Q Plot for Lakes without Dam")


# Exploring the data by type.
# Type 1
DataWithType1 <- filter(dat, dat$type==1)
length(DataWithType1$y)
mean(DataWithType1$y)
sd(DataWithType1$y)
summary(DataWithType1$y)

# Type 2
DataWithType2 <- filter(dat, dat$type==2)
length(DataWithType2$y)
mean(DataWithType2$y)
sd(DataWithType2$y)
summary(DataWithType2$y)

# Type 3 
DataWithType3 <- filter(dat, dat$type==3)
length(DataWithType3$y)
mean(DataWithType3$y)
sd(DataWithType3$y)
summary(DataWithType3$y)

hist(DataWithType1$y)
hist(DataWithType2$y)
hist(DataWithType3$y)

###################################
##### Question of interest(c) #####
###################################
# confidence interval
upper_cl <- mean(dat$y) + qnorm(0.975)*sd(dat$y)
lower_cl <- mean(dat$y) - qnorm(0.975)*sd(dat$y)
print(c(lower_cl, upper_cl))









###################################
##### Question of interest(d) #####
###################################

#Using one-way ANOVA to test the treatment effect of having a dam near a lake on mercury level
with(dat, tapply(dat$y, dat$dam, mean))
contrasts(dat$dam)34

fish.regmodel <- lm(dat$y~dat$dam, dat)
summary(fish.regmodel)

#should get same values since the anova is regression model with categorical variables
anova(fish.regmodel)


###################################
##### Question of interest(e) #####
###################################

#ANOVA with completely randomized design
model3 <- lm(dat$y ~ dat$type, data=dat)
anova(model3)





###################################
##### Question of interest(f) #####
###################################

fish <- within(dat, {x <- paste(dat$dam, dat$type )})
with(dat, tapply(dat$y, x, mean))

boxplot(dat$y~x, data=fish)

# run two-way ANOVA
model1 <- lm(dat$y~type*dam, data = dat)
summary(model1)
anova(model1)

###################################
##### Question of interest(g) #####
###################################

#ANOVA using dam as a blocking factor
modelblock <- lm(dat$y ~ dat$type + dat$dam, data=dat)
anova(modelblock)
