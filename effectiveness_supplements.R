# STA305 | Professor Luai Al Labadi | Final Project
# BY: Tahir Muhammad | 1002537613 & Saurabh Sant | 1002434047

######################
######################
#### Case Study 2 ####
######################
######################

#Loading the data
y <- c(48, 67, 78, 69, 53, 65, 49, 37, 73, 63, 79, 52, 63, 65, 67, 59, 50, 59, 42, 34)
a <- factor(rep(c("Supplement A", "Supplement B","Supplement C","Supplement D" ), each = 5))
x <- c(35, 44, 44, 51, 47, 40, 45, 37, 53, 42, 51,  41, 47, 47, 48, 53, 52, 52, 51, 43)

supdata <- data.frame(y=y, sup = a, x=x)
attach(supdata)

#########################################
######        Course : STA305      ######
###### Final Project : Question (f)######
###### Muhammad Tahir | 1002537613 ######
######  Sant Saurabh | 1002434047  ######
#########################################
model_test <- lm(supdata$y~supdata$sup + supdata$x, data = supdata)

par(mfrow=c(2, 2))
plot( model_test , main="Diagnostic plots for ANCOVA" )

library(car)
durbinWatsonTest(model_test)

plot(supdata$x,supdata$y, xlab = "Calories intake/10", ylab = "Weight gain in grams")

library(ggplot2)
require(ggplot2)

print(c("Correlation between weight gain and calorie intake", cor(supdata$x,supdata$y)))

qplot(supdata$x, supdata$y, data=supdata, colour=supdata$sup) + labs(title = "Calories Consumed and Weight Gain based
on Vitamin", x = "Calories consumed/10", y = "Weight gain in grams") + geom_smooth(method="lm", se=F)

model3 <- lm(supdata$y~ supdata$x*supdata$sup, data = supdata)
anova(model3)

#########################################
######        Course : STA305      ######
###### Final Project : Question (g)######
###### Muhammad Tahir | 1002537613 ######
######  Sant Saurabh | 1002434047  ######
#########################################



#Using one-way ANOVA to test the significance of the effect of supplements on weight gain
model1 <- lm(supdata$y~supdata$sup, data = supdata)
anova(model1)

###########################################
######        Course : STA305      ########
###### Final Project : Question (h, i)#####
###### Muhammad Tahir | 1002537613 ########
######  Sant Saurabh | 1002434047  ########
###########################################

#Using ANCOVA to test the significance of the effect of supplements on weight gain
#using calorie intake as a covariate

model2 <- lm(supdata$y~ supdata$x + supdata$sup, data = supdata)
anova(model2)
summary(model2)


#########################################
######        Course : STA305      ######
###### Final Project : Question (j)######
###### Muhammad Tahir | 1002537613 ######
######  Sant Saurabh | 1002434047  ######
#########################################

model3 <- lm(supdata$y~ supdata$x*supdata$sup, data = supdata)
anova(model3)
summary(model3)