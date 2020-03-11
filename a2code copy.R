# Question 1.2 Boxplot
y = c(7,7,15,11,9,12,17,12,18,18,14,19,19,18,18,19,25,22,19,23,7,10,11,15,11)
a <- factor(rep(c("15% Cotton", "20% Cotton", "25% Cotton", "30% Cotton","35% Cotton"), each = 5))
dat <- data.frame(x=a, y=y)
boxplot(y~a, xlab = "", ylab = "", data= dat)

# Question 1.3
with(dat, tapply(y, a, mean))
with(dat, tapply(y, a, var))
mean(y)
var(y)

# Question 1.4
anv <- lm(y~a, data = dat)
anova(anv)

# Question 1.6
fit <- lm(y~ x-1, data=dat)
K <- matrix(c(1, -1/4, -1/4, -1/4, 0, 1, -1/3, -1/3,-1/3, 0, 0 ,1, -1/2, -1/2, 0 , 0, 0, 1, -1), byrow=T, nrow=4)
#install.packages("multcomp")
#library(multcomp)
summary(glht(fit, K), test = adjusted("none"))

#Question 1.7
SS1 = (-6.550)^2/0.25
SS2 = (-1.267)^2/(4/15)
SS3 = (1.4^2)/(0.3)
SS4 = (10.8^2)/(0.4)

SST = sum(SS1, SS2, SS3, SS4)

# Question 1.8
cu = -6.55 + qt(0.975, 20)*1.42
cl = -6.55 - qt(0.975, 20)*1.42
cu; cl

# Question 1.9
L = matrix(c(1, -1/4, -1/4, -1/4, 0, 1, -1/3, -1/3, -1/3), byrow=T, nrow=2)
summary(glht(fit,L), test = adjusted("bonferroni"))

# Question 1.10
TukeyHSD(aov(lm(y~x, data=dat)), conf.level = 0.95)

clTukey= TukeyHSD(aov(lm(y~x, data=dat)), factor=a, conf.level = 0.95)
plot(clTukey)