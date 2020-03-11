HONEYCOUGH <- read.csv("~/Downloads/HONEYCOUGH.CSV")
View(HONEYCOUGH)

honey <- c(HONEYCOUGH$Honey)
md <- c(HONEYCOUGH$DM)
control <- c(HONEYCOUGH$Control)
score <- c(HONEYCOUGH$ImproveScore)
treatment <- c(HONEYCOUGH$Treatment)

summary(control)
summary(honey)
summary(md)