
dta <-read.csv("TDG.csv")
dt <-na.omit(dta)
rm(dta)
dt<-dt[,-c(1,3,12)]
boxplot(dt)
boxplot(scale(dt))

str(dt)
summary(dt)

require(dplyr)
smpl <- sample_frac(dt,0.5,replace=FALSE)

cor.matrix(smpl)

lg <-(log(smpl+1))
str(lg)
boxplot(scale(lg))

cor.matrix(lg)

library(MASS)
pca <-princomp(scale(lg))
summary(pca)
biplot(pca)
scores(lg)
