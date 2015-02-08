#Principal Component Analysis for variables associated with Willamette Falls TDG

dta <-read.csv("TDG.csv")
dt <-na.omit(dta)
rm(dta)
dt<-dt[,-c(1,3,12)]

require(dplyr)
tdg <- sample_frac(dt,0.075,replace=FALSE)
detach("package:dplyr", unload=TRUE)
rm(dt)  # distributions are not normal

boxplot(tdg)
boxplot(scale(tdg))
boxplot(scale(log(tdg+1)))

lshap <- lapply(tdg, shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

lshap <- lapply(log(tdg+1), shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

cor.matrix(tdg) #regression correlatin matrix
cor.matrix(log((tdg+1)))

cor(tdg)#covariance matrix

library(MASS)
pca <-princomp(scale(log(tdg+1)))
summary(pca) #Eigenvalues
broken.stick(9)
biplot(pca)

pca$scores #PC Matrix
loadings(pca) #Check eigenvectors: How closely variables are related to components; 
# Principal component loading (pg 50)





