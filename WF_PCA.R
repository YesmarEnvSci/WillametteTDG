
dta <-read.csv("TDG.csv")
dt <-na.omit(dta)
rm(dta)
dt<-dt[,-c(1,3,12)]
boxplot(dt)
boxplot(scale(dt))

require(dplyr)
tdg <- sample_frac(dt,0.10,replace=FALSE)

cor.matrix(tdg) #regression correlatin matrix
cor.matrix(log((tdg+1)))

cor(tdg)#covariance matrix
cor()

library(MASS)
pca <-princomp(scale(log(tdg+1)))
summary(pca) #Eigenvalues
broken.stick(9)
biplot(pca)

pca$scores #PC Matrix
loadings(pca) #Check eigenvectors: How closely variables are related to components; 
# Principal component loading (pg 50)





