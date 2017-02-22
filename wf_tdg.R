# Water quality and Total Dissolved Gas (TDG); Willamette Falls Sullivan Flow Control Structure

##--- Data ----
op <- par(mfrow=c(1,1))
data <- read.csv("tdg_wf.csv")
dta <-data[,c("PST","FlashBoards","SALEM_FLOW", "RA_Q_Falls","BELOW_FALLS_STAGE", "MWH","Mean_FCS","Bay.1","Bay.2","Bay.3","Temp_C","BP_mmHg","TDG_PSat")]
names(dta) <- c("PST","FB","Q_s", "Q_f","tide", "MWH","Mean_FCS","b1","b2","b3","Temp_C","BP_mmHg","TDG_PSat")
rm(data)
summary(dta)
apply(apply(dta,2,is.na),2,sum)# How many missing values from each variable?

## FCS on or off
dta$fcs <- as.factor(with(dta,ifelse(dta$Mean_FCS >= 15, "on",
                       ifelse(dta$Mean_FCS < 15, "off", ""))))

## Create date data; R book pg. 103
dta$date <- as.Date(dta$PST, "%m/%d/%Y %H:%M")
     dta$yr <- strftime(dta$date,"%y")
     dta$mo <- strftime(dta$date,"%m")
     dta$wk <- strftime(dta$date,"%W")
     dta$day <- strftime(dta$date,"%d")

tdgas <- na.omit(dta)# data with no NA
## Daily median by month and year
tdg_med <- with(tdgas,aggregate(tdgas[,c(3:13)],by=list(date=date,day=day,month=mo,year=yr),FUN=median))
tdg_med$fcs <- as.factor(with(dta,ifelse(tdg_med$Mean_FCS >= 15, "on",
                           ifelse(tdg_med$Mean_FCS < 15, "off", ""))))

#---- Table summary ----
## Median TDG by month for each year
(tg_m <- with(dta,
             {as.data.frame(t(tapply(TDG_PSat,list(yr,mo),median)))
             }))
(tg_wk <- with(dta,
              {as.data.frame(t(tapply(TDG_PSat,list(yr,wk),median)))
              }))
with(tg_wk,#weekly median
     {plot(`08`,type="l",ylim = c(95,125))
       par(new=T)
       plot(`09`,type="l", col="red",ylim = c(95,125))
       par(new=T)
       plot(`10`,type="l", col="blue",ylim = c(95,125))
       abline(h=110, col="grey", lty="dotted")
     })

##---- Color palette ----
library(RColorBrewer) # R book, page 912
### The sequential palettes names are: 
## Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds
## YlGn YlGnBu YlOrBr YlOrRd
## The diverging palettes are:
## BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
par(mfrow=c(2,3))
par(mar=c(1,1,1,1))
rpal <- brewer.pal(8,"Reds")
pie(rep(1,8), col=rpal, radius=0.9,main="Reds")
bpal <- brewer.pal(8,"Blues")
pie(rep(1,8), col=bpal, radius=0.9,main="Blues")
gpal <- brewer.pal(8,"Greens")
pie(rep(1,8), col=gpal, radius=0.9,main="Greens")
grpal <- brewer.pal(8,"Greys")
pie(rep(1,8), col=grpal, radius=0.9,main="Greys")
apal <- brewer.pal(8,"Accent")
pie(rep(1,8), col=apal, radius=0.9,main="Accent")
ygbpal <- brewer.pal(8,"YlGnBu")
pie(rep(1,8), col=ygbpal, radius=0.9,main="YlGnBu")
par(op)
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
npal <- brewer.pal(8,"BrBG")
pie(rep(1,8), col=npal, radius=0.9,main="BrBG")
rbpal <- brewer.pal(8,"RdBu")
pie(rep(1,8), col=rbpal, radius=0.9,main="RdBu")
rgpal <- brewer.pal(8,"RdGy")
pie(rep(1,8), col=rgpal, radius=0.9,main="RdGy")
sppal <- brewer.pal(8,"Spectral")
pie(rep(1,8), col=sppal, radius=0.9,main="Spectral")
par(op)

## ---- Median summary table ----
(st <- aggregate(round(cbind(TDG_PSat,Q_f,b1,b2,b3,Mean_FCS),2)~ FB+fcs,dta,median))
with(st,# In order with flashboards, then fcs
     {st[order(FB,fcs),]})

#----Analysis/Plots ----
with(tdgas,# Boxplots
     {par(mfrow=c(1,2))
      boxplot(TDG_PSat~FB, main="Flashboards")
      boxplot(TDG_PSat~fcs, main="FCS")
})
par(op)
with(tdgas,# TDG vs Q, given flashboards in/out or during install
     {plot(TDG_PSat~Q_s, col=c("grey","black","darkgreen")[FB], cex=.5,xlab="Flow (cms)",ylab="TDG (% Saturation)",
           ylim=range(TDG_PSat),xlim=range(Q_s))
       title(main="Flashboards in/out/install")
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
      (mod <- nls(TDG_PSat~a*Q^b, dta, start=list(a=78, b=.04)))
      #abline(110,0, lty=2, col='grey60')#1:1 line
      #par(new=T)
      #curve(78.41*x^0.03472, xlim=range(Q), ylim=range(TDG_PSat),
      #      col="black",lwd= 2, xlab = "", ylab = "")
      
})
with(tdgas,# TDG vs Q, given FCS % Inflated, with FB installed
     {col.grad <- function(x, colors=gpal[1:8], colsteps=100) {# Creates color gradient function
       return( colorRampPalette(colors) (colsteps) [findInterval(x, seq(min(x),max(x), length.out=colsteps))])
     }
       plot(TDG_PSat[FB=="FB"]~Q_s[FB=="FB"], col=col.grad(Mean_FCS), pch=16,cex=1.25,xlab="Flow (cms)",ylab="TDG (% Saturation)",
           ylim=range(TDG_PSat[FB=="FB"]),xlim=range(Q_s[FB=="FB"]))
       title(main="Flashboards in")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')#1:1 line
         })
with(tdgas,# TDG vs Q, given FB out & MWH
     {col.grad <- function(x, colors=rbpal[2:8], colsteps=100) {# Creates color gradient function
       return( colorRampPalette(colors) (colsteps) [findInterval(x, seq(min(x),max(x), length.out=colsteps))])
     }
     plot(TDG_PSat[FB=="N_FB"]~Q_s[FB=="N_FB"], col=col.grad(MWH), pch=16,cex=0.75,xlab="Flow (cms)",ylab="TDG (% Saturation)",
          ylim=range(TDG_PSat[FB=="N_FB"]),xlim=range(Q_s[FB=="N_FB"]))
     title(main="TDG vs Q, given FB out & MWH")
     grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
          lwd = par("lwd"), equilogs = TRUE) 
     abline(110,0, lty=2, col='grey60')#1:1 line
     })
with(tdgas,# TDG vs Q, given H2O T with FCS on
     {col.grad <- function(x, colors=rbpal[1:8], colsteps=100) {# Creates color gradient function
       return( colorRampPalette(colors) (colsteps)[findInterval(x, seq(min(x),max(x), length.out=colsteps))])
     }
     plot(TDG_PSat[fcs=="on"]~Q_s[fcs=="on"], col=col.grad(Temp_C), pch=16,cex=1,xlab="Flow (cms)",ylab="TDG (% Saturation)",
          ylim=range(TDG_PSat[fcs=="on"]),xlim=range(Q_s[fcs=="on"]))
     title(main="TDG vs Q, given H2O temp (C) with FCS ON")
     grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
          lwd = par("lwd"), equilogs = TRUE) 
     legend("topright",legend=c("warm","med","cold"),pch=16,col=rbpal[c(1,4,8)],bg="grey95",inset=.02, adj=.15)
     abline(110,0, lty=2, col='grey60')#1:1 line
     })
with(tdgas,# FCS on/off
     {par(mfrow=c(1,1))
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=c(.75,.5)[fcs],pch=c(1,16)[fcs],col=c("grey","darkgreen")[fcs],
           ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS on/off")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
})

with(tdgas,# FCS % inflated
     {par(mfrow=c(2,2))
       col.grad <- function(x, colors=bpal[2:8], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q_s,cex=0.5,pch=1,col=col.grad(Mean_FCS),ylab="TDG (% Saturation)",xlab="Flow (cms)",# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdgas,# FCS Bay 1, % inflated
     {col.grad <- function(x, colors=bpal[2:8], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b1),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS Bay 1, % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdgas,# FCS Bay 2, % inflated
     {col.grad <- function(x, colors=bpal[2:8], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b2),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS Bay 2, % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdgas,# FCS Bay 3, % inflated
     {col.grad <- function(x, colors=bpal[2:8], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b3),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS Bay 3, % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
par(op)
with(tdg_med,# TDG vs Q, given MWH; daily median
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=c("black","yellow","red"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=1.5,pch=16,col=col.grad(MWH),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q_s), main="MWH")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdg_med,# TDG vs H2O T(C), given Q(cms
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=gpal[8:1], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Temp_C,ylab="TDG (% Saturation)",xlab="H2O temp (C)",cex=1.25,pch=16,col=col.grad(Q_f),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Temp_C), main="TDG vs H2O T(C), given Q(cms)")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdg_med,# TDG vs tide, given % FCS
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=rgpal[8:1], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~tide,ylab="TDG (% Saturation)",xlab="H2O level below falls (ft)",cex=1.25,pch=16,col=col.grad(Mean_FCS),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(tide), main="TDG vs tide, given % FCS")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
       legend("topleft",legend=c("down","med","up"),cex=.75,pt.cex=1.5,pch=21,col="grey",pt.bg=rgpal[c(8,5,2)],
              bg="white",adj = .15,inset=.025)
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })
with(tdg_med,# TDG vs Q, given % FCS; using daily median 
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=bpal[2:8], colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       colfunc <- colorRampPalette(c("grey", "darkgreen"))
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",pch=21,col="grey",bg=col.grad(Mean_FCS),cex=1.5,
            ylim=range(TDG_PSat),xlim=range(Q_s), main="Daily median FCS % inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
       legend("topleft",legend=c("down","med","up"),cex=.75,pt.cex=1.5,pch=21,col="grey",pt.bg=bpal[c(2,5,8)],fill=bpal[c(2,5,8)],
              bg="white",adj = .15,inset=.025)  
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
})# spike at 15k cms is most likely from lowering FCS to save FB
with(tdg_med,# TDG vs Q, given on/off FCS; using daily median 
     {par(mfrow=c(1,1))
       plot(TDG_PSat~Q_s,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=c(.75,.5)[fcs],pch=c(1,16)[fcs],col=c("grey","darkgreen")[fcs],
            ylim=range(TDG_PSat),xlim=range(Q_s), main="FCS on/off")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })


round(cor(tdg_med[,c(4:14)]),2)#correlation matrix
round(cov(tdg_med[c(4:14)]),2)# var-cov matrix

source("cor.matrix.r")
library(dplyr)
tdg <- sample_frac(tdgas,0.10,replace=FALSE)# Uses the dplyr package to sample a fraction of the original data
detach("package:dplyr", unload=TRUE)
cor.matrix(tdg_med[,c(4:14)])  # TDG is primarily correlated with Q, tide, FCS bay 1,2,3, & h2O temp C.
#cor.matrix(log(tdg_med[,c(4:14)]+1))

with(tdg_med,# Response variable on Y-axis, explanatory variable on X-axis, given 2nd explanatory variable FCS
     {coplot(TDG_PSat~Q_f|Mean_FCS, panel = panel.smooth)})# Plots lower left to upper right

##----- Random forest model -----
library(randomForest)
dt <- tdg[3:13]
(10/3)# number of 'mtry' regression predictors to use for each random forest
train <- sample(1:nrow(dt),nrow(dt)/2)# use subset to train, used for node purity
set.seed(167)
(rf <- randomForest(dt$TDG_PSat ~.,data = dt, subset = train,mtry=6, importance=T, corr.bias=F,proximity=F, 
                        do.trace=1000, ntree=3000))# view results
plot(rf)
round(mean(rf$rsq),3)#Pseudo R-sqr.
## The %IncMSE is the variance of the predictor, IncNode Purity is the RSS for the predictor
## Variable is important if > the absolute value of lowest predictor importance value (%IncMSE for regression, MDA for classification)
varImpPlot(rf,sort=T,n.var=min(10,nrow(rf$importance)))
rf$importance

partialPlot(rf,dt,b1)
partialPlot(rf,dt,cms)
partialPlot(rf,dt,tide)
partialPlot(rf,dt,Temp_C)
partialPlot(rf,dt,MWH)


## ----- Histogram fancy plot -----
with(tdg,# FCS on/off frequency distribution (histogram)
     {b.width <- function(x,y){# Rule of thumb function for bandwidth (pg 226 in R)
       fcs <- table(fcs)
       n=fcs[y]#1=on, 2=off
       return(b <- (max(x)-min(x))/(2*(1+log2(n))))
       }
       hist(TDG_PSat[fcs=="on"], prob=T,breaks=50,col=rgb(0,1,0,1/2),xlab="",main="",
            ylim=c(0,.25),xlim = range(TDG_PSat))
       lines(density(TDG_PSat[fcs=="on"], width=b.width(TDG_PSat,1), n=500), col="darkgreen", lwd=2)
       hist(TDG_PSat[fcs=="off"], prob=T, xlab="",add=T, breaks=50,col=rgb(0,1/4,1,1/4),
            ylim = c(0,.25), xlim = range(TDG_PSat))
       lines(density(TDG_PSat[fcs=="off"], width=b.width(TDG_PSat,2), n=500), col="blue", lwd=2)
       title(main="FCS on (green), off (blue)", xlab = "TDG (% Saturation)")
       abline(v=110,col="grey",lty="dotted")
       })
## The secondary mode when FCS is on prob due to flows increasing and FCS turning off leading to elevated TDG


with(tdg,
     {boxplot(TDG_PSat~fcs)
     })
b1 <- tdg$b1
b2 <- tdg$b2
b3 <- tdg$b3
tg <- tdg$TDG_PSat
bay <- as.data.frame(cbind(b1,b2,b3,tg))
library(tidyr)
b <- tidyr::gather(bay,"bay","perc",1:3)
with(b, # percent inflated
     {boxplot(perc~bay, col="grey")
       title(main = "FCS", ylab = "% inflated")
     })

dta[dta$b1>95,]

#---- Variable normality test ----
lshap <- lapply(tdg[3:9], shapiro.test) #shapiro test on data
t(lres <- sapply(lshap, `[`, c("statistic","p.value")))

lshap <- lapply(log(tdg[3:9]+1), shapiro.test) #shapiro test on log transformed data
t(lres <- sapply(lshap, `[`, c("statistic","p.value")))

#---- Time series ----
par(mfrow=c(2,1))
with(dta,# Time series plot of TDG
     {ts.plot(TDG_PSat)
})
with(tdg_med,# Time series plot of TDG
     {ts.plot(TDG_PSat)
     })
par(op)

ma3 <- function (x) {
  y <- numeric(length(x)-2)
  for (i in 2: (length(x)-1)) {
    y[i] <- (x[i-1]+x[i]+x[i+1])/3
  }
y }# compures the 3-point moving average; smothes out the line
tg <- ma3(tdg_med$TDG_PSat)# applies ma3 fuction to 
plot(tg,ylim=c(90,130),pch=".")
lines(tg[2:39259],col="red")

##---- Seasonal data analysis -----
## reference R book page 794
index <- 1:839
839/3
time <- index/280

(mod <- lm(tdg_med$TDG_PSat~sin(time*2*pi)+cos(time*2*pi)))
summary(mod)
plot(tdg_med$date, tdg_med$TDG_PSat, pch=".")
lines(time, predict(mod),col="red",lwd=2)
plot(mod$residuals,pch=".")

windows(7,4)
par(mfrow=c(1,2))
acf(mod$resid,main=".")
acf(mod$resid,type="p",main="")
## There is a very strong residual correlation in the residuals.
## The partial autocorrelation is a lag of 2.  This suggest that an AR(2) model (autoregressive model
## with order (2)) might be appropriate. Joke: what will TDG be like tomorrow? ...'Like today'

## Seasonal component using loess
(length(tdg_med$TDG_PSat))
(902/365)
g <- ts(tdg_med$TDG_PSat,start=c(2008,2),frequency=365) 
plot(g)# start year and month, frequency is # datapoints/yr =365 days
g1 <- stl(g,"periodic")
plot(g1)#The trend is for the seanon, not all years
names(g1)
summary(g1)

library(nlme)
(mod.g <- gls(TDG_PSat~Q_f+Mean_FCS, correlation=corARMA(p=3),method="ML",data = tdg_med))
?corClasses
# p =  # of sig lags. ML= Maximum likelihood. correlation specifies autocorrelation
summary(mod.g) # TDG = 107.98+0.00015(Q)-0.041(ave fcs)
plot(mod.g)
acf(mod.g$resid,type="p",main="")# there is a sig lag of 6
par(op)

library(lme4)
index <- 1:902
time <- index/365#days/days in year for observations per year
tdg_med$year <- as.factor(tdg_med$year)
## covariate is flow (Q_f) and grouping factor is fcs
mod1 <- lmer(TDG_PSat~index+sin(time*2*pi)+cos(time*2*pi)+(1|factor(year)),REML = "ML",data=tdg_med)
mod2 <- lme(TDG_PSat~sin(time*2*pi)+cos(time*2*pi)+(1|factor(as.factor(year))),REML = "ML",data=tdg_med)
ampva(mod1,mod2)# if p > 0.05 then trend is not sig

spectrum(tdg_med$TDG_PSat,main="",col="darkgreen") #Spectral analysis, pg 800, 95% CI is vertical blue bar
abline(h=11,col="grey",lty=3)
## 

#---- Multiple time series
## There is some evidence of periodicity is TDG
plot.ts(cbind(tdg_med$TDG_PSat,tdg_med$Q_s,tdg_med$Temp_C,main=""))



