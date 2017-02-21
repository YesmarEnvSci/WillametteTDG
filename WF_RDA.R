#Redundancy Analysis (RDA) for water quality and FCS variables with Willamette Falls TDG

##--- Data ----
data <- read.csv("tdg_wf.csv")
dta <-data[,c("PST","FlashBoards","RA_Q_Falls","MWH","Mean_FCS","Bay.1","Bay.2","Bay.3","Temp_C","BP_mmHg","TDG_PSat")]
names(dta)[3] <- "Q"
names(dta)[2] <- "FB"
names(dta)[6:8] <- c("b1","b2","b3")
rm(data)
summary(dta)

dta$fcs <- with(dta,ifelse(dta$Mean_FCS >= 10, "on",# Flow constrol structure active
                       ifelse(dta$Mean_FCS < 10, "off", "")))
dta$fcs <- as.factor(dta$fcs)

dta$date <- as.Date(dta$PST, "%m/%d/%Y %H:%M")
dta$yr <- strftime(dta$date,"%y")
dta$mo <- strftime(dta$date,"%m")
dta$day <- strftime(dta$date,"%d")
#dta$mo2 <- strftime(dta$date,"%B")

tdgas <- na.omit(dta)
## Daily median by month and year
(tdg_med <- with(tdgas,aggregate(tdgas[,c(3:11)],by=list(day=day,month=mo,year=yr),FUN=median)))

## Median TDG by month for each year
tg_m <- with(dta,
             {as.data.frame(t(tapply(TDG_PSat,list(yr,mo),median)))
             })

##Median summary table
(st <- aggregate(round(cbind(TDG_PSat,Q,b1,b2,b3,Mean_FCS),2)~ FB+fcs,dta,median))
with(st,# In order with flashboards, then fcs
     {st[order(FB,fcs),]})

#----Analysis/Plots ----
with(tdgas,# Boxplots
     {par(mfrow=c(1,2))
      boxplot(TDG_PSat~FB, main="Flashboards")
      boxplot(TDG_PSat~fcs, main="FCS")
})
par(mfrow=c(1,1))
with(tdgas,# Flashboards in/out or during install
     {plot(TDG_PSat~Q, col=c("grey","black","darkgreen")[FB], cex=.5,xlab="Flow (cms)",ylab="TDG (% Saturation)",
           ylim=range(TDG_PSat),xlim=range(Q))
       title(main="Flashboards in/out/install")
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
      #(mod <- nls(TDG_PSat~a*Q^b, dta, start=list(a=78, b=.04)))
      abline(110,0, lty=2, col='grey60')#1:1 line
      #par(new=T)
      #curve(78.41*x^0.03472, xlim=range(Q), ylim=range(TDG_PSat),
      #      col="black",lwd= 2, xlab = "", ylab = "")
      
})

with(tdgas,# Flashboards in
     {col.grad <- function(x, colors=c("black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
       return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
     }
       plot(TDG_PSat[FB=="FB"]~Q[FB=="FB"], col=col.grad(Mean_FCS), pch=16,cex=1.25,xlab="Flow (cms)",ylab="TDG (% Saturation)",
           ylim=range(TDG_PSat[FB=="FB"]),xlim=range(Q[FB=="FB"]))
       title(main="Flashboards in")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       #(mod <- nls(TDG_PSat~a*Q^b, dta, start=list(a=78, b=.04)))
       abline(110,0, lty=2, col='grey60')#1:1 line
       #par(new=T)
       #curve(78.41*x^0.03472, xlim=range(Q), ylim=range(TDG_PSat),
       #      col="black",lwd= 2, xlab = "", ylab = "")
       
     })

with(tdgas,# FCS on
     {col.grad <- function(x, colors=c("Black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
       return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
     }
     plot(TDG_PSat[fcs=="on"]~Q[fcs=="on"], col=col.grad(Temp_C), pch=16,cex=1,xlab="Flow (cms)",ylab="TDG (% Saturation)",
          ylim=range(TDG_PSat[fcs=="on"]),xlim=range(Q[fcs=="on"]))
     title(main="FCS ON")
     grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
          lwd = par("lwd"), equilogs = TRUE) 
     #(mod <- nls(TDG_PSat~a*Q^b, dta, start=list(a=78, b=.04)))
     abline(110,0, lty=2, col='grey60')#1:1 line
     #par(new=T)
     #curve(78.41*x^0.03472, xlim=range(Q), ylim=range(TDG_PSat),
     #      col="black",lwd= 2, xlab = "", ylab = "")
     
     })

with(tdgas,# FCS on/off
     {par(mfrow=c(1,1))
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=c(.75,.3)[fcs],pch=c(1,16)[fcs],col=c("grey","darkgreen")[fcs],
           ylim=range(TDG_PSat),xlim=range(Q), main="FCS on/off")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
})

with(tdgas,# FCS % inflated
     {par(mfrow=c(2,2))
       col.grad <- function(x, colors=c("black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,cex=0.5,pch=1,col=col.grad(Mean_FCS),ylab="TDG (% Saturation)",xlab="Flow (cms)",# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="FCS % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdgas,# FCS Bay 1 % inflated
     {col.grad <- function(x, colors=c("black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b1),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="FCS Bay 1 % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdgas,# FCS Bay 2 % inflated
     {col.grad <- function(x, colors=c("black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b2),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="FCS Bay 2 % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdgas,# FCS Bay 3 % inflated
     {col.grad <- function(x, colors=c("black","grey","darkgreen"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=.5,pch=1,col=col.grad(b3),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="FCS Bay 3 % Inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdg_med,# MWH level
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=c("black","yellow","red"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=1.5,pch=1,col=col.grad(MWH),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="MWH")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdg_med,# Water temperature
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=c("blue","yellow","red"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",cex=1.75,pch=1,col=col.grad(Temp_C),# Apply col.grad function
            ylim=range(TDG_PSat),xlim=range(Q), main="Water temperature (C)")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
     })

with(tdg_med,# Daily median FCS % Inflated; spike at 15k cms is most likely from lowering FCS to save FB
     {par(mfrow=c(1,1))
       col.grad <- function(x, colors=c("grey","darkgreen"), colsteps=100) {# Creates color gradient function
         return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps))])
       }
       colfunc <- colorRampPalette(c("grey", "darkgreen"))
       plot(TDG_PSat~Q,ylab="TDG (% Saturation)",xlab="Flow (cms)",pch=16,cex=1.5,col=col.grad(Mean_FCS),
            ylim=range(TDG_PSat),xlim=range(Q), main="Daily median FCS % inflated")
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE) 
       abline(110,0, lty=2, col='grey60')# Critical TDG saturation for aquatic vertebrate
})


round(cor(tdg[,c(3:5,9:11)]),2)#correlation matrix
round(cov(tdg[c(3:5,9:11)]),2)# var-cov matrix

source("cor.matrix.r")
library(dplyr)
tdg <- sample_frac(tdgas,0.10,replace=FALSE)# Uses the dplyr package to sample a fraction of the original data
detach("package:dplyr", unload=TRUE)
cor.matrix(tdg[,c(3:11)])  
cor.matrix(log(tdg[,c(3:11)]+1))
#boxplot(scale(log(tdg[3:11]+1)))

with(tdg,# FCS on/off frequency distribution
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

#dta$on_off <- as.factor(with(dta,ifelse(b1 > 95, 'on',#
#                                   ifelse(b1 <=95 , 'off',""))))#high


#---- Variable normality test ----
lshap <- lapply(tdg[3:9], shapiro.test) #shapiro test on data
t(lres <- sapply(lshap, `[`, c("statistic","p.value")))

lshap <- lapply(log(tdg[3:9]+1), shapiro.test) #shapiro test on log transformed data
t(lres <- sapply(lshap, `[`, c("statistic","p.value")))

#---- Time series ----
with(dta,# Time series plot of TDG
     {ts.plot(TDG_PSat)
})

ts.plot(tdg_med$TDG_PSat)

ma3 <- function (x) {
  y <- numeric(length(x)-2)
  for (i in 2: (length(x)-1)) {
    y[i] <- (x[i-1]+x[i]+x[i+1])/3
  }
y }#geometric mean

tg <- ma3(dta$TDG_PSat)
plot(tg,ylim=c(90,130),pch=".")
lines(tg[2:39259],col="red")

tg1 <- ma3(tdg_med$TDG_PSat)
plot(tg1,ylim=c(90,130),pch="*")
lines(tg[2:30],col="red")


##---- Seasonal data analysis -----
## reference R book page 794
index <- 1:39259
39259/3
time <- index/13086

(mod <- lm(dta$TDG_PSat~sin(time*2*pi)+cos(time*2*pi)))
summary(mod)
plot(dta$PST, dta$TDG_PSat, pch=".")
lines(time, predict(mod),col="red",lwd=2)
plot(mod$residuals,pch=".")

windows(7,4)
par(mfrow=c(1,2))
acf(mod$resid,main=".")
acf(mod$resid,type="p",main="")
## There is a very strong residual correlation in the residuals, and it does not drop off very fast.
## The partial autocorrelation is large up to a lag of 5.  This suggest that an AR(5) model (autoregressive model
## with order 5) might be appropriate. Joke: what will TDG be like tomorrow? ...'Like today'

g <- ts(dta$TDG_PSat,start=c(2008,2),frequency=800) # start year and month, frequency is # datapoints
plot(g)
g1 <- stl(g,"periodic")
plot(g1)

## Calcualte monthly average and do same 
g <- ts(tdg_med$TDG_PSat,start=c(2008,2),frequency=7) # start year and month, frequency is # datapoints/yr
plot(g)
g1 <- stl(g,"periodic")
plot(g1)

library(nlme)
mod11 <- lme(dta$TDG_PSat~index+sin(time*2*pi)+cos(time*2*pi)+(1|factor(yr)),REML=FALSE)
mod12 <- lme(dta$TDG_PSat~sin(time*2*pi)+cos(time*2*pi)+(1|factor(yr)),REML=FALSE)

spectrum(dta$TDG_PSat,main="",col="darkgreen") #Spectral analysis, pg 800, 95% CI is vertical blue bar
## 

#---- Multiple time series
## There is some evidence of periodicity is TDG
plot.ts(cbind(dta$TDG_PSat,dta$Q,dta$Temp_C, main=""))

par(mfrow=c(3,3))
acf(cbind(tdgas$TDG_PSat,tdgas$Q,tdgas$Temp_C),type="p",col="darkgreen")


