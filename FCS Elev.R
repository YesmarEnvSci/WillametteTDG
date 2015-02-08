# Bernard Romey ~ 25April14

dta <- read.csv("TDG.csv")
dt <-na.omit(dta)

require(dplyr)
tdg <- sample_frac(dt,0.075,replace=FALSE)
psat <- tdg %.% select(Date, Q_falls, mWH, Fcs_mean, Mean_ft_msl, TDG_PSat)

fcs <- subset(psat, Fcs_mean > 15)
fcs25 <- subset(fcs, Fcs_mean > 15 & Fcs_mean <= 25, select=c(Q_falls, Fcs_mean, TDG_PSat))
fcs50 <- subset(fcs, Fcs_mean > 25 & Fcs_mean <= 50)
fcs75 <- subset(fcs, Fcs_mean > 50 & Fcs_mean <= 75)
fcs100 <- subset(fcs, Fcs_mean > 75 & Fcs_mean <= 100)
cfs12k <- subset(psat, Q_falls < 15000)


library(ggplot2) 
p25 <- ggplot(fcs25, aes(Q_falls, TDG_PSat))
    p25 + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")

p50 <- ggplot(fcs50, aes(Q_falls, TDG_PSat))
    p50 + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")

p75 <- ggplot(fcs75, aes(Q_falls, TDG_PSat))
    p75 + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")

p100 <- ggplot(fcs100, aes(Q_falls, TDG_PSat))
    p100 + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")

p <- ggplot(fcs, aes(Q_falls, TDG_PSat))
   p + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")

p1 <- ggplot(psat, aes(Q_falls, TDG_PSat))
    p1 + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "springgreen")

cfs12k <- ggplot(cfs12k, aes(Q_falls, TDG_PSat))
    cfs12k + geom_point(aes(colour = Fcs_mean)) + scale_colour_gradient(low = "black", high = "greenyellow")


pp1 <- ggplot(data = psat, aes(Mean_ft_msl, TDG_PSat)) 
    pp1 + stat_smooth(span = 1) + geom_point()
    pp1 + stat_smooth(method = "lm") + geom_point()

# Only TDG for FCS active above 15% inflation
hist2 <- hist(fcs$TDG_PSat, breaks = 75, 
    xlim = c(min(fcs$TDG_PSat), max(fcs$TDG_PSat)), 
    freq = FALSE, 
    col = 'lightblue',
    main = 'FCS Active',
    xlab = 'TDG Saturation')
  lines(density(fcs$TDG_PSat, bw=.75), col = 'red', lwd = 2)

hist(psat$TDG_PSat, breaks = 100)



