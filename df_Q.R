## Parsing/querying data in DF

dt <- read.csv("tdg_wf.csv")
View(dt)
str(dt)
head(dt)

dt$fcs <- with(dta,ifelse(dta$Mean_FCS >= 10, "on",# Flow constrol structure active
                           ifelse(dta$Mean_FCS < 10, "off", "")))
dt$fcs <- as.factor(dta$fcs)


## query data

dt[dt$FlashBoards == "N_FB",]#Only data with No flashboards
dt[dt$Mean_FCS >= 50,]# Only data with FCS above 50% inflated
dt[dt$Mean_FCS > 25 & dt$Mean_FCS <=100,]
ncol(dt)# How many colums?
nrow(dt)# How many rows?
dt[dt$Dep_ft > 1, c("RA_Q_Falls","TDG_PSat")] # Only data for Q & TDG when sensor is below 1m

## Only data when sensor is below 1m and FCS is off, or when TDG is > 110
dt[dt$Dep_ft > 1 & dt$Mean_FCS < 10 | dt$TDG_PSat >=110 & dt$FlashBoards == "N_FB",] 

## Median TDG by month for each year
tg_m <- with(dta,
     {as.data.frame(t(tapply(TDG_PSat,list(yr,mo),median)))
       })

aggregate(cbind(TDG_PSat,Q)~ FB+fcs,dta,median)
    