library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(plyr)
library(scales)
library(zoo)
install.packages("anytime")
library(anytime)
library(plyr)
install.packages("xlsx")
library(xlsx)
library(foreign)
install.packages("readstata13")
library(readstata13)
require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)
require(RColorBrewer)
require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)


getwd()
setwd("C:/Users/Eduado Acosta/Documents/datafest 18/data/datafest2018_data_and_documentation/data")

GEOLOCATION <- data.table::fread("geolocation.csv")
CONN <- data.table::fread("conn.csv")


###
countries <- unique(GEOLOCATION$location)
length(countries)
length(unique(GEOLOCATION$id.orig_h))
locFreq <- as.data.frame(table(GEOLOCATION$location))
CONN <- rename(CONN, replace = c("id.orig_h" = "ip"))
CONN <- merge(CONN, GEOLOCATION, by=c("ip"))

longduration <- subset(CONN, duration > 600)
longduration$formatteddate <- anytime(longduration$ts)
longduration$formatteddate <- as.Date(longduration$formatteddate)
longduration$year<-as.numeric(as.POSIXlt(longduration$formatteddate)$year+1900)
longduration$month<-as.numeric(as.POSIXlt(longduration$formatteddate)$mon+1)
longduration$monthf<-factor(longduration$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
longduration$weekday = as.POSIXlt(longduration$formatteddate)$wday
longduration$weekdayf<-factor(longduration$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
longduration$yearmonth<-as.yearmon(longduration$formatteddate)
longduration$yearmonthf<-factor(longduration$yearmonth)
longduration$week <- as.numeric(format(longduration$formatteddate,"%W"))
longduration<-ddply(longduration,.(yearmonthf),transform,monthweek=1+week-min(week))

longduration$totcon <- seq(from= 1, to=1)

longduration$totcon <- as.numeric(longduration$totcon)

write.dta(longduration, "C:/Users/Eduado Acosta/Desktop/Semester 4/Applied Methods in Policy Evaluation/londuration2.dta")

longduration2 <- read.dta13("C:/Users/Eduado Acosta/Documents/datafest 18/data/datafest2018_data_and_documentation/data/longduration2.dta")

P<- ggplot(longduration2, aes(monthweek, weekdayf, fill = totcon)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="yellow", high="red") + xlab("Week of Month") + ylab("")
P


