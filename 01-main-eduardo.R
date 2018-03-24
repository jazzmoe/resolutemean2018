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


rm(diamonds2)

getwd()
setwd("C:/Users/Eduado Acosta/Documents/datafest 18/data/datafest2018_data_and_documentation/data")

GEOLOCATION <- data.table::fread("geolocation.csv")
CONN <- data.table::fread("conn.csv")

# hist of durations
conn$duration2 <- as.numeric(conn$duration)
ggplot(conn, aes(duration2)) +
  geom_histogram(binwidth = 30)
conn <- order(conn$duration2)
summary(conn$duration2)

# suspicipicious ip
IPsuspicious <- conn[conn$id.orig_h=="213.108.73.187"]
View(IPsuspicious)

###
countries <- unique(GEOLOCATION$location)
length(countries)
length(unique(GEOLOCATION$id.orig_h))
locFreq <- as.data.frame(table(GEOLOCATION$location))
CONN <- rename(CONN, replace = c("id.orig_h" = "ip"))
CONN <- merge(CONN, GEOLOCATION, by=c("ip"))

longduration <- subset(CONN, duration > 600 )

longduration$formatteddate <- anytime(longduration$ts)
longduration$formatteddate <- as.Date(longduration$formatteddate)

longduration$datecount <- as.character(longduration$formatteddate)
longduration$year <- substr(longduration$formatteddate,1,4)

longduration$yearmonth <- as.yearmon(longduration$formatteddate)
longduration$yearmonthf <- factor(longduration$yearmonth)
longduration$monthf <- months.Date(longduration$formatteddate)
longduration$week <- strftime(longduration$formatteddate, format = "%V")
longduration$week <- as.numeric(longduration$week)
longduration$day <- weekdays(as.Date(longduration$formatteddate))
longduration <- ddply(longduration,.(yearmonthf), transform, monthweek=1+week-min(week))
conn1 <- longduration[, c("year", "yearmonthf", "monthf", "week", "monthweek", "day")]

write.dta(conn1, "C:/Users/Eduado Acosta/Desktop/Semester 4/Applied Methods in Policy Evaluation/conn1.dta")

dat <- read.dta13("C:/Users/Eduado Acosta/Documents/datafest 18/data/datafest2018_data_and_documentation/data/conn1.dta")

plot1 <- ggplot2::ggplot(dat, aes(monthweek, day, fill = count)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Conn data", fill = "count")

plot1
ggsave(plot1, "plot1.png")
