###########################
## Datafest Munich 2018  ##
###########################

###############################################################################
# CONTENT
# 0. Preparation
# 1. Get Data
# 2. Data Preparation
###############################################################################

###############################################################################
# 0. Settings
###############################################################################

# Clear global environment
rm(list=ls())

## Setting working directory
try(setwd("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/data-fest2018"), silent = TRUE)
try(setwd("includeyourfolder"), silent = TRUE)


source("00-packages.r")
#source("functions.r")
library(outliers)

options(scipen=999)

###############################################################################
# 1. get data 
###############################################################################

# conn <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/conn.csv")
# headconn <- head(conn,50000)
# save(headconn, file = "Headconn.RData")


#load("Headconn.RData")

# files <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/files.csv")
# headfiles <- head(files,50000)
# save(headfiles, file = "HeadFiles.RData")

#####################
### Preparation #####
#####################

### 

#Calendar heat map

library(ggplot2)
library(plyr)
library(scales)
library(zoo)
library(anytime)

conn$formatteddate <- anytime(conn$ts)



conn$formatteddate <- as.Date(conn$formatteddate)
conn$year <- substr(conn$formatteddate,1,4)

conn$yearmonth <- as.yearmon(conn$formatteddate)
conn$yearmonthf <- factor(conn$yearmonth)
conn$monthf <- months.Date(conn$formatteddate)
conn$week <- strftime(conn$formatteddate, format = "%V")

conn$week <- as.numeric(conn$week)

conn$day <- weekdays(as.Date(conn$formatteddate))

conn <- ddply(conn,.(yearmonthf), transform, monthweek=1+week-min(week))

conn1 <- conn[, c("year", "yearmonthf", "monthf", "week", "monthweek", "day", "orig_ip_bytes")]

head(conn1)

plot1 <- ggplot2::ggplot(conn1, aes(monthweek, day, fill = orig_ip_bytes)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Conn data", fill = "orig_ip_bytes")

plot1
ggsave(plot1, "plot1.png")