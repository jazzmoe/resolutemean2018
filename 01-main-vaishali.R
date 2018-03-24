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
library(magrittr)

options(scipen=999)

###############################################################################
# 1. get data 
###############################################################################

conn <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/conn.csv")
geolocation <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/geolocation.csv")

geolocation$id.orig_h <- geolocation$ip
conngeo <- merge(conn, geolocation, by = "id.orig_h")

# headconn <- head(conn,50000)
# save(headconn, file = "Headconn.RData")


#load("Headconn.RData")

# files <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/files.csv")
# headfiles <- head(files,50000)
# save(headfiles, file = "HeadFiles.RData")

#####################
### Preparation #####
#####################

##########################Outlier analysis#######################################
library(outliers)
conngeo$duration <- as.numeric(conngeo$duration)

#Variables with an outlier
chisq.out.test(conngeo$duration)
chisq.out.test(conngeo$orig_ip_bytes)
chisq.out.test(conngeo$resp_ip_bytes)

outliers <- conngeo %>%  filter(duration==2157.128511 | orig_ip_bytes==330177 |resp_ip_bytes==4433136) %>% select(id.orig_h)

outliers

################################################################################

weird <- data.table::fread("C:/Users/WINDOWS-PC/Dropbox/Datafest 2018/datafest2018_data_and_documentation/data/weird.csv")



countrycount <- conngeo %>% dplyr::group_by(location) %>% summarize(count=n())

##Korea heat map by IP addresses
korea <- conngeo %>% dplyr::filter(location=="Korea")

library(ggmap)
for (i in 1:nrow(korea)) {
  latlon = geocode(korea[i,1])
  korea$lon[i] = as.numeric(latlon[1])
  korea$lat[i] = as.numeric(latlon[2])
}

bbox <- make_bbox(korea$lon,korea$lat,f=1)
b <- get_map(bbox)

ggmap(b) + geom_point(data=korea, aes(lon, lat), size=2, alpha=0.7)

#####################TIME SERIES PLOT#######################
library(plyr)
library(scales)
library(zoo)
library(anytime)

conngeo$formatteddate <- anytime(conngeo$ts)



conngeo$formatteddate <- as.Date(conngeo$formatteddate)
conngeo$year <- substr(conngeo$formatteddate,1,4)

conngeo$yearmonth <- as.yearmon(conngeo$formatteddate)
conngeo$yearmonthf <- factor(conngeo$yearmonth)
conngeo$monthf <- months.Date(conngeo$formatteddate)
conngeo$week <- strftime(conngeo$formatteddate, format = "%V")

conngeo$week <- as.numeric(conngeo$week)

conngeo$day <- weekdays(as.Date(conngeo$formatteddate))

conngeo <- ddply(conngeo,.(yearmonthf), transform, monthweek=1+week-min(week))



conn1 <- conngeo[, c("week", "duration", "location", "yearmonthf")]


conn2 <- filter(conn1, location=="United States"|location=="Germany")
ggplot(conn2, aes(week, duration, group=location, color=location)) + geom_line() +
   xlab("") + ylab("Duration")

#######################################################################################

#plot1 <- ggplot2::ggplot(conn1, aes(monthweek, day, fill = orig_ip_bytes)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Conn data", fill = "orig_ip_bytes")

plot1
ggsave(plot1, "plot1.png")