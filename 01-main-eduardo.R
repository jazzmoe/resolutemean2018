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

longduration$totalconn %>% group_by(longduration$formatteddate) %>% summarise(count())
  dplyr::(longduration$datecount)

longduration$year <- substr(longduration$formatteddate,1,4)

connections$yearmonth <- as.yearmon(connections$formatteddate)
connections$yearmonthf <- factor(connections$yearmonth)
connections$monthf <- months.Date(connections$formatteddate)
connections$week <- strftime(connections$formatteddate, format = "%V")

connections$week <- as.numeric(connections$week)

connections$day <- weekdays(as.Date(connections$formatteddate))

connections <- ddply(connections,.(yearmonthf), transform, monthweek=1+week-min(week))

###
connections$date <- toString(connections$formatteddate)

connections$totalconn %>% group_by(formatteddate) %>% summarise(sum )

connections$totalconn <- count(connections, c('formatteddate'))



conn1 <- connections[, c("year", "yearmonthf", "monthf", "week", "monthweek", "day", "orig_ip_bytes")]

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
