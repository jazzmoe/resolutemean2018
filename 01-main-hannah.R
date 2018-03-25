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
try(setwd("C:/Users/Moritz/OneDrive/data-fest2018"), silent = TRUE)
try(setwd("/Users/hannahmiles/Documents/GitHub/resolutemean2018"), silent = TRUE)


source("00-packages.r")
#source("functions.r")


###############################################################################
# 1. get data 
###############################################################################

#HTTP <- data.table::fread("./data/data/http.csv")
#SIP <- data.table::fread("./data/data/sip.csv")
#SNMP <- data.table::fread("./data/data/snmp.csv")
#X509 <- data.table::fread("./data/data/x509.csv")
#WEIRD <- data.table::fread("./data/data/weird.csv")
#HOST <- data.table::fread("./data/data/host.csv")
CONN <- data.table::fread("./data/data/conn.csv")
GEOLOCATION <-  data.table::fread("./data/data/geolocation.csv")
FILES <-  data.table::fread("./data/data/files.csv")

load("./oecd-data/CONN.OECD.r")



#Info geolocation
unique(GEOLOCATION$location)
locFreq <- as_data_frame(table(GEOLOCATION$location))


#duration as numeric
glimpse(CONN)
names(CONN)

CONN$duration <- as.numeric(as.character(CONN$duration))
summarise(CONN, mean = mean(duration, na.rm = T), min = min(duration, na.rm = T),
          max = max(duration, na.rm = T))
#change time stamp#

library(anytime)
CONN$ts <- anytime(CONN$ts)
glimpse(CONN)
hist(CONN$duration)

#subset data - getting rid of low durations#
CONNDUR <- CONN %>% 
  select(ts, duration) %>% 
  filter(!is.na(duration)) %>% 
  arrange(duration)

sum(is.na(CONN$duration))

SUMCONDUR <- summarise(CONNDUR, 
            `25%`=quantile(duration, probs=0.25),
            `50%`=quantile(duration, probs=0.5),
            `75%`=quantile(duration, probs=0.75),
            avg=mean(duration),
            n=n())
head(CONNDUR)
tail(CONNDUR)

#histogram#
temp <- filter(CONNDUR, duration > 500)
hist(temp$duration)

#log temp$duration#
hist(log(temp$duration))

# unique IP addresses
length(unique(CONN$id.orig_h))
uniqueIP <- unique(CONN$id.orig_h)

# frequency of IP
IPfreq <- as.data.frame(table(CONN$id.orig_h)) %>% rename(id.orig_h = Var1)
CONN <- left_join(CONN, IPfreq, by="id.orig_h")
GEOLOCATION <- rename(GEOLOCATION, id.orig_h = ip)
CONN <- left_join(CONN, GEOLOCATION, by="id.orig_h")


# 158 countries 
length(unique(GEOLOCATION$location))
length(unique(GEOLOCATION$id.orig_h))
locFreq <- as.data.frame(table(GEOLOCATION$location))

#1. scatterplot - duration and frequency of IP address# 
glimpse(CONN)
plot1 <- CONN %>% ggplot(aes(x = duration, y = Freq), na.rm = T)+
  geom_point()

#2. scatterplot - duration and frequency of IP address, w/o Germany# 

plot2 <- CONN %>% filter(Freq < 10000) %>%  
  ggplot(aes(x = duration, y = Freq), na.rm = T)+
  geom_point()

#sum of the durations # 
SUMDUR <- CONN.OECD %>% select(id.orig_h, duration, location, 
                               gdp.per.capita, broadband.per.100, 
                               population, education.spending) %>%
        filter(!is.na(duration)) %>% 
        group_by(id.orig_h)

%>% 
  summarise(sumdur = sum(duration))

SUMDUR <- merge(SUMDUR, IPfreq, by = 'id.orig_h')

sum(is.na(SUMDUR$sumdur))
sum(is.na(CONN$duration))

#3. scatterplot - aggregated duration and IP address#

plot3 <- SUMDUR %>% filter(sumdur < 200000, Freq < 200000) %>% 
  ggplot(aes(x = sumdur, y = Freq), na.rm = T)+
  geom_point()

plot3 + geom_jitter(aes(col=CONN.OECD$gdp.per.capita, 
                        size=CONN.OECD$broadband.per.100))




#country count
CountryCount <- CONN.OECD %>% group_by(location) %>% summarise(count=n())
CountryCount <- merge(CountryCount, locFreq, by = 'location')
locFreq <- rename(locFreq, location = Var1)




#add country data to SUMDUR# 
SUMDUR <- merge(SUMDUR, GEOLOCATION, by = 'id.orig_h')



#heatmap#

GEOLOCATION <- data.table::fread("geolocation.csv")
CONN <- data.table::fread("conn.csv")
load("./oecd-data/CONN.OECD.r")
countries <- unique(GEOLOCATION$location)
length(countries)
length(unique(GEOLOCATION$id.orig_h))
locFreq <- as.data.frame(table(GEOLOCATION$location))

longduration <- subset(CONN.OECD, duration > 600)
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

write.dta(longduration, "/Users/hannahmiles/Documents/GitHub/resolutemean2018/hannahmap.dta")

longduration2 <- read.dta13("/Users/hannahmiles/Documents/GitHub/resolutemean2018/hannahmap.dta")

P<- ggplot(longduration2, aes(monthweek, weekdayf, fill = totcon)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="yellow", high="red") + xlab("Week of Month") + ylab("")
P



#number of attempts aggregated per day

AT.PER.DAY <- CONN.OECD %>% select(date) %>% 
  mutate(date.day = ymd(date))

#countries with most requests

CONN.OECD1 <- CONN.OECD %>% select(location, locFreq) %>% 
  arrange(locFreq)

#bar chart secure servers
options(scipen=999)
bardf <- CONN.OECD %>% select(secureServer.per.million, location) %>% 
  filter(location == 'Russian Federation' | location == 'China' |
           location == 'United States' | location == 'Netherlands' | location == 'Brazil') %>% 
  group_by(location) %>% summarise(servers= max(secureServer.per.million))


positions <- c("Netherlands", "United States", "Russian Federation", "Brazil", "China")


barchart <- ggplot(bardf, aes(x=location, y=servers)) + 
  geom_bar(stat="identity", fill="tomato3") + 
  labs(title="Number of Secure Servers per million people", 
       subtitle="of the five countries with the most requests to honeynet", 
       caption="source: OECD") + 
  labs(x="Country", y="Number of servers per million people")+
  scale_x_discrete(limits = positions)+
  geom_text(aes(label=bardf$servers), position=position_dodge(width=0.9), vjust=-0.25)




          




# DataMain <- head(DataMain,50000)
# save(DataMain, file = "HeadMain.RData")

#DataMain <- data.table::fread("./data/data/data.txt")
#DataMain <- head(DataMain,200000)
#save(DataMain, file = "HeadMain.RData")

#DataTotal <- fread("C:/Users/Moritz/Dropbox/DatafestTeamTori/Data/data/data/data.txt")
#save(DataTotal, file = "DataTotal.RData")

# load
#load("sampleDest.RData")

#####################
### Preparation #####
#####################

### 

