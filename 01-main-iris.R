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
try(setwd("C:/Users/MsUser/Documents/GitHub/data-fest2018"), silent = TRUE)


source("00-packages.r")
#source("functions.r")


###############################################################################
# 1. get data 
###############################################################################

# DataMain <- data.table::fread("./data/data/data.txt")
# DataMain <- head(DataMain,50000)
# save(DataMain, file = "HeadMain.RData")

setwd("C:/Users/MsUser/Desktop/Datafest/R files")
library(foreign)
WEIRD <- data.table::fread("weird.csv")
SSL <- data.table::fread("ssl.csv")
X509 <- data.table::fread("x509.csv")
GEOLOCATION <- data.table::fread("geolocation.csv", stringsAsFactors = F)
FILES <- data.table::fread("files.csv", stringsAsFactors = F)
CONN <- data.table::fread("conn.csv", stringsAsFactors = F)

table(GEOLOCATION$location)
n_distinct(GEOLOCATION$location)

#Merging GEOLOCATION and CONN
GEOLOCATION$id.orig_h <- GEOLOCATION$ip
CONNGEOLOCATION <- merge(CONN, GEOLOCATION, by="id.orig_h")
View(CONNGEOLOCATION)
#About 600,000 obs left, lost about half of the data

summary(CONN$duration)
summarise(CONN$duration)

install.packages("anytime")
library(anytime)
CONN$TS <- anytime(CONN$ts)

CONN$durationnum <- as.numeric(CONN$duration)
summary(CONN$durationnum)

CountryCount <- CONNGEOLOCATION %>% dplyr::group_by(location) %>% summarize(count=n())
CountryCountWithoutGermany <- CountryCount[ which (CountryCount$count <= 40000), ]
plot (CountryCountWithoutGermany$Freq, CountryCountWithoutGermany$count)
summary(lm(count ~ Freq, data=CountryCountWithoutGermany))

SUMDUR <- CONN %>% select(ts, id.orig_h, durationnum) %>% 
  filter(!is.na(durationnum)) %>% 
  group_by(id.orig_h) %>% 
  summarise(sum(durationnum))

SumDurFreq <- merge(SUMDUR, IPfreq, by ="id.orig_h")
plot (SumDurFreq$`sum(durationnum)`, SumDurFreq$Freq)
SumDurFreqNoOutliers <- SumDurFreq [ which (SumDurFreq <= 100000), ]
plot (SumDurFreqNoOutliers$`sum(durationnum)`, SumDurFreqNoOutliers$Freq)

SumDurFreqLoc <- merge(SumDurFreq, GEOLOCATION, by="id.orig_h")
SumDurFreqLoc$ip <- NULL

#Exploring outliers
unique(CONN$proto)

#Dataframe with duration, frequency, location, and time
SumDurFreqLocTime <- merge(SumDurFreqLoc, CONN, by="id.orig_h")
#Deleting superfluous variables
SumDurFreqLocTime$uid <- NULL
SumDurFreqLocTime$id.orig_p <- NULL
SumDurFreqLocTime$id.resp_h <- NULL
SumDurFreqLocTime$id.resp_p <- NULL
SumDurFreqLocTime$proto <- NULL
SumDurFreqLocTime$service <- NULL
SumDurFreqLocTime$orig_bytes <- NULL
SumDurFreqLocTime$resp_bytes <- NULL
SumDurFreqLocTime$conn_state <- NULL
SumDurFreqLocTime$local_orig <- NULL
SumDurFreqLocTime$local_resp <- NULL
SumDurFreqLocTime$missed_bytes <- NULL
SumDurFreqLocTime$history <- NULL
SumDurFreqLocTime$orig_pkts <- NULL
SumDurFreqLocTime$orig_ip_bytes <- NULL
SumDurFreqLocTime$resp_pkts <- NULL
SumDurFreqLocTime$resp_ip_bytes <- NULL
SumDurFreqLocTime$tunnel_parents <- NULL
SumDurFreqLocTime$tunnel_parents <- NULL
#plotting by duration and ts
plot(durationnum ~ ts, data=subset(SumDurFreqLocTime, location =="United States"))

try(setwd("C:/Users/MsUser/Documents/GitHub/resolutemean2018"), silent = TRUE)

load("./oecd-data/OECD.WDI.r")
