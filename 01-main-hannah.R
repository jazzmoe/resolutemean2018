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
try(setwd("/Users/hannahmiles/Documents/GitHub/data-fest2018"), silent = TRUE)


source("00-packages.r")
#source("functions.r")


###############################################################################
# 1. get data 
###############################################################################

HTTP <- data.table::fread("./data/data/http.csv")
SIP <- data.table::fread("./data/data/sip.csv")
SNMP <- data.table::fread("./data/data/snmp.csv")
X509 <- data.table::fread("./data/data/x509.csv")
WEIRD <- data.table::fread("./data/data/weird.csv")
HOST <- data.table::fread("./data/data/host.csv")
CONN <- data.table::fread("./data/data/conn.csv")
GEO <-  data.table::fread("./data/data/geolocation.csv")
FILES <-  data.table::fread("./data/data/files.csv")

glimpse(CONN)
names(CONN)
CONN$duration <- as.numeric(CONN$duration)
CONN$duration <- as.numeric(as.character(CONN$duration))
summarise(CONN, mean = mean(duration, na.rm = T), min = min(duration, na.rm = T),
          max = max(duration, na.rm = T))
library(anytime)
CONN$ts <- anytime(CONN$ts)
glimpse(CONN)
hist(CONN$duration)

#subset data - getting rid of low durations#
CONNDUR <- CONN %>% 
  select(ts, duration) %>% 
  filter(!is.na(duration)) %>% 
  arrange(duration) %>% 
  summarise(`25%`=quantile(duration, probs=0.25),
            `50%`=quantile(duration, probs=0.5),
            `75%`=quantile(duration, probs=0.75),
            avg=mean(duration),
            n=n())

count(is.na)

head(CONNDUR)
tail(CONNDUR)



unique(GEO$location)
locFreq <- as_data_frame(table(GEO$location))

glimpse(CONN)


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

