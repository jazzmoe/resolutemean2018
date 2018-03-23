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
try(setwd("C:\Users\Moritz\Desktop\resolutemean2018"), silent = TRUE)
try(setwd("includeyourfolder"), silent = TRUE)


source("00-packages.r")
#source("functions.r")


###############################################################################
# 1. get data 
###############################################################################

# CONN <- data.table::fread("./data/data/conn.csv")
save(CONN, file = "CONN.r")
DNS <- data.table::fread("./data/data/dns.csv")
DPD <- data.table::fread("./data/data/dpd.csv")
FILES <- data.table::fread("./data/data/files.csv")
GEOLOCATION <- data.table::fread("./data/data/geolocation.csv")
HOST <- data.table::fread("./data/data/host.csv")
HTTP <- data.table::fread("./data/data/http.csv")
SIP <- data.table::fread("./data/data/sip.csv")
SNMP <- data.table::fread("./data/data/snmp.csv")
SSL <- data.table::fread("./data/data/ssl.csv")
WEIRD <- data.table::fread("./data/data/weird.csv")
x509 <- data.table::fread("./data/data/sip.csv")

# dataFiles <- c("dns", "dpd", "files", "geolocation", "host", "http", "sip", "snmp", "ssl", "weird", "x509")
# temp = list.files(pattern="*.csv")
# myfiles = lapply(temp, read.delim)

# for (i in 1:end(dataFiles)) {
#   tloop <- paste("./data/data/", i, ".csv")
#   i <-  data.table::fread(tloop)
#   rm(tloop)
# }

#####################
### Preparation #####
#####################

# unique IP addresses
length(unique(CONN$id.orig_h))
uniqueIP <- unique(CONN$id.orig_h)

# date
CONN <- CONN %>% 
  mutate(date = as_datetime(ts),
  mutate(date = as_datetime(ts),
  mutate(date = as_datetime(ts),
  mutate(date = as_datetime(ts),
  mutate(date = as_datetime(ts),
  mutate(date = as_datetime(ts),
    
         )

# frequency of IP

# time duration from first to last request
# 'time'
