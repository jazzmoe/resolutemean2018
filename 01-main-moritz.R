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
# save(CONN, file = "./data/data/CONN.r")
load("./data/data/CONN.R")
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

#####################
### Preparation #####
#####################

# change date to human readable
CONN$date <- anytime(CONN$ts)
# CONN$month <- month(CONN$date)
# CONN$day <- day(CONN$ts)
glimpse(CONN)

CONN$duration <- as.numeric(as.character(CONN$duration))

# unique IP addresses
length(unique(CONN$id.orig_h))
uniqueIP <- unique(CONN$id.orig_h)

# Include in CONN: frequency of occurences per IP = ipFreq
IPfreq <- as.data.frame(table(CONN$id.orig_h)) %>% rename(id.orig_h = Var1)
CONN <- left_join(CONN, IPfreq, by="id.orig_h") %>% rename(ipFreq = Freq)
# Include Geolocation to CONN
GEOLOCATION <- rename(GEOLOCATION, id.orig_h = ip)
CONN <- left_join(CONN, GEOLOCATION, by="id.orig_h") 
# frequency of unique IP per location = locUniqueFreq

# frequency of total IP per geolocation = locFreq
locFreq <- CONN %>% group_by(location) %>% summarize(locFreq = n())
CONN <- left_join(CONN, locFreq, by="location") 

# 158 countries 
countries <- unique(GEOLOCATION$location)
length(countries)
length(unique(GEOLOCATION$id.orig_h))
locFreq <- as.data.frame(table(GEOLOCATION$location))

######################
### Outside Data #####
######################

temp <- read_csv("./oecd-data/WDIData.csv")
names(temp)[1] <- "Country"
names(temp)[4] <- "Indicator"
OECD.WDI <- temp[temp$Country %in% countries,]

OECD.vars <- as.character(c("ER.H2O.FWAG.ZS", "ER.H2O.FWDM.ZS", "ER.H2O.FWIN.ZS", "ER.H2O.FWTL.ZS", "TX.VAL.OTHR.ZS.WT", "TM.VAL.OTHR.ZS.WT", "BX.GSR.CMCP.ZS", "BM.GSR.CMCP.ZS", "IC.IMP.COST.CD",
"SE.XPD.CTOT.ZS", "EG.USE.ELEC.KH.PC", "EG.ELC.HYRO.ZS", "IC.FRM.THEV.ZS", "IT.NET.BBND", "IT.NET.BBND.P2", "NY.GDP.PCAP.KD", "NY.GDP.PCAP.CD", "SI.POV.GINI", "TX.VAL.TECH.MF.ZS",
"TX.VAL.ICTG.ZS.UN", "TM.VAL.ICTG.ZS.UN", "EG.FEC.RNEW.ZS", "ER.H2O.INTR.PC", "ER.H2O.INTR.K3", "IP.JRN.ARTC.SC", "IT.NET.SECR", "IT.NET.SECR.P6", "SP.POP.TECH.RD.P6", "BX.GSR.CCIS.CD", "SH.H2O.SAFE.ZS",
"SH.H2O.SAFE.RU.ZS", "SH.H2O.SAFE.UR.ZS", "IE.PPI.ICTI.CD", "MS.MIL.XPND.GD.ZS", "MS.MIL.XPND.ZS", "IC.BUS.NREG", "SH.H2O.BASW.ZS", "SH.H2O.BASW.RU.ZS", "SH.H2O.BASW.UR.ZS", "SP.POP.TOTL"))

OECD.WDI <- OECD.WDI[OECD.WDI$Indicator %in% OECD.vars,] %>% 
  select(one_of(c("Country", "Indicator", "2014"))) %>%
  gather( `2014`, key = "year", value = "Value") %>%
  spread(key = "Indicator", value = "Value")
rm(temp)

# selecting: Secure Server/1 Million IT.NET.SECR.P6 -> secureServer.per.million
# GDP per capita (current US$)	NY.GDP.PCAP.CD -> gdp.per.capita
# Fixed broadband subscriptions (per 100 people)	IT.NET.BBND.P2 -> Broadband.per.100
# Electricity production from hydroelectric sources (% of total) EG.ELC.HYRO.ZS -> hydro.electricity.pct
# Current education expenditure, total (% of total expenditure in public institutions) SE.XPD.CTOT.ZS -> ecucation.spending
OECD.WDI <- OECD.WDI %>% rename(
  secureServer.per.million = IT.NET.SECR.P6,
  gdp.per.capita = NY.GDP.PCAP.CD,
  broadband.per.100 = IT.NET.BBND.P2,
  hydro.electricity.pct = EG.ELC.HYRO.ZS,
  education.spending = SE.XPD.CTOT.ZS,
  population = SP.POP.TOTL) %>%
  select(one_of(
    c("Country", 
      "Year", 
      "secureServer.per.million", 
      "gdp.per.capita", 
      "broadband.per.100", 
      "hydro.electricity.pct", 
      "education.spending",
      "population")))
  
save(OECD.WDI, file = "./oecd-data/OECD.WDI.r")
load("./oecd-data/OECD.WDI.r")
OECD.WDI$secureServer.per.million <- round(OECD.WDI$secureServer.per.million)
OECD.WDI <- OECD.WDI %>% rename(location = Country)

### CONN without missing locations
CONN.loc <- CONN %>% filter(!is.na(CONN$location))

# merge OECD and CONN data
CONN.OECD <- left_join(CONN.loc, OECD.WDI, key = location)
save(CONN.OECD, file = "./oecd-data/CONN.OECD.r")

########################
##### Descriptives #####
########################

# number of attempts aggregated per day
AT.PER.DAY <- CONN.OECD %>% select(ts) %>%
  mutate(date.day = as.Date(ts)) %>% select(date.day) %>%
  group_by(date.day) %>% summarize(requestFreq = n())




# c("Russia", "China", "US", "Netherlands", "Korea", "Brazil")



  














rm(CONN, DNS)

load("./oecd-data/CONN.OECD.r")

# compute curation per week by country
DUR.LOC <- CONN.OECD %>% select(date, duration, location) %>% 
  filter(!is.na(duration)) %>%
  mutate(week = week(date), year = year(date))

DUR.LOC$week <- ifelse(DUR.LOC$week == 33 & DUR.LOC$year == 2014, 1, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 34 & DUR.LOC$year == 2014, 2, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 35 & DUR.LOC$year == 2014, 3, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 36 & DUR.LOC$year == 2014, 4, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 37 & DUR.LOC$year == 2014, 5, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 38 & DUR.LOC$year == 2014, 6, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 39 & DUR.LOC$year == 2014, 7, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 40 & DUR.LOC$year == 2014, 8, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 41 & DUR.LOC$year == 2014, 9, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 42 & DUR.LOC$year == 2014, 10, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 43 & DUR.LOC$year == 2014, 11, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 44 & DUR.LOC$year == 2014, 12, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 45 & DUR.LOC$year == 2014, 13, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 46 & DUR.LOC$year == 2014, 14, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 47 & DUR.LOC$year == 2014, 15, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 48 & DUR.LOC$year == 2014, 16, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 49 & DUR.LOC$year == 2014, 17, DUR.LOC$week) 
DUR.LOC$week <- ifelse(DUR.LOC$week == 50 & DUR.LOC$year == 2014, 18, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 51 & DUR.LOC$year == 2014, 19, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 52 & DUR.LOC$year == 2014, 20, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 53 & DUR.LOC$year == 2014, 21, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 1 & DUR.LOC$year == 2015, 22, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 2 & DUR.LOC$year == 2015, 23, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 3 & DUR.LOC$year == 2015, 24, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 4 & DUR.LOC$year == 2015, 25, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 5 & DUR.LOC$year == 2015, 26, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 6 & DUR.LOC$year == 2015, 27, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 7 & DUR.LOC$year == 2015, 28, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 8 & DUR.LOC$year == 2015, 29, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 9 & DUR.LOC$year == 2015, 30, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 10 & DUR.LOC$year == 2015, 31, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 11 & DUR.LOC$year == 2015, 32, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 12 & DUR.LOC$year == 2015, 33, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 13 & DUR.LOC$year == 2015, 34, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 14 & DUR.LOC$year == 2015, 35, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 15 & DUR.LOC$year == 2015, 36, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 16 & DUR.LOC$year == 2015, 37, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 17 & DUR.LOC$year == 2015, 38, DUR.LOC$week)
DUR.LOC$week <- ifelse(DUR.LOC$week == 18 & DUR.LOC$year == 2015, 39, DUR.LOC$week)
DUR.LOC$week <- as.numeric(DUR.LOC$week)

# DUR.LOC <- DUR.LOC %>% group_by(location, week) %>%
#   summarize(dur.p.week = sum(duration)) %>% 
#   filter(location = one_of(c("China", "United States", "Korea", "Japan", "Russian Federation", "Brazil", "Germany", "Italy", "India", "Taiwan")))

DUR.LOC <- DUR.LOC %>% group_by(location, week) %>%
  summarize(dur.p.week = sum(duration)) %>% 
  filter(location %in% c("China", "United States", "Russian Federation", "Brazil"))
#DUR.LOC$dur.p.week <- as.numeric(DUR.LOC$dur.p.week/60)
DUR.LOC$dur.p.week <- round(DUR.LOC$dur.p.week)

DUR.LOC <- DUR.LOC %>% group_by(location, week) %>%
  summarize(dur.p.week = sum(duration)) %>%
  filter(location != "Germany")
DUR.LOC$dur.p.week <- round(DUR.LOC$dur.p.week)

plot1 <- DUR.LOC %>% ggplot(aes(x=week, y=dur.p.week, fill=location)) + 
  geom_area()

# changes
