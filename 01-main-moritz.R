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
glimpse(CONN)

# unique IP addresses
length(unique(CONN$id.orig_h))
uniqueIP <- unique(CONN$id.orig_h)

# frequency of IP
IPfreq <- as.data.frame(table(CONN$id.orig_h)) %>% rename(id.orig_h = Var1)
CONN <- left_join(CONN, IPfreq, by="id.orig_h")
GEOLOCATION <- rename(GEOLOCATION, id.orig_h = ip)
CONN <- left_join(CONN, GEOLOCATION, by="id.orig_h")

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

######################
### Descriptives #####
######################

# 


######################
###### Graphics ######
######################

# World Map