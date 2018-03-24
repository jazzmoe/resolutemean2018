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
rm(CONN, DNS)
# 
DUR.LOC <- CONN.OECD %>% select(date, duration, location) %>% 
  filter(!is.na(duration)) %>%
  mutate(week = week(date), year = year(date)) %>%
  group_by()
  summarize()
  r
         
# DUR.LOC$year <- as.numeric(DUR.LOC$year) 


# DUR.LOC <- if(DUR.LOC$year == 2014) {
#     mutate(DUR.LOC, exp.week = week-32) 
# } else {
#     replace(DUR.LOC, exp.week, week+21)
#   }
#   
  
         
  



conngeo$formatteddate <- anytime(conngeo$ts)



conngeo$formatteddate <- as.Date(conngeo$formatteddate)
conngeo$year <- substr(conngeo$formatteddate,1,4)

conngeo$yearmonth <- as.yearmon(conngeo$formatteddate)
conngeo$yearmonthf <- factor(conngeo$yearmonth)
conngeo$monthf <- months.Date(conngeo$formatteddate)
conngeo$week <- strftime(conngeo$formatteddate, format = "%V")

conngeo$week <- as.numeric(conngeo$week)

conngeo$day <- weekdays(as.Date(conngeo$formatteddate))







# summarize duration by IP
CONN.DUR.SUM.IP <- CONN.OECD %>% select(date, id.orig_h, duration) %>%
  filter(!is.na(duration)) %>% 
  group_by(id.orig_h) %>% 
  summarise(sumdur = sum(duration))
  
# summarize duration by location
CONN.DUR.SUM.LOCATION <- CONN.OECD


#1. scatterplot - duration and frequency of IP address# 
plot1 <- CONN %>% ggplot(aes(x = duration, y = Freq.y), na.rm = T)+
  geom_point()

#2. scatterplot - duration and frequency of IP address, w/o Germany# 

plot2 <- CONN %>% filter(Freq.y < 10000) %>%  
  ggplot(aes(x = duration, y = Freq.y), na.rm = T)+
  geom_point()

#3. scatterplot - aggregated duration and IP address#

plot3 <- SUMDUR %>% ggplot(aes(x = sumdur, y = Freq), na.rm = T)+
  geom_point()

#4. scatterplot - aggregated duration and IP address, with colours etc.#

plot4 <- SUMDUR %>% ggplot(aes(x = sumdur, y = Freq), na.rm = T)+
  geom_point()


#country count
CountryCount <- CONN %>% group_by(location) %>% summarise(count=n())
CountryCount <- merge(CountryCount, locFreq, by = 'location')
locFreq <- rename(locFreq, location = Var1)

#sum of the durations # 
SUMDUR <- CONN %>% select(ts, id.orig_h, duration) %>%
  filter(!is.na(duration)) %>% 
  group_by(id.orig_h) %>% 
  summarise(sumdur = sum(duration))

sum(is.na(SUMDUR$sumdur))
sum(is.na(CONN$duration))

SUMDUR <- merge(SUMDUR, IPfreq, by = 'id.orig_h')

#add country data to SUMDUR# 
SUMDUR <- merge(SUMDUR, GEOLOCATION, by = 'id.orig_h')






CONN %>% filter(Freq.y < 10000) %>%  
  ggplot(aes(x = duration, y = Freq.y), na.rm = T)+
  geom_point()






######################
###### Graphics ######
######################

# World Map