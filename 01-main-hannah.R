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
try(setwd("includeyourfolder"), silent = TRUE)


source("00-packages.r")
#source("functions.r")


###############################################################################
# 1. get data 
###############################################################################

# DataMain <- data.table::fread("./data/data/data.txt")
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
