# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

####
# DATA_DISTRIBUTION.R
####
require("RMySQL")
# connect DB
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
distData <- dbReadTable(mydb, name = 'TB_CYBERKNIFE_SURVIVAL')
dbDisconnect(mydb)
####
par( mfrow = c(1,4))
hist(distData$PCAP_CLASS)
hist(distData$RISK_CATEGORY)
hist(distData$EXCEL_RISK)
hist(distData$DAMICO_RISK_CATEGORY)

par( mfrow = c(1,3))
hist(distData$AGE)
hist(distData$GLEASONS_CATEGORY)
hist(distData$PSA_LEVEL)
