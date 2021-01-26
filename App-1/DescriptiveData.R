# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

# descriptive data
#source(./load.R)
#########################################
## load.R : load data from mysql
require("RMySQL")
# connect DB
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
survData <- dbReadTable(mydb, name = 'TB_CYBERKNIFE_SURVIVAL')
dbDisconnect(mydb)


# followup days
mean(survData$SurvDays)
median(survData$SurvDays)
min(survData$SurvDays)
max(survData$SurvDays)


# followup date info
median(cldata$Date)
min(cldata$Date)
max(cldata$Date)

# descriptive analysis
D <- survData
names(D)
library(moonBook)
D$D_TEMP <- 1
mytable(D_TEMP~RACE_CATEGORY+PSA_LEVEL+RISK_CATEGORY+T_STAGE_CATEGORY+AGE_CATEGORY+GLEASONS_CATEGORY+PCAP_CLASS,data = D)
