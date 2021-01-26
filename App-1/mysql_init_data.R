# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

####################################################
# mysql_init_data.R : run config.R followed by this 
####################################################
require("RMySQL")
require("openxlsx")
require("stringr")

df01 <- read.xlsx("./Data/DATA.xlsx", sheet = "Sheet1")


ckdata <- df01

# filter missing values
# do not filter until checked
ckdata <- subset(ckdata, subset=(epic_q1!=10))

# char to numeric
ckdata$Time_Point = as.numeric(ckdata$Time_Point)
ckdata$PSA = as.numeric(ckdata$PSA)

# 엑셀 날짜 타입 변환
ckdata[,c("DOB")] <- convertToDate(ckdata[,c("DOB")]) 
ckdata[,c("Date")] <- convertToDate(ckdata[,c("Date")]) 

ckdata <- subset(ckdata, subset=(!is.na(Date))) # Date values NA rows will be ommitted
ckdata <- subset(ckdata, subset=(!is.na(DOB))) # DOB values NA rows will be ommitted


# PSA Doubling 숫자 변환
ckdata[is.na(ckdata$PSA.Doubling), "PSA.Doubling"] <- 0 # PSA.Doubling NA to 0
ckdata$PSA.Doubling <- as.numeric(ckdata$PSA.Doubling)

# PSA "<0.1" 0.09로 바꿈
ckdata$PSA <- as.numeric(sub('<0.1','0.09',ckdata$PSA))

# AGE 추가 "shim_date year" - "DOB year"
ckdata$AGE <- round(as.numeric(ckdata$Date - ckdata$DOB)/365)

# GleasonsSum Column add
ckdata$GleasonsFirst <- substr(str_split_fixed(ckdata$Gleasons, "=", 2)[,1],1,1)
ckdata$GleasonsFirst = as.numeric(ckdata$GleasonsFirst)
ckdata$GleasonsSecond <- substr(str_split_fixed(ckdata$Gleasons, "=", 2)[,1],3,3)
ckdata$GleasonsSecond = as.numeric(ckdata$GleasonsSecond)
ckdata$GleasonsSum <- str_split_fixed(ckdata$Gleasons, "=", 2)[,2]
ckdata$GleasonsSum = as.numeric(ckdata$GleasonsSum)


############
## save data frame to mysql
# connect DB
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
dbSendQuery(mydb, "drop table if exists TB_CYBERKNIFE;")
dbWriteTable(mydb, 'TB_CYBERKNIFE', ckdata, row.names=F)
dbDisconnect(mydb)
