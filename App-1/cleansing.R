# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

###############################################
# cleansing.R : cleansing data from mysql
###############################################
require("RMySQL")
require("openxlsx")

###############################################
# connect to mysql
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
# load raw data from mysql: (mysql_init_data.R loads data into mysql DB)
ckdata <- dbReadTable(mydb, name = 'TB_CYBERKNIFE')
dbDisconnect(mydb)
# make before file 
#write.xlsx(ckdata,"./Data/Before_Cleansing_Data.xlsx")
###############################################

# character to numeric value
n <- ncol(ckdata)
cols <- names(ckdata)
for( i in 1:n )
{ 
  if ( substr(cols[i],1,4) == "epic" )
  {
    ckdata[,cols[i]] <- as.numeric(ckdata[,cols[i]])
  }
}
#str(ckdata)

###############################################
# cleansing: cldata
# change date type
ckdata <- ckdata[!is.na(ckdata$DOB),]
ckdata <- ckdata[!is.na(ckdata$Date),]

ckdata[,c("DOB")] <- as.Date(ckdata[,c("DOB")],format="%Y-%m-%d") 
ckdata[,c("Date")] <- as.Date(ckdata[,c("Date")],format="%Y-%m-%d") 
cldata <- ckdata

###########
# AGE Calculation ( based on start date : Time_Point = 1)
## vNData is new patient's information
###########
## 0. Data preparation for suvival analysis
TempData <- cldata
## 1. Get unique patient_id list 
uniquePIDlist <- unique(TempData[,c("Patient_Id")])
n <- length(uniquePIDlist)

###########
## Target Dataframe preparation
AGE = 0
for(i in 1:n)
{
  subList <- TempData[which(TempData$Patient_Id==uniquePIDlist[i]),]
  AGE = round(as.numeric(subList[1,"Date"] - subList[1,"DOB"])/365)
  cldata[ which(cldata$Patient_Id == uniquePIDlist[i]), ]$AGE <- AGE
}
##########
# AGE GROUP(40s:1, 50s:2, 60s:3, 70s:4, 80s:5, 90s:6)
# AGE GROUP NEW(Under 50s:1, 60s:2, 70s:3, Over 80s:4)
head(cldata$AGE)
cldata<- within( cldata, {
  AGE_CATEGORY = character(0) 
  AGE_CATEGORY[ AGE >= 40 & AGE < 60 ] = "1" 
  AGE_CATEGORY[ AGE >= 60 & AGE < 70 ] = "2"
  AGE_CATEGORY[ AGE >= 70 & AGE < 80 ] = "3" 
  AGE_CATEGORY[ AGE >= 80 ] = "4" 
  AGE_CATEGORY = factor(AGE_CATEGORY, level = c("1", "2", "3", "4"))
})
cldata$AGE_CATEGORY <- as.numeric(cldata$AGE_CATEGORY)
# CHK : AGE_CATEGORY
# str(cldata$AGE_CATEGORY)

##########
# T-Stage GROUP( T1-(T1a,T1b,T1c):1, T2a:2, T2b:3, T2c:4, T3-(T3a,T3b):5)
head(cldata$T_Stage)
cldata<- within( cldata, {
  T_STAGE_CATEGORY = character(0) 
  T_STAGE_CATEGORY[ substr(T_Stage,1,2) == "T1" ] = "1" 
  T_STAGE_CATEGORY[ substr(T_Stage,1,3) == "T2a" ] = "2" 
  T_STAGE_CATEGORY[ substr(T_Stage,1,3) == "T2b" ] = "3" 
  T_STAGE_CATEGORY[ substr(T_Stage,1,3) == "T2c" ] = "4" 
  T_STAGE_CATEGORY[ substr(T_Stage,1,2) == "T3" ] = "4" 
})
cldata$T_STAGE_CATEGORY <- as.numeric(cldata$T_STAGE_CATEGORY)
# CHK : T_STAGE_CATEGORY
#str(cldata$T_STAGE_CATEGORY)
#summary(cldata$T_STAGE_CATEGORY)
#subset(cldata, subset=(is.na(T_STAGE_CATEGORY)))
##########



##########
# Gleason score GROUP(Low(<=6): 1, Intermediate-1(7=3+4): 2, Intermediate-2(7=4+3): 3, High(8-10): 4, GX(none): NA)
head(cldata$GleasonsSum)
cldata<- within( cldata, {
  GLEASONS_CATEGORY = character(0) 
  GLEASONS_CATEGORY[ GleasonsSum <= 6 ] = "1"
  GLEASONS_CATEGORY[ GleasonsSum == 7 & GleasonsFirst == 3 ] = "2"
  GLEASONS_CATEGORY[ GleasonsSum == 7 & GleasonsFirst == 4 ] = "3"
  GLEASONS_CATEGORY[ GleasonsSum >= 8 ] = "4"
})
cldata$GLEASONS_CATEGORY <- as.numeric(cldata$GLEASONS_CATEGORY)
# CHK : GLEASONS_CATEGORY
#str(cldata$GLEASONS_CATEGORY)
#summary(cldata$GLEASONS_CATEGORY)




##########
# PSA level GROUP( Level1(<4.0ng/mL): 1, Level2(4.0-10.0ng/mL): 2, Level3(>10.0ng/mL): 3)
# ncol(cldata): 133
##########
# set start time PSA to PSA_START
# 1. add PSA_START column to cldata
cldata$PSA_START <- 0
TempData <- cldata
# 2. init PSA_START VALUE
## 1. Get unique patient_id list 
uniquePIDlist <- unique(TempData[,c("Patient_Id")])
n <- length(uniquePIDlist)

###########
## Target Dataframe preparation
PSA_START = 0
for(i in 1:n)
{
  subList <- TempData[which(TempData$Patient_Id==uniquePIDlist[i]),]
  PSA_START = subList[1,"PSA"]
  cldata[ which(cldata$Patient_Id == uniquePIDlist[i]), ]$PSA_START <- PSA_START
}


#####
## PSA_START GROUP( Level1(<=10.0ng/mL): 1, Level2(10.0-20.0ng/mL): 2, Level3(>20.0ng/mL): 3)
head(cldata$PSA_START)
cldata<- within( cldata, {
  PSA_LEVEL = character(0) 
  PSA_LEVEL[ PSA_START <= 10 ] = "1"
  PSA_LEVEL[ PSA_START > 10 & PSA_START <= 20 ] = "2"
  PSA_LEVEL[ PSA_START > 20 ] = "3"
  #PSA_LEVEL[ PSA_START < 4 ] = "1"
  #PSA_LEVEL[ PSA_START >= 4 & PSA_START <= 10 ] = "2"
  #PSA_LEVEL[ PSA_START > 10 ] = "3"
})
cldata$PSA_LEVEL <- as.numeric(cldata$PSA_LEVEL)
# CHK : PSA_LEVEL
#str(cldata$PSA_LEVEL)
#summary(cldata$PSA_LEVEL)



##########
# Prostate Clinical Outlook(PCO) Risk Group
# Risk GROUP( Low Risk(Gleason Score<=6 and PSA<=10ng/mL): 1, 
#             Intermediate ==> (PSA>10 and PSA<20) & Gleason <= 6 : 2
#                              Gleason = 7(3+4) : 3
#                              Gleason = 7(4+3) : 4, 
#             
#             High Risk(Gleason>=8 or PSA>20): 5)
# (*Column Name list - PSA: PSA_START, Gleason: GleasonsSum)
##########
head(cldata$PSA_START)
head(cldata$GleasonsSum)
cldata<- within( cldata, {
  RISK_CATEGORY = character(0) 
  RISK_CATEGORY[ GleasonsSum <= 6 & PSA_START <= 10 ] = "1"
  RISK_CATEGORY[ GleasonsSum <= 6 & ( PSA_START > 10 & PSA_START < 20 ) ] = "2"
  RISK_CATEGORY[ GleasonsSum == 7 & GleasonsFirst == 3 ] = "3"
  RISK_CATEGORY[ GleasonsSum == 7 & GleasonsFirst == 4 ] = "4"
  RISK_CATEGORY[ GleasonsSum >= 8 | PSA_START >= 20 ] = "5"
})
cldata$RISK_CATEGORY <- as.numeric(cldata$RISK_CATEGORY)
# CHK : RISK_CATEGORY
#str(cldata$RISK_CATEGORY)
#summary(cldata$RISK_CATEGORY)


##########
# RACE GROUP(A: 1, B: 2, H: 3, W: 4, O(others): 5)
head(cldata$Race)
cldata<- within( cldata, {
  RACE_CATEGORY = character(0) 
  RACE_CATEGORY[ substr(Race,1,1) == "A" ] = "1"
  RACE_CATEGORY[ substr(Race,1,1) == "B" ] = "2"
  RACE_CATEGORY[ substr(Race,1,1) == "H" ] = "3"
  RACE_CATEGORY[ substr(Race,1,1) == "W" ] = "4"
  RACE_CATEGORY[ Race == "O: Filipino" | Race == "indian" | Race == "O-Indian" | Race == "O: Indian" | Race == "O:Indian" | Race == "O:Chinese" | Race == "O:Vietnamese"] = "1"
  RACE_CATEGORY[ Race == "Nigerian" | Race == "O:Nigerian" | Race == "O:Jamaican"] = "2"
  RACE_CATEGORY[ Race == "Ukranian" ] = "4"
  RACE_CATEGORY[ Race == "Egyptian" | Race == "O" | Race == "O: El Salvadorian" | Race == "O:Persian" | Race == "O:Ethopian" | Race == "O - Iranian" | Race == "O- Armenian" | Race == "O:" | Race == "O:Guyanese" | Race == "O:Iranian" | Race == "O:Turkish" | Race == "Other:Kudrish" | Race == "Pakistani" | Race == "Sierra Leone" | Race == "Ghana"] = "5"
 })
cldata$RACE_CATEGORY <- as.numeric(cldata$RACE_CATEGORY)
# CHK : RACE_CATEGORY
#str(cldata$RACE_CATEGORY)
#summary(cldata$RACE_CATEGORY)
#cldata[which(is.na(cldata$RACE_CATEGORY)),]

##########
# DAmico Risk GROUP
#                      PSA          GLEASON        CLINICAL STAGE    CONDITION
# LOW-RISK(3)          <=10         <=6            T1-2a             AND
# INTERMEDIATE-RISK(2) 10-20        7              T2b               NOT(LOW or HIGH RISK)
# HIGH-RISK(1)         >20          >=8            T2c-3a            OR
# (*Column Name list - PSA: PSA_START, Gleason: GleasonsSum, STAGE: T_STAGE_CATEGORY;T1=1,T2a=2,T2b=3,T2c=4,T3=5)
##########
head(cldata$PSA_START)
head(cldata$GleasonsSum)
head(cldata$T_STAGE_CATEGORY)
cldata<- within( cldata, {
  DAMICO_RISK_CATEGORY = character(0) 
  DAMICO_RISK_CATEGORY[ PSA_START <= 10 & GleasonsSum <= 6 & (T_STAGE_CATEGORY <= 2) ] = "1"
  #DAMICO_RISK_CATEGORY[  ] = "2" none of 1 or 2 type
  DAMICO_RISK_CATEGORY[ PSA_START > 20 | GleasonsSum >= 8 | (T_STAGE_CATEGORY >= 4) ] = "3"
})
cldata$DAMICO_RISK_CATEGORY <- as.numeric(cldata$DAMICO_RISK_CATEGORY)
cldata[c("DAMICO_RISK_CATEGORY")][is.na(cldata[c("DAMICO_RISK_CATEGORY")])] <- 2
# CHK : DAMICO_RISK_CATEGORY
#str(cldata$DAMICO_RISK_CATEGORY)
#summary(cldata$DAMICO_RISK_CATEGORY)


##########
# Comprehensive Cancer Network(NCCN) Risk GROUP
#                      PSA          GLEASON        CLINICAL STAGE    CONDITION
# LOW-RISK(3)          <10          2-6            T1-T2a             AND
# INTERMEDIATE-RISK(2) 10-20        7              T2b-T2c            OR
# HIGH-RISK(1)         >20          >=8            T3                 OR
# (*Column Name list - PSA: PSA_START, Gleason: GleasonsSum, STAGE: T_STAGE_CATEGORY;T1=1,T2a=2,T2b=3,T2c=4,T3=5)
##########
head(cldata$PSA_START)
head(cldata$GleasonsSum)
head(cldata$T_STAGE_CATEGORY)
cldata<- within( cldata, {
  NCCN_RISK_CATEGORY = character(0) 
  NCCN_RISK_CATEGORY[ PSA_START < 10 & (GleasonsSum >= 2 & GleasonsSum <= 6) & (T_STAGE_CATEGORY <= 2) ] = "1"
  NCCN_RISK_CATEGORY[ (PSA_START >= 10 & PSA_START <= 20) | GleasonsSum == 7 | (T_STAGE_CATEGORY >= 3 & T_STAGE_CATEGORY <= 4) ] = "2"
  NCCN_RISK_CATEGORY[ PSA_START > 20 | (GleasonsSum >= 8 & GleasonsSum <= 10) | (T_STAGE_CATEGORY >= 5) ] = "3"
})
cldata$NCCN_RISK_CATEGORY <- as.numeric(cldata$NCCN_RISK_CATEGORY)
# CHK : NCCN_RISK_CATEGORY
#str(cldata$NCCN_RISK_CATEGORY)
#summary(cldata$NCCN_RISK_CATEGORY)


###################
# PCa Patient Classfication
# value = AGE_CATEOGY(1=0, 2=33, 3=67, 4=100) + RISK_CATEGORY(1=0, 2=19, 3=38, 4=57, 5=76) + T_STAGE_CATEGORY(1=0, 2=0, 3=1, 4=1, 5=1)
# 0-72 : very low (1)
# 73-106 : low (2)
# 107-125 : Intermediate (3)
# 126-177 : High (4)
###################
cldata<- within( cldata, {
  AGE_CATEGORY_SCORE = character(0)
  AGE_CATEGORY_SCORE[ AGE_CATEGORY == 1 ] = "0"
  AGE_CATEGORY_SCORE[ AGE_CATEGORY == 2 ] = "33"
  AGE_CATEGORY_SCORE[ AGE_CATEGORY == 3 ] = "67"
  AGE_CATEGORY_SCORE[ AGE_CATEGORY == 4 ] = "100"
})
cldata$AGE_CATEGORY_SCORE <- as.numeric(cldata$AGE_CATEGORY_SCORE)

cldata<- within( cldata, {
  RISK_CATEGORY_SCORE = character(0)
  RISK_CATEGORY_SCORE[ RISK_CATEGORY == 1 ] = "0"
  RISK_CATEGORY_SCORE[ RISK_CATEGORY == 2 ] = "13"
  RISK_CATEGORY_SCORE[ RISK_CATEGORY == 3 ] = "26"
  RISK_CATEGORY_SCORE[ RISK_CATEGORY == 4 ] = "40"
  RISK_CATEGORY_SCORE[ RISK_CATEGORY == 5 ] = "53"
})
cldata$RISK_CATEGORY_SCORE <- as.numeric(cldata$RISK_CATEGORY_SCORE)

cldata<- within( cldata, {
  T_STAGE_CATEGORY_SCORE = character(0)
  T_STAGE_CATEGORY_SCORE[ T_STAGE_CATEGORY == 1 ] = "0"
  T_STAGE_CATEGORY_SCORE[ T_STAGE_CATEGORY == 2 ] = "1"
  T_STAGE_CATEGORY_SCORE[ T_STAGE_CATEGORY == 3 ] = "2"
  T_STAGE_CATEGORY_SCORE[ T_STAGE_CATEGORY == 4 ] = "3"
})
cldata$T_STAGE_CATEGORY_SCORE <- as.numeric(cldata$T_STAGE_CATEGORY_SCORE)

cldata$PCAP_SCORE <- (cldata$AGE_CATEGORY_SCORE + cldata$RISK_CATEGORY_SCORE + cldata$T_STAGE_CATEGORY_SCORE)

## Prostate Clinical Outlook(PCO) Classification
cldata<- within( cldata, {
  PCAP_CLASS = character(0)
  PCAP_CLASS[ PCAP_SCORE >= 0 & PCAP_SCORE <= 34 ] = "1"
  PCAP_CLASS[ PCAP_SCORE >= 35 & PCAP_SCORE <= 115 ] = "2"
  PCAP_CLASS[ PCAP_SCORE >= 116 & PCAP_SCORE <= 156 ] = "3"
  
})
cldata$PCAP_CLASS <- as.numeric(cldata$PCAP_CLASS)

