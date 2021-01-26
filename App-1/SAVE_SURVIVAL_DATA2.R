# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

#########################################
## 0. Data preparation for suvival analysis
TempData <- cldata[which(!is.na(cldata$PSA)),]
TempData$SurvDays <- 0
TempData$startDate <- NA
TempData$endDate <- NA
TempData$isBCR <- 0
## 1. Get unique patient_id list 
uniquePIDlist <- unique(TempData[,c("Patient_Id")])
#uniquePIDlist <- c("IM-059","IM-078","IM-114","IM-122","IM-260","IM-281","IM-329")
n <- length(uniquePIDlist)

num_col <- ncol(cldata)
## Target Dataframe preparation
TgtData <- data.frame(matrix(c(1:num_col),nrow=1))
TgtData$SurvDays <- 0 # survival days
TgtData$startDate <- NA # start date
TgtData$endDate <- NA # end date
TgtData$isBCR <- 0 # BCR or NOT
names(TgtData) <- names(TempData)
TgtData[,c(1:num_col)] <- NA
ncol(TgtData)
for(i in 1:n)
{
  subList <- TempData[which(TempData$Patient_Id==uniquePIDlist[i]),]
  #subList <- TempData[which(TempData$Patient_Id==uniquePIDlist[1]),]
  m <- nrow(subList)
  startDate <- subList[1,"Date"]
  # 1. get nadir point
  nadirPoint <- subList[which(subList$PSA == min(subList$PSA)),]
  nadirPoint <- nadirPoint[1,]
  # 2. get PSA Values (>= 0.5)
  dbPoint <- subList[which( (subList$Time_Point > nadirPoint$Time_Point) ),]
  # 3. get PSA doubling counts
  dbN <- nrow(dbPoint)
  nPoint <- 1
  if ( dbN >= 3 ) {
    # check isBCR
    nTimePoint <- 1
    for(j in 1:(dbN-1) ) {
      if ( (as.numeric(dbPoint[j+1,"PSA"]) - as.numeric(dbPoint[j,"PSA"])) > 0 & as.numeric(dbPoint[j,"PSA"]) >= 0.5 ) {
        nPoint <- nPoint + 1
        
      } else {
        nPoint <- 1
        nTimePoint <- j+1
      }
      
    } # end of inner for
    
    
    if ( nPoint >= 3 ) { # is BCR
      # get survData for BCR
      endDate <- dbPoint[nTimePoint,"Date"]
      subList[m,"startDate"] <- startDate
      subList[m,"endDate"] <- endDate
      subList[m,"SurvDays"] <- round(as.numeric(unclass(as.Date(endDate)) - unclass(as.Date(startDate))))
      subList[m,"isBCR"] <- 1
      TgtData = rbind(TgtData, subList[m,])
    } else { # none BCR
      # get survData for none BCR
      endDate <- subList[m,"Date"]
      subList[m,"startDate"] <- startDate
      subList[m,"endDate"] <- endDate
      subList[m,"SurvDays"] <- round(as.numeric(unclass(as.Date(endDate)) - unclass(as.Date(startDate))))
      subList[m,"isBCR"] <- 0
      TgtData = rbind(TgtData, subList[m,])
    }
    
  } else {
    # get survData for none BCR
    endDate <- subList[m,"Date"]
    subList[m,"startDate"] <- startDate
    subList[m,"endDate"] <- endDate
    subList[m,"SurvDays"] <- round(as.numeric(unclass(as.Date(endDate)) - unclass(as.Date(startDate))))
    subList[m,"isBCR"] <- 0
    TgtData = rbind(TgtData, subList[m,])
  }

}
TgtData <- TgtData[!is.na(TgtData$Patient_Id),]
# GleasonsSum Column add
cldata$PSA.Doubling = as.numeric(cldata$PSA.Doubling)
cldata$GleasonsFirst = as.numeric(cldata$GleasonsFirst)
cldata$GleasonsSecond = as.numeric(cldata$GleasonsSecond)
cldata$GleasonsSum = as.numeric(cldata$GleasonsSum)
cldata$AGE_CATEGORY = as.numeric(cldata$AGE_CATEGORY)

# write xlsx
write.xlsx(TgtData,"./Data/PSA_Survival_Data.xlsx")

################################################################################################################
#survData <- TgtData[,c("Patient_Id","SurvDays","PSA.Doubling","AGE_CATEGORY","T_STAGE_CATEGORY","GLEASONS_CATEGORY",
#                       "PSA_LEVEL","RISK_CATEGORY","isBCR","EXCEL_RISK","PCAP_CLASS","DAMICO_RISK_CATEGORY","PCAP_CLASS")] # only numbers
survData <- TgtData
################################################################################################################
# date conversion
survData[,c("DOB")] <- convertToDate(survData[,c("DOB")]) 
survData[,c("Date")] <- convertToDate(survData[,c("Date")]) 
###############################################
# save survival data
# connect to mysql
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
# init mysql table-cleansing
dbSendQuery(mydb, "drop table if exists TB_CYBERKNIFE_SURVIVAL;")
dbWriteTable(mydb, 'TB_CYBERKNIFE_SURVIVAL', survData, row.names=F)
dbDisconnect(mydb)
###############################################
