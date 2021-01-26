# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

## load.R : load data from mysql
require("RMySQL")
# connect DB
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
ckdata <- dbReadTable(mydb, name = 'TB_CYBERKNIFE')
cldata <- dbReadTable(mydb, name = 'TB_CYBERKNIFE_CLEAN')
dbDisconnect(mydb)

# char to numeric
# cldata$Time_Point = as.numeric(cldata$Time_Point)
# cldata$PSA = as.numeric(cldata$PSA)
# 
# cldata$GleasonsFirst = as.numeric(cldata$GleasonsFirst)
# cldata$GleasonsSecond = as.numeric(cldata$GleasonsSecond)
# cldata$GleasonsSum = as.numeric(cldata$GleasonsSum)
# 
# cldata$PSA.Doubling <- as.numeric(cldata$PSA.Doubling)
# 
# cldata$AGE_CATEGORY <- as.numeric(cldata$AGE_CATEGORY)
# cldata$T_STAGE_CATEGORY <- as.numeric(cldata$T_STAGE_CATEGORY)
# cldata$GLEASONS_CATEGORY <- as.numeric(cldata$GLEASONS_CATEGORY)
# cldata$PSA_LEVEL <- as.numeric(cldata$PSA_LEVEL)
# cldata$RISK_CATEGORY <- as.numeric(cldata$RISK_CATEGORY)
# cldata$RECTAL_MEAN <- as.numeric(cldata$RECTAL_MEAN)
# cldata$URINARY_QOL_INCONTINENCE_MEAN <- as.numeric(cldata$URINARY_QOL_INCONTINENCE_MEAN)
# cldata$URINARY_QOL_IRRITATION_MEAN <- as.numeric(cldata$URINARY_QOL_IRRITATION_MEAN)
# 
# write.xlsx(cldata,"./Data/PreprocessingData.xlsx")

