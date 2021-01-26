################################################################################
# File: UnitTest.R
# Desc: Test Data Management Function
################################################################################
rm(list=ls())
homedir = "/srv/shiny-server/PCO"
setwd(homedir)
suppressMessages(source(file="./App-1/config.R"))

args <- commandArgs(trailingOnly = TRUE)
srcFile <- args[1]
testType <- as.numeric(args[2])
testStep <- as.numeric(args[3])
testTarget=paste0("./App-1/", srcFile)

load=paste0("./App-1/load.R")
save=paste0("./App-1/SAVE_CLDATA.R")

###############################################
# load survival data
# connect to mysql
require("RMySQL")
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
survData <- dbReadTable(mydb, name = 'TB_CYBERKNIFE_SURVIVAL')
dbDisconnect(mydb)
###############################################
survData$PSA_LEVEL <- as.numeric(survData$PSA_LEVEL)
survData$T_STAGE_CATEGORY <- as.numeric(survData$T_STAGE_CATEGORY)
survData$RISK_CATEGORY <- as.numeric(survData$RISK_CATEGORY)
survData$AGE_CATEGORY <- as.numeric(survData$AGE_CATEGORY)
###############################################

###############################################
# set test input data
testInput <- data.frame(Doctor = 'DoctorName',
                        Name = 'PatientName',
                        AGE = 60,
                        T_Stage = 'T1',
                        GleasonsSum = '3+4=7',
                        PSA = 10,
                        AGE_CATEGORY = 2,
                        T_STAGE_CATEGORY = 1,
                        PSA_LEVEL = 1,
                        RISK_CATEGORY = 1,
                        PCAP_CLASS = 1,
                        MinNum <- 5 )
nData <- function(){
    return(list(df=testInput))
}
###############################################
testFunc <- function() {
	tryCatch({
        if (testType == 1) {# Data Management Test
            if (testStep >= 3) 
                source(file=load)
	        source(file=testTarget)
            if (testStep >= 2)
                source(file=save)
        } # end of Data Management Test
        else if (testType == 2) {# Data Visualization Test 
            suppressMessages(source(file=load))
            testValues = NA
            suppressMessages(source("./App-1/GET_PROFILE_VALUE.R"))
            suppressMessages(source("./App-1/GET_PCAP_CLASS.R"))
            score = getPCaPScore(pAGE_CATEGORY = 1,
                                 pRISK_CATEGORY = 1,
                                 pT_STAGE_CATEGORY = 1)
            if (testStep == 1) {
                suppressMessages(source(file=testTarget))
                suppressMessages(riskGaugeFunc(nValue = score))
            } else if ( testStep == 2) {
                suppressMessages(source(file=testTarget))
                suppressMessages(vsfunc(vSurvData = survData, vPlotData = cldata, vNData = testInput))
            } else if ( testStep == 3) {
                suppressMessages(source(file=testTarget))
                suppressMessages(calcSurvFunc(vValues = testValues,
                            vNData = testInput,
                            vSurvData = survData))
            }

        } # end of Data Visualization Test 
        else if (testType == 3) {# Data Report Generation Test 
            suppressMessages(source(file=load))
            values = list()
            values$df <- as.data.frame(cldata)
            suppressMessages(source("./App-1/GET_PROFILE_VALUE.R"))
            suppressMessages(source("./App-1/GET_PCAP_CLASS.R"))
            src <- normalizePath(testTarget)
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, srcFile, overwrite = TRUE)
            library(rmarkdown)
            out <- render(srcFile, pdf_document())
        } # end of Data Report Generation Test 

    },
	error=function(cond, f, n) {
        message(paste("============================================="))
	    message(paste("Error on: ", srcFile))
	    message("Here's the orginal error message:")
        message(cond) 
        message(cond$call)
        #message(paste(traceback(1, max.lines = 1)))
	    #return(NA)
	},
	warning=function(cond) {
        message(paste("============================================="))
	    message(paste("Warning on: ", srcFile))
	    message("Here's the orginal warning message:")
	    message(cond)
	    #return(NA)
	},
    finally={
        message(paste(""))
	    message(paste("[Done] Unit Test For: ", srcFile))
        message(paste("============================================="))
    }
    ) # end of tryCatch
}

testFunc();
