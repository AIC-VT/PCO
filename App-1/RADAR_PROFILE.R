# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

###################
# RADAR_PROFILE.R #
###################

radarfunc <- function(..., vSurvData, vPlotData, vNData) {
  
  vSurvData2 = vSurvData[which( (vSurvData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
                                & (vSurvData$T_STAGE_CATEGORY == vNData$T_STAGE_CATEGORY)
                                & (vSurvData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  
  vPlotData2 = vPlotData[which( (vPlotData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
                                & (vPlotData$T_STAGE_CATEGORY == vNData$T_STAGE_CATEGORY)
                                & (vPlotData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  
  
  labs <- c("Disease Free", "Rectal Risk Free", "Bladdar Risk Free", "Erectile Dysfunction Free")
  
  
  require("survminer")
  require("survival")
  fit1 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData)
  fit2 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData2)
  
  # report data
  prob1 <- summary(fit1, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  overallDiseaseFree <- round(prob1*100,2)
  
  prob2 <- summary(fit2, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  patientDiseaseFree <- round(prob2*100,2)
  
  # Timepoint 16 : 5 year
  overallRectalFree <- (round(mean(vPlotData[which(vPlotData$Time_Point == 16),"RECTAL_MEAN"]),2))
  patientRectalFree <- (round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"RECTAL_MEAN"]),2))
  
  scores <- list(
    "Overall" = c(overallDiseaseFree, overallRectalFree, 0, 0),
    "Patient" = c(patientDiseaseFree, patientRectalFree, 0, 0)
  )
  chartJSRadar(scores = scores, labs = labs, maxScale = 100, showToolTipLabel=TRUE)

}