# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

################################################
# FILE: SurvivalCalculator_Overall.R
# DESC: print 5 year survival probability (%)
################################################
######## calc function
calcSurvFunc <- function(..., vValues, vNData, vOption = "PSA", vSurvData,
                         vGroupOption = "T-Stage", vFontSize=5, vLFontSize=5, vInput = "") {

  ##############################################
  ##### Kaplan-Meier curve
  require("survminer")
  require("survival")
  fit <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData)
  
  # report data
  prob <- summary(fit, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  result <- (paste("Disease-free survival(Overall 5 years) : ",round(prob*100,2),"%"))
  
  return ( result )
}