# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

################################################
# FILE: DiseaseFree_Risk.R
# DESC: BCR free 5 year survival probability (%)
################################################
######## calc function
calcSurvFunc <- function(..., vValues, vNData, vOption = "PSA", vSurvData,
                         vGroupOption = "T-Stage", vFontSize=5, vLFontSize=5, vInput = "") {
  #if ( vNData$T_STAGE_CATEGORY == "5") {
  #  vSurvData2 = vSurvData[which( (vSurvData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
  #                                & (vSurvData$T_STAGE_CATEGORY == 4 | vSurvData$T_STAGE_CATEGORY == 5)
  #                                & (vSurvData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  #} else {
  #  vSurvData2 = vSurvData[which( (vSurvData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
  #                                & (vSurvData$T_STAGE_CATEGORY == vNData$T_STAGE_CATEGORY)
  #                                & (vSurvData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  #}
  vSurvData2 = vSurvData[which (vSurvData$PCAP_CLASS == vNData$PCAP_CLASS),]
  ##############################################
  ##### Kaplan-Meier curve
  require("survminer")
  require("survival")
  fit1 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData)
  prob1 <- summary(fit1, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  result1 <- round(prob1*100,2)
  
  if ( nrow(vSurvData2) >= 1 ) {
    fit2 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData2)
    prob2 <- summary(fit2, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
    result2 <- round(prob2*100,2)
    
    Section <- c("Reference","Patient Specific")
    Probability <- c(result1,result2)
  } else {
    Section <- c("Reference")
    Probability <- c(result1)
  }
  
  riskTable <- data.frame(Section, Probability)
  
  formattable(riskTable, list(
    Section = color_tile("lightgray", "skyblue"),
    Probability = color_bar("yellowgreen","proportion")
    #area(col = c(Probability)) ~ normalize_bar("yellowgreen", 0.2)
  ))
  
}