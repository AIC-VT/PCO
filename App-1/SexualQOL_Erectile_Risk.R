# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

######## plot function
riskfunc <- function(..., vPlotData, vNData) {
  source("./App-1/GET_PROFILE_VALUE.R")
  #if ( vNData$T_STAGE_CATEGORY == "5") {
  #  vPlotData2 = vPlotData[which( (vPlotData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
  #                                & (vPlotData$T_STAGE_CATEGORY == 4 | vPlotData$T_STAGE_CATEGORY == 5)
  #                                & (vPlotData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  #} else {
  #  vPlotData2 = vPlotData[which( (vPlotData$AGE_CATEGORY == vNData$AGE_CATEGORY) 
  #                                & (vPlotData$T_STAGE_CATEGORY == vNData$T_STAGE_CATEGORY)
  #                                & (vPlotData$RISK_CATEGORY == vNData$RISK_CATEGORY) ),]
  #}
  vPlotData2 = vPlotData[which (vPlotData$PCAP_CLASS == vNData$PCAP_CLASS),]
  if ( nrow(vPlotData2) >= 1 ) {
    # patient
    nCount2 <- c(table(vPlotData2$Time_Point))
    cond2 <- lapply(nCount2, function(x) x > vNData$MinNum)
    nCount2 <- nCount2[unlist(cond2)]
    vPlotData2 <- vPlotData2[which(vPlotData2$Time_Point %in% c(names(nCount2))),]
  }
  
  # Timepoint 16 : 5 year
  #overall <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  #patient <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
  overall <- getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  patient <- getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
  
  Section <- c("Reference","Patient Specific")
  Probability <- c(overall,patient)
  riskRectal <- data.frame(Section, Probability)
  
  #overall <- round(100 - mean(vPlotData[which(vPlotData$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2)
  #if ( nrow(vPlotData2) >= 1 & length(vPlotData2[which(vPlotData2$Time_Point == 16),"SEXUAL_QOL_MEAN"]) >= 1 ) {
  #  patient <- round(100 - mean(vPlotData2[which(vPlotData2$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2)
  #  Section <- c("Overall","Patient Specific Data")
  #  Probability <- c(overall,patient)
  #  riskRectal <- data.frame(Section, Probability)
  #} else {
  #  Section <- c("Overall")
  #  Probability <- c(overall)
  #  riskRectal <- data.frame(Section, Probability)
  #}
  formattable(riskRectal, list(
    Section = color_tile("lightgray", "skyblue"),
    Probability = color_bar("yellowgreen","proportion")
    #area(col = c(Probability)) ~ normalize_bar("yellowgreen", 0.2)
  ))
  
} ## end of function

