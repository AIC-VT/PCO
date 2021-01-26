# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

###################
# OverallVSProfile.R #
###################

vsfunc <- function(..., vSurvData, vPlotData, vNData) {
  
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
  
  labs <- c("Disease Free", "Rectal QOL", "Bladdar Risk Free", "Erectile Dysfunction")
  
  
  require("survminer")
  require("survival")
  
  # overall
  fit1 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData)
  
  # report data
  prob1 <- summary(fit1, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  overallDiseaseFree <- round(prob1*100,2)
  
  # Timepoint 16 : 5 year
  source("./App-1/GET_PROFILE_VALUE.R")
  #overallRectalFree <- (round(mean(vPlotData[which(vPlotData$Time_Point == 16),"RECTAL_MEAN"]),2))
  #overallRectalBloodFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q6_D_SCORE"]),2)
  #overallUrinaryFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q5_SCORE"]),2)
  #overallUrinaryBloodFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q4_C_SCORE"]),2)
  #overallErectileDysfunctionFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2)
  overallRectalFree <- getProfileValue(dfSource = vPlotData, profileCondition = "RECTAL_MEAN")
  overallRectalBloodFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q6_D_SCORE")
  overallUrinaryFree <- getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q5_SCORE")
  overallUrinaryBloodFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q4_C_SCORE")
  #overallErectileDysfunctionFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  overallErectileDysfunctionFree <- getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  if ( nrow(vPlotData2) >= 1 ) {
    # patient
    fit2 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData2)
    
    prob2 <- summary(fit2, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
    patientDiseaseFree <- round(prob2*100,2)
    
    #patientRectalFree <- (round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"RECTAL_MEAN"]),2))
    #patientRectalBloodFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q6_D_SCORE"]),2) 
    #patientUrinaryFree <- (round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q5_SCORE"]),2))
    #patientUrinaryBloodFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q4_C_SCORE"]),2) 
    #patientErectileDysfunctionFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2) 
    patientRectalFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "RECTAL_MEAN")
    patientRectalBloodFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q6_D_SCORE")
    patientUrinaryFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q5_SCORE")
    patientUrinaryBloodFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q4_C_SCORE")
    #patientErectileDysfunctionFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
    patientErectileDysfunctionFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
    dfScores <- data.frame(
      Section = factor(c("Reference","Reference","Reference","Reference","Reference","Reference",
                         "Patient Specific","Patient Specific","Patient Specific","Patient Specific","Patient Specific","Patient Specific")),
      Profile = factor(c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)",
                         "Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)"), 
                       levels=c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)")),
      Value = c(overallDiseaseFree, overallRectalFree,overallRectalBloodFree,overallUrinaryFree,overallUrinaryBloodFree,overallErectileDysfunctionFree,
                patientDiseaseFree, patientRectalFree,patientRectalBloodFree,patientUrinaryFree,patientUrinaryBloodFree,patientErectileDysfunctionFree)
    )
  } else {
    dfScores <- data.frame(
      Section = factor(c("Reference","Reference","Reference","Reference","Reference","Reference")),
      Profile = factor(c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)"), 
                       levels=c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)")),
      Value = c(overallDiseaseFree, overallRectalFree,overallRectalBloodFree,overallUrinaryFree,overallUrinaryBloodFree,overallErectileDysfunctionFree)
    )
    
  }
  
  dfScores$Section <- factor(dfScores$Section, levels = c("Reference","Patient Specific"))
  dfScores$Profile <- factor(dfScores$Profile, levels = c("Relapse Free Survival(+)",
                                                          "Rectal QOL(+)","Urinary QOL(+)","Sexual QOL(+)",
                                                          "Rectal Bleeding(-)", "Urinary Bleeding(-)"
                                                          ))
  p <- ggplot(data=dfScores, aes(x=Profile, y=Value, fill=Section)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c("#2F8BAF","yellowgreen")) +
    theme(text = element_text(size=10), 
          axis.text.x = element_text(angle = -40, hjust = 10, vjust = 100),
          axis.ticks.x=element_blank())
  
  p <- p + labs(x = "", y = "") + 
    theme(legend.position=c(0,0),
          legend.justification=c(0,0),
          legend.title = element_blank())

  p <- p + coord_fixed(ratio = 0.2)
  ggplotly(p)
}


vsfunc2 <- function(..., vSurvData, vPlotData, vNData) {
  
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
  
  labs <- c("Disease Free", "Rectal QOL", "Bladdar Risk Free", "Erectile Dysfunction")
  
  
  require("survminer")
  require("survival")
  
  # overall
  fit1 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData)
  
  # report data
  prob1 <- summary(fit1, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
  overallDiseaseFree <- round(prob1*100,2)
  
  # Timepoint 16 : 5 year
  source("./App-1/GET_PROFILE_VALUE.R")
  #overallRectalFree <- (round(mean(vPlotData[which(vPlotData$Time_Point == 16),"RECTAL_MEAN"]),2))
  #overallRectalBloodFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q6_D_SCORE"]),2)
  #overallUrinaryFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q5_SCORE"]),2)
  #overallUrinaryBloodFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"EPIC_Q4_C_SCORE"]),2)
  #overallErectileDysfunctionFree <- round(mean(vPlotData[which(vPlotData$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2)
  overallRectalFree <- getProfileValue(dfSource = vPlotData, profileCondition = "RECTAL_MEAN")
  overallRectalBloodFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q6_D_SCORE")
  overallUrinaryFree <- getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q5_SCORE")
  overallUrinaryBloodFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "EPIC_Q4_C_SCORE")
  #overallErectileDysfunctionFree <- 100-getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  overallErectileDysfunctionFree <- getProfileValue(dfSource = vPlotData, profileCondition = "SEXUAL_QOL_MEAN")
  if ( nrow(vPlotData2) >= 1 ) {
    # patient
    fit2 <- survfit(Surv(SurvDays,isBCR==1)~1, data=vSurvData2)
    
    prob2 <- summary(fit2, times = 1800, censored=TRUE, scale=1, extend=TRUE, rmean=getOption('survfit.rmean'))$surv
    patientDiseaseFree <- round(prob2*100,2)
    
    #patientRectalFree <- (round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"RECTAL_MEAN"]),2))
    #patientRectalBloodFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q6_D_SCORE"]),2) 
    #patientUrinaryFree <- (round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q5_SCORE"]),2))
    #patientUrinaryBloodFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"EPIC_Q4_C_SCORE"]),2) 
    #patientErectileDysfunctionFree <- round(mean(vPlotData2[which(vPlotData2$Time_Point == 16),"SEXUAL_QOL_MEAN"]),2) 
    patientRectalFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "RECTAL_MEAN")
    patientRectalBloodFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q6_D_SCORE")
    patientUrinaryFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q5_SCORE")
    patientUrinaryBloodFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "EPIC_Q4_C_SCORE")
    #patientErectileDysfunctionFree <- 100-getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
    patientErectileDysfunctionFree <- getProfileValue(dfSource = vPlotData2, profileCondition = "SEXUAL_QOL_MEAN")
    dfScores <- data.frame(
      Section = factor(c("Reference","Reference","Reference","Reference","Reference","Reference",
                         "Patient Specific","Patient Specific","Patient Specific","Patient Specific","Patient Specific","Patient Specific")),
      Profile = factor(c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)",
                         "Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)"), 
                       levels=c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)")),
      Value = c(overallDiseaseFree, overallRectalFree,overallRectalBloodFree,overallUrinaryFree,overallUrinaryBloodFree,overallErectileDysfunctionFree,
                patientDiseaseFree, patientRectalFree,patientRectalBloodFree,patientUrinaryFree,patientUrinaryBloodFree,patientErectileDysfunctionFree)
    )
  } else {
    dfScores <- data.frame(
      Section = factor(c("Reference","Reference","Reference","Reference","Reference","Reference")),
      Profile = factor(c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)"), 
                       levels=c("Relapse Free Survival(+)","Rectal QOL(+)","Rectal Bleeding(-)","Urinary QOL(+)","Urinary Bleeding(-)","Sexual QOL(+)")),
      Value = c(overallDiseaseFree, overallRectalFree,overallRectalBloodFree,overallUrinaryFree,overallUrinaryBloodFree,overallErectileDysfunctionFree)
    )
    
  }
  
  dfScores$Section <- factor(dfScores$Section, levels = c("Reference","Patient Specific"))
  dfScores$Profile <- factor(dfScores$Profile, levels = c("Relapse Free Survival(+)",
                                                          "Rectal QOL(+)","Urinary QOL(+)","Sexual QOL(+)",
                                                          "Rectal Bleeding(-)", "Urinary Bleeding(-)"
  ))

  p <- ggplot(data=dfScores, aes(x=Profile, y=Value, fill=Section)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c("#2F8BAF", "yellowgreen")) +
    ggtitle("Relapse Free Suvival Probability - at 5 years") +
    theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust=0.9))
  return(p)
}