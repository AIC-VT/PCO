# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

######## plot function
plotSurvFunc <- function(..., vValues, vNData, vOption = "PSA", vSurvData,  
                         vGroupOption = "T-Stage", vFontSize=5, vLFontSize=5, vInput = "") {
  withProgress(message = 'Survival plot', value = 0, {
    n = 6
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
    incProgress(1/n, detail = paste("Doing part", 1))
    
    ##############################################
    ##### Kaplan-Meier curve
    require("survminer")
    require("survival")
    D <- NA
    if ( nrow(vSurvData2) < 1 ) { 
      D <- rbind((cbind(vSurvData, T = 'Reference')))
    
    } else {
      D <- rbind((cbind(vSurvData2, T = 'Patient Specific')),
                 (cbind(vSurvData, T = 'Reference')))
    }
    incProgress(1/n, detail = paste("Doing part", 2))
    
    ## Generate survival for fit for all D as a function of T
    
    sf_varmints <- with(D,survfit(Surv(SurvDays,isBCR==1) ~ T))
    incProgress(1/n, detail = paste("Doing part", 3))

    p<-ggsurvplot(sf_varmints,risk.table = TRUE,risk.table.height = 0.35,break.time.by = 120,conf.int=FALSE,
                  legend.title ="", risk.table.title = "Number of patients by time periods",
                  surv.scale = "percent", xlab = "", data = D)
    
    p$plot <- p$plot + ggtitle("") +
      theme_stata() 
    p$plot <- p$plot +  
      scale_x_continuous(breaks = c(0, 120, 240, 
                                    360, 480, 600, 
                                    720, 840, 960,
                                    1080, 1200, 1320,
                                    1440, 1560, 1680,
                                    1800, 1920, 2040,
                                    2160, 2280),
                         labels=c("start","4 months","8 months",
                                  "1 year","1 year 4 months","1 year 8 months",
                                  "2 years","2 years 4 months","2 years 8 months",
                                  "3 years","3 years 4 months","3 years 8 months",
                                  "4 years","4 years 4 months","4 years 8 months",
                                  "5 years","5 years 4 months","5 years 8 months",
                                  "6 years","6 years 4 months")) +
      theme(text = element_text(size=vFontSize), axis.text.x = element_text(angle = 45, hjust = 1))
    
    p$plot <- p$plot +
      scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
                                    0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
                         labels=c("0 %", "5 %", "10 %", "15 %", "20 %", "25 %", "30 %", "35 %", "40 %", "45 %", "50 %", "55 %",
                                  "60 %", "65 %", "70 %", "75 %", "80 %", "85 %", "90 %", "95 %", "100 %")) +
      theme(text = element_text(size=vFontSize), axis.text.y = element_text(angle = 0, hjust = 1)) +
      theme(legend.background = element_rect(colour = "white"))

    p$table <- p$table + theme_stata() +
      scale_x_continuous(breaks = c(0, 120, 240, 
                                    360, 480, 600, 
                                    720, 840, 960,
                                    1080, 1200, 1320,
                                    1440, 1560, 1680,
                                    1800, 1920, 2040,
                                    2160, 2280),
                         labels=c("start","4 months","8 months",
                                  "1 year","1 year 4 months","1 year 8 months",
                                  "2 years","2 years 4 months","2 years 8 months",
                                  "3 years","3 years 4 months","3 years 8 months",
                                  "4 years","4 years 4 months","4 years 8 months",
                                  "5 years","5 years 4 months","5 years 8 months",
                                  "6 years","6 years 4 months")) +
      theme(text = element_text(size=vFontSize), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(text = element_text(size=vFontSize), axis.text.y = element_text(angle = 0, hjust = 1))
    
    incProgress(1/n, detail = paste("Doing part", 4))
    
    if ( nrow(vSurvData2) < 1 ) {
      p$plot <- p$plot + annotate("text", x = 360, y = 0.25, label = "No Patient Specific Data Exists")
    }
    incProgress(1/n, detail = paste("Doing part", 5))
  }) # end of progress
  
  print(p)
  incProgress(1/n, detail = paste("Doing part", 6))
}