# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

######## plot function
plotfunc <- function(..., vPlotData, vNData) {
  
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
  
  #######################
  # OMIT DATA LESS THAN #
  #######################
  # overall 
  nCount1 <- c(table(vPlotData$Time_Point))
  cond1 <- lapply(nCount1, function(x) x > vNData$MinNum)
  nCount1 <- nCount1[unlist(cond1)]
  nLength1 <- length(nCount1)
  vPlotData <- vPlotData[which(vPlotData$Time_Point %in% c(names(nCount1))),]
  Label1 <- as.data.frame(nCount1)
  Label1$Time_Point <- names(nCount1)
  Label1$Time_Point <- as.numeric(Label1$Time_Point)
  Label1$nCount <- as.numeric(Label1$nCount)
  
  if ( nrow(vPlotData2) >= 1 ) {
    # patient
    nCount2 <- c(table(vPlotData2$Time_Point))
    cond2 <- lapply(nCount2, function(x) x > vNData$MinNum)
    nCount2 <- nCount2[unlist(cond2)]
    nLength2 <- length(nCount2)
    vPlotData2 <- vPlotData2[which(vPlotData2$Time_Point %in% c(names(nCount2))),]
    Label2 <- as.data.frame(nCount2)
    Label2$Time_Point <- names(nCount2)
    Label2$Time_Point <- as.numeric(Label2$Time_Point)
    Label2$nCount <- as.numeric(Label2$nCount)
  }
  #######################
  #######################
  
  if ( nrow(vPlotData2) < 1 ) {
    varmints <- rbind((cbind(vPlotData, section = 'Reference')))
  } else {
    varmints <- rbind((cbind(vPlotData2, section = 'Patient Specific')),
                      (cbind(vPlotData, section = 'Reference')))
  }  
  #######################
  
  p <- ggplot(data = varmints, aes(x = Time_Point, y = (EPIC_Q12_SCORE), color = section)) +
    theme_stata() + labs(color="") + theme(legend.position = "bottom")
  p <- p + 
    labs(x = "", y = "") +
    # title
    ggtitle("Sexual QOL Overall(Average)") +
    # scale
    scale_x_continuous(breaks = seq(0, 19, 1),
                       labels=c("pre-start","start","1 month","3 months","6 months",
                                "9 months","12 months","15 months","18 months","21 months",
                                "2 years","2.5 years","3 years","3.5 years","4 years",
                                "4.5 years","5 years","6 years","6.5 years","7 years")) +
    scale_y_continuous(limits = c(0,100)) +
    # font size
    theme(text = element_text(size=10), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.ticks.x=element_blank(),
          legend.background = element_rect(colour = "white")) +
    # graph
    # overall
    stat_summary(fun.y = "mean", geom = "line") 
  
  
  
  # show plot
  ggplotly(p)
  
} ## end of function

