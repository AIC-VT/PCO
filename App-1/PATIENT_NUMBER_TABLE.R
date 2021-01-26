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
  
  p <- ggplot(data = vPlotData, aes(x = vPlotData$Time_Point, y = (vPlotData$RECTAL_MEAN))) +
    theme_stata() +
    theme(axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())
  p <- p + 
    labs(x = "", y = "") +
    scale_x_continuous(breaks = seq(0, 19, 1),
                       labels=c("pre-start","start","1 month","3 months","6 months",
                                "9 months","12 months","15 months","18 months","21 months",
                                "2 years","2.5 years","3 years","3.5 years","4 years",
                                "4.5 years","5 years","6 years","6.5 years","7 years")) +
    scale_y_continuous(limits = c(0,25)) +
    # font size
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks.x=element_blank()) +
    
    geom_label(data = Label1, aes(x=Time_Point,y=10,label=nCount), color = "black") +
    annotate("text", x = 0, y = 20, label = paste("Patient specific to"), size = 5, color="darkblue", hjust = 0) +
    annotate("text", x = 3, y = 20, label = paste("Reference patients"), size = 5, color="black", hjust = 0)
  
  if ( nrow(vPlotData2) >= 1 ) {
    p <- p + geom_label(data = Label2, aes(x=Time_Point,y=15,label=nCount), color = "darkblue")
  }
  if ( nrow(vPlotData2) < 1 ) {
    p <- p + annotate("text", x = 5, y = 5, label = "No Patient Specific Data Exists")
  }
  # show plot
  p
  
} ## end of function

