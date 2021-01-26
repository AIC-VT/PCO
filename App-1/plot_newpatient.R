# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

######## plot function
plotfunc <- function(..., vValues, vNData, vOption = "PSA", 
                     vGroupOption = "T-Stage", vFontSize=5, vLFontSize=5) {
  
  #### search w/ subset Patient_Id
  plotValues <- vValues$df
  sdata <- vNData
  #for ( i in c(1:16,16.5,17,17.5))
  for ( i in c(-0.5,18))
  {
    sdata = rbind(sdata,c(sdata[1,1:4],Time_Point=i))
  }
  
  
  data.labels <- data.frame(
    grpText = c("grpText"),
    label = c("Data Group : ")
  )
  
  ## Group Option
  if(vGroupOption == "T-Stage")
  {
    plotValues <- subset(plotValues, subset = (substr(T_Stage,1,2) == sdata$T_Stage[1]))
    data.labels$grpText <- "T-Stage: "
    data.labels$label <- toString(sdata$T_Stage)
  }
  else if (vGroupOption == "GleasonsSum")
  {
    plotValues <- subset(plotValues, subset=(GleasonsSum %in% sdata$GleasonsSum[1]))
    data.labels$grpText <- "Gleasons: "
    data.labels$label <- toString(sdata$GleasonsSum)
  }


 
  ## PSA Plot
  if(vOption == "PSA")
  {
    plotValues <- subset(plotValues, subset=(PSA != "nd" & !is.na(PSA) & PSA != "<0.1")) # PSA "<0.1" 처리 결정 필요
    height <- max(c(sdata$PSA+2,mean(plotValues$PSA+2)))
   
    # when using ui geom* function added by check the option on the web ui
    p <- ggplot(data = plotValues, aes(x = plotValues$Time_Point, y = plotValues$PSA)) 
    
    p <- p + 
      labs(x = "Time", y = "Average PSA") +
      theme_bw() +
      # font size
      theme(text = element_text(size=vFontSize), axis.text.x = element_text(angle = 45, hjust = 1)) +
      # title
      ggtitle("PSA Progression(Average)") +
      # scale
      scale_y_continuous(limits = c(0, height)) +
      scale_x_continuous(breaks = seq(0, 19, 1),
                         labels=c("0start","start","1month","3month","6month",
                                  "9month","12month","15month","18month","21month",
                                  "2year","2.5year","3year","3.5year","4year",
                                  "4.5year","5year","6year","6.5year","7year")) +
      
      # graph
      stat_summary(fun.y = "mean", geom = "bar", fill = "skyblue") +

      # New Patient Line
      geom_line(data = sdata, aes(sdata$Time_Point, sdata$PSA), linetype=2, col = 'darkblue', size = 1) +
      
      # legend
      annotate("text", x = 11.8, y = sdata$PSA+0.5, label = paste("Patient's PSA: ",sdata$PSA[1]), 
               size = vLFontSize, fontface = "bold", color="darkblue", hjust = 0)
      
    # show plot
    p
  } ## End of PSA Plot

 
  
} ## end of function

