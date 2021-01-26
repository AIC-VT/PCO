# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

####################################################
# BCR_estimate_list.R
####################################################
require(ggplot2)
plotPSA <- function(..., vPatient_Id = "IM-001") {
  
    #plotdata1 <- cldata[which(cldata$Patient_Id==vPatient_Id & !is.na(cldata$PSA)),]
    plotdata1 <- ckdata[which(ckdata$Patient_Id==vPatient_Id & !is.na(ckdata$PSA)),]
    
    # nadir point from R
    nadir1 <- plotdata1[which(plotdata1$PSA == min(plotdata1$PSA)),]
    nadir1 <- nadir1[1,]
    # PSA Doubling Point in EXCEL
    dbPoint1 <- plotdata1[which(plotdata1$PSA.Doubling == 1),]
    NdbPoint1 <- nrow(dbPoint1)
    # PSA Doubling Point from R
    dbPointR1 <- plotdata1[which(plotdata1$Time_Point > nadir1$Time_Point),]
    NdbPointR1 <- nrow(dbPointR1)
    
    # confirm PSA Doubling Point in EXCEL and R are same.
    #dbPointR1[,c("PSA","Time_Point")]
    #dbPoint1[,c("PSA","Time_Point")]
    bcrPoint <- data.frame(x=0,y=0)
    dbN <- nrow(dbPointR1)
    nPoint <- 1
    if ( dbN >= 3 ) {
      nTimePoint <- as.numeric(dbPointR1[1,"Time_Point"])
      # check isBCR
      for(j in 1:(dbN-1) ) {
        if ( (as.numeric(dbPointR1[j+1,"PSA"]) - as.numeric(dbPointR1[j,"PSA"])) > 0 & as.numeric(dbPointR1[j,"PSA"]) >= 0.5 ) {
          nPoint <- nPoint + 1
        } else {
          nPoint <- 1
          nTimePoint <- as.numeric(dbPointR1[j+1,"Time_Point"])
        }
      } # end of inner for
      
      if ( nPoint >= 3 ) { # is BCR
        # get survData for BCR
        endDate <- nTimePoint
        startDate <- nadir1[1,"Time_Point"]
        delta <- (endDate - startDate)/2
        bcrPoint$x = (startDate + delta)
      }
    }
    #####################
    ## plot time_point ##
    #####################
    ### plot start ###
    p <- ggplot(cldata, aes(x=Time_Point, y = PSA))
    #p <- ggplot(ckdata, aes(x=Time_Point, y = PSA))
    p <- p +
      # plotdata1
      geom_line(data = plotdata1, color="darkgray") +
      
      # nadir
      geom_jitter(data = nadir1, aes(x=nadir1$Time_Point, y = nadir1$PSA), position=position_jitter(h=0,w=0), color="red") +
      annotate("segment", x=nadir1$Time_Point, xend=nadir1$Time_Point, y=nadir1$PSA+2, yend=nadir1$PSA, size=1, colour="red", arrow=arrow(10) ) +
      annotate("text", x=nadir1$Time_Point, y=nadir1$PSA+2.5, size=6, colour="red", label="Nadir PSA")
      if( nrow(dbPoint1) >= 1 ) {
        # PSA.Doubling
        p <- p + geom_jitter(data = dbPoint1, aes(x=dbPoint1$Time_Point, y = dbPoint1$PSA), position=position_jitter(h=0,w=0), color="black", size=3) +
          annotate("segment", x=dbPoint1$Time_Point-1, xend=dbPoint1$Time_Point, y=dbPoint1$PSA+4, yend=dbPoint1$PSA, size=1, colour="black", arrow=arrow(10) ) +
          annotate("text", x=dbPoint1[1,"Time_Point"]-1, y=dbPoint1[1,"PSA"]+4.5, size=6, colour="black", label="PSA.Doubling")
      }
      #if ( nrow(dbPointR1) >= 1 ) {
      #  # PSA.Doubling(Caculated)
      #  p <- p + geom_jitter(data = dbPointR1, aes(x=dbPointR1$Time_Point, y = dbPointR1$PSA), position=position_jitter(h=0,w=0), color="orange") +
      #    annotate("segment", x=dbPointR1$Time_Point+2, xend=dbPointR1$Time_Point, y=dbPointR1$PSA+6, yend=dbPointR1$PSA, size=1, colour="orange", arrow=arrow(10) ) +
      #    annotate("text", x=dbPointR1[1,"Time_Point"]+2, y=dbPointR1[1,"PSA"]+6.5, size=6, colour="orange", label="PSA.Doubling(Calculated)")
      #}
      if ( bcrPoint$x > 0 ) {
        # PSA.Doubling(Caculated)
        p <- p + 
          annotate("segment", x=bcrPoint$x, xend=bcrPoint$x, y=bcrPoint$y+10, yend=bcrPoint$y, size=1, colour="blue", arrow=arrow(10) ) +
          annotate("text", x=bcrPoint$x, y=bcrPoint$y+10.5, size=6, colour="blue", label="BCR(ASTRO)") +
          annotate("segment", x=nTimePoint, xend=nTimePoint, y=11, yend=0, size=1, colour="red", arrow=arrow(10) ) +
          annotate("text", x=nTimePoint, y=11.5, size=6, colour="red", label="PSA Rising the 1st Point")
      }
      # scale x
    p <- p + geom_text(data = plotdata1, label = plotdata1$PSA, position=position_jitter(h=0,w=0), size = 5, color="blue") + 
      scale_x_continuous(limits=c(0, 19),breaks = seq(0, 19, 1),labels=c("0start","start","1month","3month","6month","9month","12month","15month","18month","21month","2year","2.5year","3year","3.5year","4year","4.5year","5year","6year","6.5year","7year"))
    p
    ### plot end ###
    #####################################################################################################
 
}