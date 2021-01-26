# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

####################################################
# PSA_TREND_TO_PDF.R
####################################################

require(ggplot2)

uniquePIDlist <- unique(cldata[,c("Patient_Id")])
n <- length(uniquePIDlist)
plotList <- vector('list', n)

for ( i in 1:n ) {

  plotdata1 <- cldata[which(cldata$Patient_Id==uniquePIDlist[i] & !is.na(cldata$PSA)),]
  
  # nadir point from R
  nadir1 <- plotdata1[which(plotdata1$PSA == min(plotdata1$PSA)),]
  nadir1 <- nadir1[1,]
  # PSA Doubling Point in EXCEL
  dbPoint1 <- plotdata1[which(plotdata1$PSA.Doubling == 1),]
  NdbPoint1 <- nrow(dbPoint1)
  # PSA Doubling Point from R
  dbPointR1 <- plotdata1[which(plotdata1$PSA >= (min(plotdata1$PSA)*2) & plotdata1$Time_Point > nadir1$Time_Point),]
  NdbPointR1 <- nrow(dbPointR1)
  
  # confirm PSA Doubling Point in EXCEL and R are same.
  dbPointR1[,c("PSA","Time_Point")]
  dbPoint1[,c("PSA","Time_Point")]
  
  #####################
  ## plot time_point ##
  #####################
  ### plot start ###
  p <- ggplot(cldata, aes(x=Time_Point, y = PSA)) +
    ggtitle(uniquePIDlist[i]) +
    # plotdata1
    geom_line(data = plotdata1, color="darkgray") +
    geom_text(data = plotdata1, label = plotdata1$PSA, position=position_jitter(h=0,w=0), size = 5, color="red") +
    # scale x
    scale_x_continuous(limits=c(0, 19),breaks = seq(0, 19, 1),labels=c("0start","start","1month","3month","6month","9month","12month","15month","18month","21month","2year","2.5year","3year","3.5year","4year","4.5year","5year","6year","6.5year","7year")) +
    
    geom_jitter(data = nadir1, aes(x=Time_Point, y = PSA), position=position_jitter(h=0,w=0), color="red") +
    geom_jitter(data = dbPoint1, aes(x=Time_Point, y = PSA), position=position_jitter(h=0,w=0), color="black", size=3) +
    geom_jitter(data = dbPointR1, aes(x=Time_Point, y = PSA), position=position_jitter(h=0,w=0), color="orange")
  p
  
  plotList[[i]] <- p
  ### plot end ###
  #####################################################################################################
  
}
graphics.off()

pdf(file="PSA_TREND_LIST.pdf", width = 12, onefile = TRUE)
plotList
dev.off()
