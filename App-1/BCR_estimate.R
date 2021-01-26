# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

####################################################
# BCR_estimate.R
####################################################

cldata$nadir.PSA
class(cldata$nadir.PSA)
cldata[which(cldata$Patient_Id=="IM-013"),c("nadir.PSA","Time_Point")]

require(ggplot2)

plotdata1 <- cldata[which(cldata$Patient_Id=="IM-078" & !is.na(cldata$PSA)),]
plotdata2 <- cldata[which(cldata$Patient_Id=="IM-122" & !is.na(cldata$PSA)),]

# nadir point from R
nadir1 <- plotdata1[which(plotdata1$PSA == min(plotdata1$PSA)),]
nadir1 <- nadir1[1,]
nadir2 <- plotdata2[which(plotdata2$PSA == min(plotdata2$PSA)),]
nadir2 <- nadir2[1,]
# PSA Doubling Point in EXCEL
dbPoint1 <- plotdata1[which(plotdata1$PSA.Doubling == 1),]
NdbPoint1 <- nrow(dbPoint1)
dbPoint2 <- plotdata2[which(plotdata2$PSA.Doubling == 1),]
NdbPoint2 <- nrow(dbPoint2)
# PSA Doubling Point from R
dbPointR1 <- plotdata1[which(plotdata1$PSA >= (min(plotdata1$PSA)*2) & plotdata1$Time_Point > nadir1$Time_Point),]
NdbPointR1 <- nrow(dbPointR1)
dbPointR2 <- plotdata2[which(plotdata2$PSA >= (min(plotdata2$PSA)*2) & plotdata2$Time_Point > nadir2$Time_Point),]
NdbPointR2 <- nrow(dbPointR2)

# confirm PSA Doubling Point in EXCEL and R are same.
dbPointR1[,c("PSA","Time_Point")]
dbPoint1[,c("PSA","Time_Point")]
dbPointR2[,c("PSA","Time_Point")]
dbPoint2[,c("PSA","Time_Point")]

#####################
## plot time_point ##
#####################
### plot start ###
p <- ggplot(cldata, aes(x=Time_Point, y = PSA))
p <- p +
  # plotdata1: IM-013
  geom_line(data = plotdata1, color="darkgray") +
  geom_jitter(data = nadir1, aes(x=nadir1$Time_Point, y = nadir1$PSA), position=position_jitter(h=0,w=0), color="red") +
  geom_jitter(data = dbPoint1, aes(x=dbPoint1$Time_Point, y = dbPoint1$PSA), position=position_jitter(h=0,w=0), color="black", size=3) +
  geom_jitter(data = dbPointR1, aes(x=dbPointR1$Time_Point, y = dbPointR1$PSA), position=position_jitter(h=0,w=0), color="orange") +
  geom_text(data = plotdata1, label = plotdata1$PSA, position=position_jitter(h=0,w=0), size = 5, color="red") +
  # plotdata2: IM-014
  geom_line(data = plotdata2, color="skyblue") +
  geom_jitter(data = nadir2, aes(x=nadir2$Time_Point, y = nadir2$PSA), position=position_jitter(h=0,w=0), color="red") +
  geom_jitter(data = dbPoint2, aes(x=dbPoint2$Time_Point, y = dbPoint2$PSA), position=position_jitter(h=0,w=0), color="black", size=3) +
  geom_jitter(data = dbPointR2, aes(x=dbPointR2$Time_Point, y = dbPointR2$PSA), position=position_jitter(h=0,w=0), color="orange") +
  geom_text(data = plotdata2, label = plotdata2$PSA, position=position_jitter(h=0,w=0), size = 5, color="blue") +
  # scale x
  scale_x_continuous(limits=c(0, 19),breaks = seq(0, 19, 1),labels=c("0start","start","1month","3month","6month","9month","12month","15month","18month","21month","2year","2.5year","3year","3.5year","4year","4.5year","5year","6year","6.5year","7year"))
p
### plot end ###
#####################################################################################################


#####################################################################################################
#### get BCR days ####
# start date
startDate <- plotdata1[1,c("Date","Time_Point")]
# nadir date
nadirDate <- plotdata1[which(plotdata1$Time_Point == nadir1$Time_Point),c("Date","Time_Point")]
# dbling first date
dbDate <- plotdata1[which(plotdata1$Time_Point == dbPointR1[1,"Time_Point"]),c("Date","Time_Point")]
## BCR days
# nadir days
nadirDays <- round(as.numeric(unclass(as.Date(nadirDate$Date)) - unclass(as.Date(startDate$Date))))
# db days
dbDays <- round(as.numeric(unclass(as.Date(dbDate$Date)) - unclass(as.Date(startDate$Date))))

#(as.numeric(dbPoint1[4,"Time_Point"]) - as.numeric(dbPoint1[1,"Time_Point"]))
# get delta value
delta <- (dbDays - nadirDays)/2
# get BCR
BCR <- nadirDays + delta
BCR
#####################################################################################################