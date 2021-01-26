# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

# GAUGE_CHART.R
# function to create a circle

circle <- function(center=c(0,0), radius=1, npoints=156)
{
  r = radius
  tt = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# function to get slices
slice2xy <- function(t, rad) 
{
  t2p = -1 * t * pi + 10*pi/8
  list(x = rad * cos(t2p), y = rad * sin(t2p))
}

# function to get major and minor tick marks
ticks <- function(center=c(0,0), from=0, to=2*pi, radius=0.9, npoints=156)
{
  r = radius
  tt = seq(from, to, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

riskGaugeFunc <- function (..., nValue = 0) {
  maxPoints = 156
  # external circle (this will be used for the black border)
  border_cir = circle(c(0,0), radius=1, npoints = maxPoints)
  
  # gray border circle
  external_cir = circle(c(0,0), radius=0.97, npoints = maxPoints)
  
  # green slice (this will be used for the yellow band)
  greenFrom = 0
  greenTo = 34
  gre_ini = (greenFrom/maxPoints) * (12/8)
  gre_fin = (greenTo/maxPoints) * (12/8)
  Sgre = slice2xy(seq.int(gre_ini, gre_fin, length.out = 30), rad=0.9)
  
  # yellow slice (this will be used for the yellow band)
  yellowFrom = 35
  yellowTo = 115
  yel_ini = (yellowFrom/maxPoints) * (12/8)
  yel_fin = (yellowTo/maxPoints) * (12/8)
  Syel = slice2xy(seq.int(yel_ini, yel_fin, length.out = 30), rad=0.9)
  
  # red slice (this will be used for the red band)
  redFrom = 116
  redTo = maxPoints
  red_ini = (redFrom/maxPoints) * (12/8)
  red_fin = (redTo/maxPoints) * (12/8)
  Sred = slice2xy(seq.int(red_ini, red_fin, length.out = 30), rad=0.9)
  
  # white slice (this will be used to get the yellow and red bands)
  whiteFrom = 0
  whiteTo = maxPoints
  white_ini = (whiteFrom/maxPoints) * (12/8)
  white_fin = (whiteTo/maxPoints) * (12/8)
  Swhi = slice2xy(seq.int(white_ini, white_fin, length.out = 30), rad=0.8)
  
  # coordinates of major ticks (will be plotted as arrows)
  major_ticks_out = ticks(c(0,0), from=5*pi/4, to=-pi/4, radius=0.9, 5)
  major_ticks_in = ticks(c(0,0), from=5*pi/4, to=-pi/4, radius=0.75, 5)
  
  # coordinates of minor ticks (will be plotted as arrows)
  tix1_out = ticks(c(0,0), from=5*pi/4, to=5*pi/4-3*pi/8, radius=0.9, 6)
  tix2_out = ticks(c(0,0), from=7*pi/8, to=7*pi/8-3*pi/8, radius=0.9, 6)
  tix3_out = ticks(c(0,0), from=4*pi/8, to=4*pi/8-3*pi/8, radius=0.9, 6)
  tix4_out = ticks(c(0,0), from=pi/8, to=pi/8-3*pi/8, radius=0.9, 6)
  tix1_in = ticks(c(0,0), from=5*pi/4, to=5*pi/4-3*pi/8, radius=0.85, 6)
  tix2_in = ticks(c(0,0), from=7*pi/8, to=7*pi/8-3*pi/8, radius=0.85, 6)
  tix3_in = ticks(c(0,0), from=4*pi/8, to=4*pi/8-3*pi/8, radius=0.85, 6)
  tix4_in = ticks(c(0,0), from=pi/8, to=pi/8-3*pi/8, radius=0.85, 6)
  
  # coordinates of min and max values (0, 100)
  v0 = -1 * 0 * pi + 10*pi/8
  z0x = 0.65 * cos(v0)
  z0y = 0.65 * sin(v0)
  
  v25 = -1 * 3/8 * pi + 10*pi/8
  z25x = 0.65 * cos(v25)
  z25y = 0.65 * sin(v25)
  
  v50 = -1 * 6/8 * pi + 10*pi/8
  z50x = 0.65 * cos(v50)
  z50y = 0.65 * sin(v50)
  
  v75 = -1 * 9/8 * pi + 10*pi/8
  z75x = 0.65 * cos(v75)
  z75y = 0.65 * sin(v75)
  
  v100 = -1 * 12/8 * pi + 10*pi/8
  z100x = 0.65 * cos(v100)
  z100y = 0.65 * sin(v100)
  #######################################################################
  # indicated value, say 80 (you can choose another number between 0-100)
  value = nValue
  
  # angle of needle pointing to the specified value
  val = (value/maxPoints) * (12/8)
  v = -1 * val * pi + 10*pi/8
  # x-y coordinates of needle
  val_x = 0.7 * cos(v)
  val_y = 0.7 * sin(v)
  
  # label to be displayed
  if ( nValue <= 34 ) {
    label = "Low"
    labelcolor = "yellowgreen"
  }
  else if ( nValue > 34 & nValue <= 115) {
    label = "Intermediate"
    labelcolor = "orange"
  }  
  else if ( nValue > 115) {
    label = "High"
    labelcolor = "red"
  }
  ############
  # open plot
  par(bg = "transparent",
      mar=c(0,0,0,0))
  plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
       xlim=c(-1.05,1.05), ylim=c(-1.05,1.05),
       #xlim=c(-1,1), ylim=c(-1,1),
       xlab="", ylab="", bg = "transparent")
  # green slice
  polygon(c(Sgre$x, 0), c(Sgre$y, 0),
          border = "yellowgreen", col = "yellowgreen", lty = NULL)
  # yellow slice
  polygon(c(Syel$x, 0), c(Syel$y, 0),
          border = "#FF9900", col = "#FF9900", lty = NULL)
  # red slice
  polygon(c(Sred$x, 0), c(Sred$y, 0),
          border = "#DC3912", col = "#DC3912", lty = NULL)
  # white slice
  polygon(c(Swhi$x, 0), c(Swhi$y, 0),
          border = "white", col = "white", lty = NULL)
  # add gray border
  lines(external_cir$x, external_cir$y, col="gray85", lwd=20)
  # add external border
  lines(border_cir$x, border_cir$y, col="gray20", lwd=2)
  # add minor ticks
  arrows(x0=tix1_out$x, y0=tix1_out$y, x1=tix1_in$x, y1=tix1_in$y,
         length=0, lwd=2.5, col="gray55")
  arrows(x0=tix2_out$x, y0=tix2_out$y, x1=tix2_in$x, y1=tix2_in$y,
         length=0, lwd=2.5, col="gray55")
  arrows(x0=tix3_out$x, y0=tix3_out$y, x1=tix3_in$x, y1=tix3_in$y,
         length=0, lwd=2.5, col="gray55")
  arrows(x0=tix4_out$x, y0=tix4_out$y, x1=tix4_in$x, y1=tix4_in$y,
         length=0, lwd=2.5, col="gray55")
  # add major ticks
  arrows(x0=major_ticks_out$x, y0=major_ticks_out$y,
         x1=major_ticks_in$x, y1=major_ticks_in$y, length=0, lwd=4, col = "#999999")
  # add needle
  arrows(0, 0, val_x, val_y, col="#999999", lwd=10)
  # add value
  text(0, 0.25, value, cex=3, col = labelcolor)
  # add label of variable
  text(0, -0.75, "Risk", cex=1.8, col=labelcolor)
  text(0, -0.55, label, cex=1.8, col=labelcolor)
  # add central blue point
  points(0, 0, col="#C6C6C6", pch=19, cex=3)
  points(0, 0, col="#000000", pch=19, cex=1)
  # add values 0 and maxPoints
  val0 = 0
  val25 = maxPoints * 0.25
  val50 = maxPoints * 0.5
  val75 = maxPoints * 0.75
  text(z0x, z0y, labels=val0, col="yellowgreen")
  text(z25x, z25y, labels=val25, col="orange")
  text(z50x, z50y, labels=val50, col="orange")
  text(z75x, z75y, labels=val75, col="red")
  text(z100x, z100y, labels=maxPoints, col="red")

}

#riskGaugeFunc(nValue = 100)