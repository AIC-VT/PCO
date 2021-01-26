# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

# config.R
rm(list=ls())
require(openxlsx)
require(shiny)
require(DT)
require(ggplot2)
require(nlme)
require(grid)
require(stringr)
require(shinyjs)
require(shinythemes)
require(ggthemes)
require(survival)
require(GGally)
require(scales)
require(knitr)
require(rmarkdown)
require(xtable)
require(survminer)
require(partykit) # need coin package
require(RMySQL)
require(formattable)
require(radarchart)
require(plotly) # dynamic plot(ex: mouseover)
require(sendmailR)
require(V8)
#require(JGR)
require(mailR)
###################### when JVM fail try next 3 steps ###############################
# 1. sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
# 2. sudo R CMD javareconf
# 3. install.packages("rJava",type='source')
#####################################################################################