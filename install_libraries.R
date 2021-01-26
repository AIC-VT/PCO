# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

# install_libraries.R
chooseCRANmirror(graphics=FALSE, ind=1)
install.packages(c("ggpubr","openxlsx","shiny","DT","ggplot2","nlme","stringr","shinyjs","formattable","shinythemes","ggthemes","survival","GGally","scales","knitr","rmarkdown","xtable","survminer","partykit","plotly","RMySQL","radarchart","sendmailR","V8","mailR",repos = c(CRAN = "http://cran.us.r-project.org")))

install.packages("devtools")

### if dbWriteTable function cause error
### remove RMySQL and install dev version of RMySQL
# remove.packages("RMySQL")
# devtools::install_github("rstats-db/DBI")
# devtools::install_github("rstats-db/RMySQL")

### if plotly library has unexpected behavior
### install plotly with follwing command
### but first need to install dependency - ggplot2 version 3.2.1
require(devtools)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.2.1.tar.gz"                         > install.packages(packageurl, repos=NULL, type="source")
install_version("plotly", version = "4.6.0", repos = "http://cran.us.r-project.org")
