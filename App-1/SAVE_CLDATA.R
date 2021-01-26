# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

###############################################
# save cleansing data
# date conversion
cldata[,c("DOB")] <- convertToDate(cldata[,c("DOB")])
cldata[,c("Date")] <- convertToDate(cldata[,c("Date")])
# connect to mysql
mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
# init mysql table-cleansing
dbSendQuery(mydb, "drop table if exists TB_CYBERKNIFE_CLEAN;")
dbWriteTable(mydb, 'TB_CYBERKNIFE_CLEAN', cldata, row.names=F)
dbDisconnect(mydb)
###############################################