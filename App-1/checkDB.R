# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

## checkDB.R : check tables for initial update

checkTables <- function(...) {
  require("RMySQL")
  # connect DB
  mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
  isTB1 = dbExistsTable(mydb, name = 'TB_CYBERKNIFE')
  isTB2 = dbExistsTable(mydb, name = 'TB_CYBERKNIFE_CLEAN')
  isTB3 = dbExistsTable(mydb, name = 'TB_CYBERKNIFE_SURVIVAL')
  isTB4 = dbExistsTable(mydb, name = 'TB_LOG')
  
  if ( isTB1 & isTB2 & isTB3 & isTB4 )
    retVal = 0 # no need to initial loading
  else {
    dbSendQuery(mydb, "drop table if exists TB_CYBERKNIFE, TB_CYBERKNIFE_CLEAN, TB_CYBERKNIFE_SURVIVAL, TB_LOG;")
    dbSendQuery(mydb, "create table TB_LOG(date DATETIME, log VARCHAR(250));")
    retVal = 1 # need to initial loading
  }
  
  dbDisconnect(mydb)
  return(retVal)
}

