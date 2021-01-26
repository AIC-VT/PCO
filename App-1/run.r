# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

require(shiny)
folder_address = '/srv/shiny-server/PCO/App-1'
stopApp()
runApp(folder_address, launch.browser=TRUE, host="0.0.0.0", port=3838)
