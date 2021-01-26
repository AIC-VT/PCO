# Define server logic
shinyServer(function(input, output, session) {
  ## load data
  firstLoad = 0
  source("./App-1/checkDB.R")
  firstLoad = checkTables()
  withProgress(message = 'Loading Data', value = 0, {
    n = 6
    if ( firstLoad == 1 ) {
      source("./App-1/mysql_init_data.R") #1 #
      incProgress(1/n, detail = paste("Doing part", 1))
      source("./App-1/cleansing.R") #2
      incProgress(1/n, detail = paste("Doing part", 2))
      source("./App-1/SAVE_SURVIVAL_DATA2.R")
      incProgress(1/n, detail = paste("Doing part", 3))
      source("./App-1/EPIC_score.R") #3
      incProgress(1/n, detail = paste("Doing part", 4))
      source("./App-1/QOL_score.R") #4
      incProgress(1/n, detail = paste("Doing part", 5))
      source("./App-1/SAVE_CLDATA.R") #6
      incProgress(1/n, detail = paste("Doing part", 6))
      firstLoad = 0
    }
  }) # end of progress
  obs <- observe({ 
    withProgress(message = 'Loading Data', value = 0, {
      n = 6
      
      if ( input$reload == TRUE ) {
        source("./App-1/mysql_init_data.R") #1 #
        incProgress(1/n, detail = paste("Doing part", 1))
        source("./App-1/cleansing.R") #2
        incProgress(1/n, detail = paste("Doing part", 2))
        source("./App-1/SAVE_SURVIVAL_DATA2.R")
        incProgress(1/n, detail = paste("Doing part", 3))
        source("./App-1/EPIC_score.R") #3
        incProgress(1/n, detail = paste("Doing part", 4))
        source("./App-1/QOL_score.R") #4
        incProgress(1/n, detail = paste("Doing part", 5))
        source("./App-1/SAVE_CLDATA.R") #6
        incProgress(1/n, detail = paste("Doing part", 6))
        firstLoad = 0
      }
    }) # end of progress
  })
  source("./App-1/load.R") #LAST
    
      
  source("./App-1/GET_PROFILE_VALUE.R")
  source("./App-1/GET_PCAP_CLASS.R")
  ###############################################
  # load survival data
  # connect to mysql
  require("RMySQL")
  mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
  survData <- dbReadTable(mydb, name = 'TB_CYBERKNIFE_SURVIVAL')
  dbDisconnect(mydb)
  ###############################################
  survData$PSA_LEVEL <- as.numeric(survData$PSA_LEVEL)
  survData$T_STAGE_CATEGORY <- as.numeric(survData$T_STAGE_CATEGORY)
  survData$RISK_CATEGORY <- as.numeric(survData$RISK_CATEGORY)
  survData$AGE_CATEGORY <- as.numeric(survData$AGE_CATEGORY)
  
  ## reactive values
  values <- reactiveValues()
  values$df <- cldata
  
  ## New Patient Data
  nData <- eventReactive(input$calculateButton|input$firstLoad,{
    AGE = as.numeric(input$nAge)
    T_Stage = input$nStage
    #GleasonsSum = as.numeric(input$nGleasonsSum)
    GleasonsSum = input$nGleasonsSum
    PSA = as.numeric(input$nPSA)
    
    # AGE_CATEGORY
    if ( AGE >= 40 & AGE < 60 )
      AGE_CATEGORY = 1
    else if ( AGE >= 60 & AGE < 70 )
      AGE_CATEGORY = 2
    else if ( AGE >= 70 & AGE < 80 )
      AGE_CATEGORY = 3
    else if ( AGE >= 80 )
      AGE_CATEGORY = 4
    else
      AGE_CATEGORY = 1
    
    # T_STAGE_CATEGORY
    if ( T_Stage == "T1a" | T_Stage == "T1b" | T_Stage == "T1c")
      T_STAGE_CATEGORY = 1
    else if ( T_Stage == "T2a" )
      T_STAGE_CATEGORY = 2
    else if ( T_Stage == "T2b" )
      T_STAGE_CATEGORY = 3
    else if ( T_Stage == "T2c" )
      T_STAGE_CATEGORY = 4
    else if ( T_Stage == "T3")
      T_STAGE_CATEGORY = 4
    
    # PSA_LEVEL
    if ( PSA <= 10 )
      PSA_LEVEL = 1
    else if ( PSA > 10 & PSA <= 20 )
      PSA_LEVEL = 2
    else if ( PSA > 20 )
      PSA_LEVEL = 3
    
    # RISK_CATEGORY
    if ( GleasonsSum == "7=3+4") {
      RISK_CATEGORY = 2 
    } else if ( GleasonsSum == "7=4+3" ) {
      RISK_CATEGORY = 3 
    } else {
      if ( as.numeric(GleasonsSum) <= 6 & PSA <= 10 )
        RISK_CATEGORY = 1
      else if ( as.numeric(GleasonsSum) >= 8 | PSA >= 20 )
        RISK_CATEGORY = 5
      else if (  ( PSA > 10 & PSA < 20 ) )
        RISK_CATEGORY = 4
    } 
    
    PCAP_CLASS = getPCaPClass(pAGE_CATEGORY = AGE_CATEGORY,
                              pRISK_CATEGORY = RISK_CATEGORY,
                              pT_STAGE_CATEGORY = T_STAGE_CATEGORY)
    
    df <- data.frame(Doctor = input$Doctor, 
                     Name = input$sPatientName,
                     AGE = AGE, 
                     T_Stage = T_Stage, 
                     GleasonsSum = GleasonsSum, 
                     PSA = PSA,
                     AGE_CATEGORY = AGE_CATEGORY,
                     T_STAGE_CATEGORY = T_STAGE_CATEGORY,
                     PSA_LEVEL = PSA_LEVEL,
                     RISK_CATEGORY = RISK_CATEGORY,
                     PCAP_CLASS = PCAP_CLASS,
                     MinNum <- input$MinNum
    )
    return(list(df=df))
  })
  
  # PSA trend button action
  observeEvent(input$Prev, {
    uniquePIDlist <- unique(cldata[,c("Patient_Id")])
    #uniquePIDlist <- unique(ckdata[,c("Patient_Id")])
    n <- length(uniquePIDlist)
    index <- which(uniquePIDlist == input$fPatient_Id)
    if ( length(index) == 0 ) {
      index = 1
    }
    if ( index == 1 ) {
      index <- n
    } else {
      index <- index - 1
    }
    ID <- uniquePIDlist[index]
    updateTextInput(session, "fPatient_Id", value=ID)  
  })
  
  observeEvent(input$Next, {
    uniquePIDlist <- unique(cldata[,c("Patient_Id")])
    #uniquePIDlist <- unique(ckdata[,c("Patient_Id")])
    n <- length(uniquePIDlist)
    index <- which(uniquePIDlist == input$fPatient_Id)
    if ( length(index) == 0 ) {
      index = n
    }
    if ( index == n ) {
      index <- 1
    } else {
      index <- index + 1
    }
    ID <- uniquePIDlist[index]
    updateTextInput(session, "fPatient_Id", value=ID)  
  })
  
  #########################################################
  ## Your Outcomes
  #########################################################
  
  ## outcome plot 
  #######
  ## plot Survival Curve Hazard
  output$survPlotOverall <- renderPlot({
    source("./App-1/SurvivalCurve_Hazard_Overall.R")
      plotSurvFunc(vValues = values,
                 vNData = nData()$df, 
                 vOption = "PSA", 
                 vSurvData = survData,
                 vGroupOption = input$GroupOption,
                 vFontSize = 10,
                 vLFontSize = 5,
                 vInput = input)
  })
  
  ####################################################
  ## get number of patients
  output$SurvivalPatients <- renderText({
    source("./App-1/GET_PROFILE_VALUE.R")
    getNumberofPatients1(dfSource = survData, profileCondition = "Patient_Id")
  })
  output$ReferencePatients <- renderText({
    source("./App-1/GET_PROFILE_VALUE.R")
    getNumberofPatients2(dfSource = values$df, profileCondition = "Patient_Id")
  })
  
  
  ####################################################
  ## plot patient data
  # tables
  output$PatientNumber1 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber2 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber3 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber4 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber5 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber6 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber7 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$PatientNumber8 <- renderPlot({
    source("./App-1/PATIENT_NUMBER_TABLE.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  ## plots
  # rectal qol
  output$RectalQOLPlot <- renderPlotly({
    ######## sourcing R script
    source("./App-1/RectalQOL_Graph.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$RectalQOLBleedingPlot <- renderPlotly({
    ######## sourcing R script
    source("./App-1/RectalQOL_Blood_Graph.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$UrinaryQOLPlot_Overall <- renderPlotly({
    ######## sourcing R script
    source("./App-1/UrinaryQOL_Graph_Overall.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  # urinary qol
  output$UrinaryQOLPlot_Incontinence <- renderPlotly({
    ######## sourcing R script
    source("./App-1/UrinaryQOL_Graph_Incontinence.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$UrinaryQOLPlot_Irritation <- renderPlotly({
    ######## sourcing R script
    source("./App-1/UrinaryQOL_Graph_Irritation.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$UrinaryQOLPlot_Bleeding <- renderPlotly({
    ######## sourcing R script
    source("./App-1/UrinaryQOL_Blood_Graph.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  # sexual qol
  output$SexualQOLPlot <- renderPlotly({
    ######## sourcing R script
    source("./App-1/SexualQOL_Graph_Overall.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$SexualQOLPlot_Erectile <- renderPlotly({
    ######## sourcing R script
    source("./App-1/SexualQOL_Graph_Erectile.R")
    plotfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  
  
  ####################################################
  # PROFILE TAB DATAs
  ####################################################
  ## plot Survival 5 year probability
  output$survCalcOverall <- renderText({
    source("./App-1/SurvivalCalculator_Overall.R")
    calcSurvFunc(vValues = values,
                 vNData = nData()$df, 
                 vOption = "PSA", 
                 vSurvData = survData,
                 vGroupOption = input$GroupOption,
                 vFontSize = 10,
                 vLFontSize = 5,
                 vInput = input)
  })
  
  output$riskDiseaseFree <- renderFormattable({
    source("./App-1/DiseaseFree_Risk.R")
    calcSurvFunc(vValues = values,
                 vNData = nData()$df, 
                 vOption = "PSA", 
                 vSurvData = survData,
                 vGroupOption = input$GroupOption,
                 vFontSize = 10,
                 vLFontSize = 5,
                 vInput = input)
  })
  
  output$riskRectal <- renderFormattable({
    source("./App-1/RectalQOL_Risk.R")
    riskfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$riskRectarBleeding <- renderFormattable({
    source("./App-1/RectalQOL_Blood_Risk.R")
    riskfunc(vPlotData = values$df, vNData = nData()$df)
  })

  output$riskUrinaryQOL <- renderFormattable({
    source("./App-1/UrinaryQOL_Risk_Overall.R")
    riskfunc(vPlotData = values$df, vNData = nData()$df)
  })
  
  output$riskUrinaryBleeding <- renderFormattable({
    source("./App-1/UrinaryQOL_Blood_Risk.R")
    riskfunc(vPlotData = values$df, vNData = nData()$df)
  })

  output$riskErectileDysfunction <- renderFormattable({
    source("./App-1/SexualQOL_Erectile_Risk.R")
    riskfunc(vPlotData = values$df, vNData = nData()$df)
  })

  output$radarProfile <- renderChartJSRadar({
    source("./App-1/RADAR_PROFILE.R")
    radarfunc(vSurvData = survData, vPlotData = values$df, vNData = nData()$df)
  })

  output$OverallVSPatient <- renderPlotly({
    ######## sourcing R script
    source("./App-1/OverallVSProfile.R")
    vsfunc(vSurvData = survData, vPlotData = values$df, vNData = nData()$df)
  })

  output$GaugeChart <- renderPlot({
    source("./App-1/GAUGE_CHART.R")
    score = getPCaPScore(pAGE_CATEGORY = nData()$df$AGE_CATEGORY,
                        pRISK_CATEGORY = nData()$df$RISK_CATEGORY,
                        pT_STAGE_CATEGORY = nData()$df$T_STAGE_CATEGORY)
    riskGaugeFunc(nValue = score)
  })
  
  #################################  
  ## old resources
  num_cols = c(2:4,5:7)
  ## Objective IDs T-Stage, Gleasons, Risk
  output$basicInfo <- DT::renderDataTable(
    nData()$df,
    options = list(searching = FALSE,
                   paging = FALSE)
  )
  
  ############
  # PSA Plot #
  ############
  output$PSAPlot <- renderPlot({
    source("./App-1/BCR_estimate_list.R")
    plotPSA(vPatient_Id = input$fPatient_Id)
  })
  
  #################
  ## report 기능 ##
  #################
  output$downloadReport <- downloadHandler(
    filename = function() {
      #paste('my-report', sep = '.', switch(
      #  input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      #))
      paste('my-report', sep = '.', 'pdf')
    },
    
    content = function(file) {
      #if ( input$type == "Graph" ) {
      #  src <- normalizePath('./App-1/report.Rmd') # type 1
      #} else {
      #  src <- normalizePath('./App-1/report_type2.Rmd') # type 2
      #}
      src <- normalizePath('./App-1/report.Rmd')
      mydb = dbConnect(MySQL(), user='shiny', password='shiny', dbname='CDSS', host='localhost')
      dbSendQuery(mydb, "insert into TB_LOG values(sysdate(), 'Report Profile');")
      dbDisconnect(mydb)
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      library(rmarkdown)
      #out <- render('report.Rmd', switch(
      #  input$format,
      #  PDF = pdf_document(), HTML = html_document(), Word = word_document()
      #))
      out <- render('report.Rmd', pdf_document())
      file.rename(out, file)
    }
  )
  ## end of report @ menu 1
  ## end of menu 1
  #########################################################
  
  ###########
  # Next Patient Button
  obs <- observe({ 
    if(input$nextPatient == TRUE) {
      session$reload()
      }
  })
  
  ##########
  # PRINT REPORT
  
  observeEvent(input$printReport, {
    shinyjs::js$printReport()
  })
  
  ###########
  # EMAIL REPORT
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  }
  string.counter<-function(strings, pattern){  
    counts<-NULL
    for(i in 1:length(strings)){
      counts[i]<-length(attr(gregexpr(pattern,strings[i])[[1]], "match.length")[attr(gregexpr(pattern,strings[i])[[1]], "match.length")>0])
    }
    return(counts)
  }
  observeEvent(input$emailReport,{
    ## check email address
    isValid <- isValidEmail(input$emailAddress)
    atCount <- string.counter(input$emailAddress,'@')
    if (!isValid | atCount != 1) {
      shinyjs::js$emailErrorPopup()
      return(0)
    } else {
      shinyjs::js$emailValidPopup()
    }
      
    #########################
    ## pdf generation start
    render('./App-1/report.Rmd', pdf_document())
    
    ############################################################
    # Please select your sendmail method #1 or #2
    # mail method #1 works only for gmail account receivers
    # mail method #2 works any email acccout(gmail acccount, for sending email, require)
    # ref: https://support.google.com/a/answer/176600?hl=en
    ########## mail method #1: sendmailR source ################
    attachementPath <- "./App-1/report.pdf"
    attachementName <- "report.pdf"
    to <- gsub(" ","",paste("<",input$emailAddress,">"))
    subject <- "PCO Likely Outcome Report"
    body <- "PCO Report attached"
    mailControl=list(smtpServer="ASPMX.L.GOOGLE.COM")
    attachementObject <- mime_part(x=attachementPath,name=attachementName)
    bodyWithAttachement <- list(body,attachementObject)
    sendmail(from="<do_not_reply@pco.com>",to=to,subject=subject,msg=bodyWithAttachement,control=mailControl)
    ########## sendmail.R source ################################
    
    ########## mailR source ################################
    #send.mail(from = "do_not_reply@pco.com",
    #          to = c(input$emailAddress),
    #          subject = "PCO Likely Outcome Report",
    #          body = "PCO Report attached",
    #          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "yourgmailid@gmail.com", passwd = "yourpassword", ssl = TRUE),
    #          authenticate = TRUE,
    #          send = TRUE,
    #          attach.files = c("./App-1/report.pdf"),
    #          debug = TRUE)
    ########## mailR source ################################
  })
})