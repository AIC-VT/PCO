# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

homedir = "/srv/shiny-server/PCO"
setwd(homedir)

source(file="./App-1/config.R")

################
# JAVSSCRIPTS
################
jsCode <- "shinyjs.printReport = function() { 
  print();
};
shinyjs.emailErrorPopup = function() {
  alert('Your email address is not correct.')
};
shinyjs.emailValidPopup = function() {
  alert('email sent successfully.')
};
"

####################
# END OF JAVASCRIPT
####################
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode),
  theme = shinytheme("cerulean"),
  titlePanel("Prostate Clinical Outlook"),
  
  navbarPage(
    title = 'Change your title in the ui.R source file'
  ),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               textInput("Doctor", label = "Doctor's Name", value = "Anatoly Dritschilo", width = "140")
        ),
        column(6, align="left", valign="center",
 
               actionButton("nextPatient",h6("Next Patient"),icon("forward"),width="100%")
        )
      ),
      fluidRow(
        column(6,
               textInput("sPatientName", label = "Patient's Name", value = "Patient Name", width = "150")
        ),
        column(3,
               textInput("nAge", label = "Age", value = 60, width = 50)
        ),
        column(3,
               textInput("nPSA", label = "PSA", value = 10, width = 50)
        )
      ),
      fluidRow(
        column(5,
               selectInput("nStage", label = "Clinical Stage",
                           c("T1a" = "T1a",
                             "T1b" = "T1b",
                             "T1c" = "T1c",
                             "T2a" = "T2a",
                             "T2b" = "T2b",
                             "T2c" = "T2c",
                             "T3" = "T3"
                           ), size=10, selectize = FALSE)
        ),
        column(6,
               selectInput("nGleasonsSum", label = "Gleason Score",
                           c("2" = "2",
                             "3" = "3",
                             "4" = "4",
                             "5" = "5",
                             "6" = "6",
                             "7=3+4" = "7=3+4",
                             "7=4+3" = "7=4+3",
                             "8" = "8",
                             "9" = "9",
                             "10" = "10"
                           ), selected = "6", size=10, selectize = FALSE)
        )
      ),
      fluidRow(
        column(12,
               actionButton("calculateButton","Calculate",icon("calculator"),width = "100%")
               )
      ),
      fluidRow(
        
        column(10,
               h4("Reference Patients Info."),
               h6("Relapse free analysis was based on",textOutput("SurvivalPatients"),"patients"),
               h6("Quality of life analysis was based on",textOutput("ReferencePatients"),"patients")
        ),
        column(10,
               h4("Graph Min #"),
               sliderInput("MinNum", "Minimum number of patients in a time slot",
                           0, 10, 0, step = 5, value = 5),
               h6("Check this and Push Next Patient button for reloading data(It takes around 30sec.)"),
               checkboxInput("reload",label = h6("Realoding ON/OFF"), value = FALSE, width = NULL),
               checkboxInput("firstLoad",label = h6("First Load"), value = TRUE, width = NULL)
        )
      ), # end of fluidrow
      width = 4
    ),
    
    mainPanel(width = 8,
              
              navbarPage(
                title = "Your",
                id = "MainNavBar",
                selected = "Profile",
                tabPanel(title = "Likely Outcomes", 
                         #value = "Likely Outcomes",
                         #########################################################
                         ## menu 1             
                         
                         fluidPage(
                           fluidRow(
                             h4("1. Relapse Free Survival Probability"),
                             h6("(* Relpase: Biochemical Relapse )"),
                             plotOutput("survPlotOverall", width = "100%", height = 500)
                           ),
                           fluidRow(
                             h4("2. Rectal Quality of Life"),
                             h5("2.1 Rectal Quality of Life Overall"),
                             plotlyOutput("RectalQOLPlot", width = "100%", height = 300),
                             plotOutput("PatientNumber1", width = "100%", height = 150),
                             h5("2.2 Rectal Bleeding Risk"),
                             plotlyOutput("RectalQOLBleedingPlot", width = "100%", height = 300),
                             plotOutput("PatientNumber5", width = "100%", height = 150)
                           ),
                           fluidRow(
                             h4("3. Urinary Quality of Life"),
                             h5("3.1 Urinary Quality of Life Overall"),
                             plotlyOutput("UrinaryQOLPlot_Overall", width = "100%", height = 300),
                             plotOutput("PatientNumber2", width = "100%", height = 150),
                             h5("3.2 Urinary Incontinence Risk"),
                             plotlyOutput("UrinaryQOLPlot_Incontinence", width = "100%", height = 300),
                             plotOutput("PatientNumber3", width = "100%", height = 150),
                             h5("3.3 Urinary Irritation Risk"),
                             plotlyOutput("UrinaryQOLPlot_Irritation", width = "100%", height = 300),
                             plotOutput("PatientNumber4", width = "100%", height = 150),
                             h5("3.4 Urinary Bleeding Risk"),
                             plotlyOutput("UrinaryQOLPlot_Bleeding", width = "100%", height = 300),
                             plotOutput("PatientNumber6", width = "100%", height = 150),
                             h4("")
                           ),
                           fluidRow(
                             h4("4. Sexual Quality of Life"),
                             h5("4.1 Sexual Quality of Life Overall"),
                             plotlyOutput("SexualQOLPlot", width = "100%", height = 300),
                             plotOutput("PatientNumber7", width = "100%", height = 150),
                             h5("4.2 Erectile Dysfunction Risk"),
                             plotlyOutput("SexualQOLPlot_Erectile", width = "100%", height = 300),
                             plotOutput("PatientNumber8", width = "100%", height = 150)
                           )
                         )
                ),
                ## end of menu 1
                #########################################################
                
                #########################################################
                ## start of menu 2
                tabPanel(title = "Profile",
                         #value = "Profile",
                         fluidRow(
                           column(width=6,
                                  # report to file
                                  #radioButtons('type', 'Report Type', c('Graph'),inline = TRUE),
                                  #radioButtons('format', 'Document format', c('PDF', 'HTML'), inline = TRUE),
                                  downloadButton('downloadReport',"Download Report"),
                                  
                                  actionButton("printReport", "Print Report",icon("print"))
                                  
                                  
                           ),
                           
                           column(width=4, 
                                  div(style = "height:0px;"),
                                  textInput("emailAddress","","",
                                            width="100%",
                                            placeholder = "patient's e-mail address")
                          ),
                          column(width=2, align="left",
                                 actionButton("emailReport", "e-Mail",icon("envelope-o"))
                                 
                          )
                         ),
                         tags$style(type='text/css', "#downloadReport { width:58%; margin-top: 20px;}"),
                         tags$style(type='text/css', "#printReport { width:40%; margin-top: 20px;}"),
                         tags$style(type='text/css', "#emailReport { width:100%; margin-top: 20px;}"),
                         tags$style(type='text/css', "#emailAddress { width:100%; margin-top: 0px;}"),
                         fluidRow(
                           
                           column(width=5,align="left",
                                  
                                  h5("Risk Score"),margin=0,
                                  plotOutput("GaugeChart", width = 280, height = 280)
                                  
                           ),
                           
                           column(width=7,
                                  h5("Summary: Reference vs. Patient Specific"),
                                  #br(),
                                  plotlyOutput("OverallVSPatient", width = "100%", height=300)
                                  
                           )
                         ),
                         fluidRow(
                           
                           column(width=6,align="right",valign="top",
                                  h5("Relapse Free Survival Probability at 5 years"),
                                  h6("(*Relapse: Bio Chemical Relapse)")
                                  
                           ),
                           column(width=6, align="left", valign="top",
                                  formattableOutput("riskDiseaseFree", width=150)
                           )
                           
                         ),
                         
                         fluidRow(
                           h5("Quality of Life - at 5 years")
                         ),
                         
                         fluidRow(
                           column(width=3,
                                  h6("Rectal QOL(+)"),
                                  formattableOutput("riskRectal", width=150)
                           ),
                           
                           column(width=3,
                                  h6("Urinary QOL(+)"),
                                  formattableOutput("riskUrinaryQOL", width=150)
                           ),
                           column(width=3,
                                  h6("Sexual QOL(+)"),
                                  formattableOutput("riskErectileDysfunction", width=150)
                           )
                         ),
                         fluidRow(
                           column(width=3,
                                  h6("Rectal Bleeding(-)"),
                                  formattableOutput("riskRectarBleeding", width=150)
                           ),
                           column(width=3,
                                  h6("Urinary Bleeding(-)"),
                                  formattableOutput("riskUrinaryBleeding", width=150)
                           )
                         )
                         
                ),
                ## end of menu 2
                #########################################################  
                
                #########################################################
                ## start of menu 2
                tabPanel(title = "PSA Trend Review",
                         #value = "PSA trend review",
                         actionButton("Prev","Prev Patient"),
                         actionButton("Next","Next Patient"),
                         h4("Patient_Id"),
                         textInput("fPatient_Id", label = h4(""), value = "Anonymous PatientID", width = 200),
                         plotOutput("PSAPlot", height = 300)
                )
                ## end of menu 2
                #########################################################  
                
              ),
              h6("Disclaimer - Not for use for patient care, this information is for investigational use only."),
              h6("This site is powered by plotly(MIT License), formattable(MIT License), R and R shiny.")
              
    ) ## end of navbarPage
    
  )
)
)

