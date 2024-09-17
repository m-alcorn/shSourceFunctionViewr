#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
source("networkFunctions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),

    # Application title
    titlePanel("Source Function Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel( 
        radioButtons("fChoice",
                     "Use existing file, or new...",
                     selected = character(0),
                     choices = c("Use a pre-processed file." = "preFile",
                                 "Create a new file from RAW Function Export plugin." = "newFile")),
        br(),
        
          h3("Select file containing function list..."),
          uiOutput("datafile"), 
        textOutput("gotFile"),
        br(),
        actionButton("btnGetData","Get Data..."),
        textOutput("gotData"),
        br(),
        actionButton("btnPlot","PlotData..."),
        textOutput("gotPlot"),
        br(),
        h3("Use the drop down list below to filter Functions"),
          selectInput( 
            "select", 
            "Add filter terms:", 
            list(), 
            multiple = TRUE 
          ),
        textOutput("gotFilters"),
          br(),
          # verbatimTextOutput("fListPath"),
          br(),
          radioButtons("plotType", label = h3("Choose a plot type.."),
                       choices = list("Simple Network" = 1, "Force Network" = 2), 
                       selected = 1)
       
        ),

        # Show a plot of the generated distribution
        mainPanel(
         h1("Source Functions: Visualisation Tool"),
          
         p("This tool uses a visual network representation of how functions 
         are related to each other in the model."), 
           p("To get started: Select a file containing Functions exported from your Source model."),
           p("To filter results: Click on a single or multiple option from the
            dropdown list. (To remove: click to highlight, then delete)"),
         br(),
         verbatimTextOutput("statusMessage"),
         
         uiOutput("plotFrame1")
         # uiOutput("plotFrame1",
         #          div(style = "width: 100%; height: 500px; visibility: inherit")
         # )
        
          
       
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  fData <- reactiveValues(inFile = NULL,
                          data = NULL,
                          selectedInputType = NULL,
                          actualInputType = NULL,
                          plotType = NULL,
                          currentMessage = NULL,
                          validInputs = NULL
  )
  
  checkInputs <- function(fData){
    #First check for file type
    fData$selectedInputType <- input$fChoice
    fData$inFile <- input$datafile$datapath
    print(input$fChoice)
    print(fData$inFile)
    #browser()
    f <- fData$inFile
    #browser()
    fData$actualInputType <- checkFileType(f)
    #browser()
    #browser()
    
    if(fData$actualInputType==fData$selectedInputType){
      #browser()
      if(!is.null(fData$data)){
        fData$validInputs <- "TRUE"
        fData$currentMessage <- "All inputs are currently valid"
        #browser()
      } else { 
        fData$validInputs <- "FALSE"
        fData$currentMessage <- "No valid data currently loaded"
        }
      
    } else {
      fData$currentMessage <- "No valid data currently loaded"
      fData$validInputs <- "FALSE"
      
    }
    
    
    
  }
  
  rVal <- reactiveValues(
      current = "preFile",
      last = "NULL")
  
  # fReset <- reactive()
  output$statusMessage <- renderPrint({fData$currentMessage})
 
 
    output$datafile <- renderUI({
      
      fileInput("datafile","Open *.csv", #UPDATE: RESTRICT TO CSV
                
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    })

 
  
  resetfileInput <- function(){
    
    req(input$fChoice)
    rVal$last = rVal$current
    rVal$current <- input$fChoice
    if(rVal$current==rVal$last){
      print("rVal has not changed....")
      return(FALSE)
    } else{
      print("rVal has changed....")
      return(TRUE)
    }
  }
  
  observe({
    # browser()
    fData$inFile <- input$datafile$datapath
    fData$selectedChoice <- input$fChoice
    # print(fData)
    
  })
  
  
  data <- observeEvent(input$btnGetData,{
    fData$inFile <- input$datafile$datapath
    file1 <- input$datafile
    
    if(is.null(file1)){
      output$gotData <- renderText({
        "No file selected"
      })
      return()
    } 

    req(input$datafile)
    req(input$fChoice)
    

    
    if(input$fChoice=="preFile"){
      req(input$datafile)
      d <- getData(input$datafile$datapath)
      
    } else{
      if(input$fChoice=="newFile"){ 
        req(input$datafile)
        d <-parseFunctionExport(input$datafile$datapath)
      }
    
    }
    output$gotData <- renderText({"Data loaded successfully."})
    
    fData$data <- d
    checkInputs(fData)
  })
  
  

  observe({
    if(is.null(fData$data)) ({return()})
    
    req(input$datafile)
    
    req(input$fChoice)
     reset <- resetfileInput()
     print(paste0('Reset = ', reset))
     if(reset){
       return()
       } else {
         # output$gotData <- renderText({"Loading data. Please wait...."})
    ch <- getFolderPaths(fData$data)
  #browser()
    updateSelectInput(inputId = "select",
                      choices = ch) }
    # browser()
     
     # output$gotFilters <- renderText({"Filter options ready.."})
     checkInputs(fData)
  }
  )

  
  
   observeEvent(input$btnPlot,{  
     output$plotFrame1 <- renderUI({
       
              
              if(is.null(fData$data)){
                
                 output$gotPlot <- renderText({"No Data loaded for plot..."})
                return()
                 }
              s <- input$select
              #browser()
              if(!is.null(s)){
                pd <- filterData(s,fData$data)
                #browser()
              } else {
                pd <- fData$data
                #browser()
              }
              req(pd)
              pt <- input$plotType
              if(pt=="1"){
                #browser()
                renderSimpleNetwork({
                
                  plotSimpleNetwork(pd)
                  
                  
                })
                # output$gotPlot <- renderText({"Simple network plot successful"})
              } else {
                renderForceNetwork({
                  plotForceNetwork(pd)
                  
                })
                # output$gotPlot <- renderText({"Force network plot successful"})
                
              }
              # browser()
              # checkInputs(fData)
  })
    
  })

 } #wrap server function
# Run the application 
shinyApp(ui = ui, server = server)
