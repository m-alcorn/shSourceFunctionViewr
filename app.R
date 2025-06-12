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
library(shinythemes)
source("networkFunctions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),

    # Application title
    titlePanel("Source Function Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel( p("This tool uses a visual network representation of how functions 
                                   are related to each other in the model."), 
                    p("To get started: Select a file containing Functions exported from your Source model."),
                    p("To filter results: Click on a single or multiple option from the
                                  dropdown list. (To remove: click to highlight, then delete)"),
        radioButtons("fChoice",
                     "Use existing file, or new...",
                     # selected = character(0),
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
        # actionButton("btnPlot","PlotData..."),
        textOutput("gotPlot"),
        br(),
        h3("Use the drop down list below to filter Functions"),
          selectInput( 
            "select", 
            "Add filter terms:", 
            list(), 
            multiple = TRUE 
          ),
        textOutput("gotFilters")
          # br(),
          # verbatimTextOutput("fListPath"),
          # br()
          # radioButtons("plotType", label = h3("Choose a plot type.."),
          #              choices = list("Simple Network" = 1, "Force Network" = 2), 
          #              selected = 1)
       
        ),
      

        # Show a plot of the generated distribution
        mainPanel( 
          tabsetPanel(type="tab", 
                      tabPanel("Main Plot",
                              
                               
                               br(),
                               verbatimTextOutput("statusMessage"),
                               
                               forceNetworkOutput("plotFrame1", height = "800px")
                               ),
                      tabPanel("Data", 
                               DT::dataTableOutput("functionsData")
                               )
                      
          )
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
    #
    f <- fData$inFile
    #
    fData$actualInputType <- checkFileType(f)
    #
    #
    
    if(fData$actualInputType==fData$selectedInputType){
      #
      if(!is.null(fData$data)){
        
        fData$validInputs <- "TRUE"
        fData$currentMessage <- "All inputs are currently valid"
        
        #
      } else { 
        fData$validInputs <- "FALSE"
        fData$currentMessage <- "No valid data currently loaded"
        }
      
    } else {
      fData$currentMessage <- "No valid data currently loaded"
      fData$validInputs <- "FALSE"
      
    }
    
    
    
  }

  output$statusMessage <- renderPrint({fData$currentMessage})
 
 
    output$datafile <- renderUI({
      
      fileInput("datafile","Open *.csv", #UPDATE: RESTRICT TO CSV
                
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),
                placeholder = "./data/FunctionsListFile.csv"
      )
    })

  
  observe({
  
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
      output$gotData <- renderText({"Loading Data........"})
      req(input$datafile)
      d <- getData(input$datafile$datapath)
      
      
    } else{
      if(input$fChoice=="newFile"){ 
        output$gotData <- renderText({"Loading Data........"})
        req(input$datafile)
        d <-parseFunctionExport(input$datafile$datapath)
        
      }
    
    }
    output$gotData <- renderText({"Data loaded successfully."})
    
    fData$data <- d
    # 
    
    checkInputs(fData)
    return(d)
  })
  
 
  
  
  observe({
    if(is.null(fData$data)){
      return()
    }
    s <- input$select
    #
    if(!is.null(s)){
      pd <- filterData(s,fData$data)
      #
    } else {
      pd <- fData$data
      #
    }
    output$functionsData <- DT::renderDataTable({pd})
    
  })
 
  ch <- reactive({
    getFolderPaths(fData$data)
  })

  observe({
    if(is.null(fData$data)){
      return()
    }
    req(input$datafile)
    req(input$fChoice)
    
    # ch <- getFolderPaths(fData$data)
  
  updateSelectInput(inputId = "select",
                      choices = ch())
    # 
  checkInputs(fData)

  })

  
observe({       
      if(is.null(fData$data)){
        
        output$gotPlot <- renderText({"No Data loaded for plot..."})
        return()
      }
      s <- input$select
      #
      if(!is.null(s)){
        pd <- filterData(s,fData$data)
        #
      } else {
        pd <- fData$data
        #
      }
      req(pd)
      
      output$plotFrame1 <-renderForceNetwork({
          
          plotForceNetwork(pd)
          
          
        })
     
      checkInputs(fData)
    
    
  })
  
  

 } #wrap server function
# Run the application 
shinyApp(ui = ui, server = server)
