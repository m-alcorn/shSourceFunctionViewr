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
          "Select file containing function list...",
          fileInput("datafile","Open *.csv", #UPDATE: RESTRICT TO CSV
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          selectInput( 
            "select", 
            "Select options below:", 
            list(), 
            multiple = TRUE 
          ),
          br(),
          verbatimTextOutput("fListPath"),
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
         
         uiOutput("plotFrame1")
         # uiOutput("plotFrame1",
         #          div(style = "width: 100%; height: 500px; visibility: inherit")
         # )
        
          
       
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({
    req(input$datafile)
    d <- getData(input$datafile$name)
    
    # browser()
    return(d)
  })
  

  observe( {
    d <- data()
    req(d)
    ch <- getFolderPaths(d)
    updateSelectInput(inputId = "select",
                      choices = ch)
  })
  
  output$fListPath <- renderText({
    input$datafile$name
    
  })
  
  output$plotFrame1 <-  renderUI({
    s <- input$select
    # if(length(s)>1){browser()}
    if(!is.null(s)){
      pd <- filterData(s,data())
    } else {
      pd <- data()
    }
    req(pd)
    pt <- input$plotType
    if(pt==1){
      renderSimpleNetwork({
      
        plotSimpleNetwork(pd)
        
      })
    } else {
      renderForceNetwork({
        plotForceNetwork(pd)
      })
      
    }
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
