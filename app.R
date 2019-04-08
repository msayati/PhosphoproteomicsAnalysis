# Required libraries
library(shiny)
library(readxl)

# Required R Scripts
source("cleaning.R")
#source("hist_create.R")

# Changes shiny limit file upload to 40MB
options(shiny.maxRequestSize = 40*1024^2)

# UI for app
ui <- fluidPage(
   
  # App title
  titlePanel("Phosphoproteomics Analysis"),
  
  # Website layout
  sidebarLayout(
    
    # our inputs will go here
    sidebarPanel(
      
      # user can decide if they want to share their data with us
      checkboxInput("shareData", "Leave checked if you would like to share your data", TRUE),
      
      # user can upload an excel file
      fileInput("file", "Choose Excel File", multiple = FALSE,
                accept = c(".xlsx"),
                width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected"),
      
      # user can choose a sheet location number
      numericInput("sheet", "Sheet", 1, min = 1, max = 100),
     
      # user can choose a threshold number
      numericInput("threshold", "Desired Threshold Percentage", 40, min = 1, max = 100),
      
      # user can select (max) the top 5 predictions
      selectInput("topPredcitionNumInput", "Top Predction(s)",
                  choices = c("1", "2", "3", "4", "5"))
    ),
    
    # results will go here
    mainPanel(
      
      # displays a table with the sheet contents
      tableOutput("contents"),
      
      # breaks/spacing
      br(),
      br(),
      
      # displays histogram of correlations
      plotOutput(outputId = "corrPlot")
    )
  )
)

# Server code for app
server <- function(input, output) {
  
  # reactive variables (if values change, app will automatically update with the new values)
  currentSheet <- reactive({ input$sheet })
  currentThreshold <- reactive({ input$threshold })
  currentTPNI <- reactive({ input$topPredcitionNumInput })
  
  # reactive inputs must be wrapped in a render function
  observe({print(input$sheet)})
  observe({print(input$threshold)})
  observe({print(input$topPredcitionNumInput)})
  
  # Displays data uploaded onto website
  # Note: for now, it's accessing sheet 1, sheet 2 crashes (I think due to size)
  output$contents <- renderTable({
    # Dataframe stores the uploaded file
    inFile <- input$file
    #checks if no file has been uploaded
    if(is.null(inFile))
      return(NULL)
    
    # Prints in console
    #print(str(inFile))
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    read_excel(paste(inFile$datapath, ".xlsx", sep=""), currentSheet())
  })
  
  # histogram of correlations of clean data
  output$corrPlot <- renderPlot({
    inFile <- input$file
     # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
    #cleans the given data
    #FIX: passing a dataframe not an excel file so program will display errors. Need to fix cleaning.R
    cleandata <- clean.bcd(input$file, currentSheet(), currentThreshold())
    
    # computes correlation & contains upper triangle of corr (stored in dataframe)
    all_corr <- all_paircorr(cleandata)
    
    # historgram of correlations
    hist(all_corr)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)