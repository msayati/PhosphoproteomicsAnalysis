# Required libraries
library(shiny)
library(readxl)

# Required R Scripts
source("cleaning.R")
source("hist_create.R")
#source("naive_bayes.R") #edit function? error: object cleanBCD not found

# Changes shiny limit file upload to 40MB
options(shiny.maxRequestSize = 100*1024^2)

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
      numericInput("threshold", "Missing Information Limit", 40, min = 1, max = 100),
      
      # user can select (max) the top 5 predictions
      selectInput("topPredcitionNumInput", "Top Prediction(s)",
                  choices = c("5", "1", "2", "3", "4"))
    ),
    
    # results will go here
    mainPanel(
      
      # tabsets
      tabsetPanel(
        # displays a table with the sheet contents
        tabPanel("Data", tableOutput("contents")),
        
        # displays histogram of correlations
        tabPanel("Co-phosphorylation", plotOutput(outputId = "corrPlot")),
        
        # displays a table with the desired top predictions
        tabPanel("Predictions", tableOutput("topPredTable"))#,
        
        #tabPanel("Cophosk+", ),
        
        #tabPanel("ksea", )
      )
      
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
  
  # Displays current sheet data uploaded onto website (will update if sheet changed)
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
  
  # Displays current sheet data uploaded onto website (will update if sheet changed)
  output$topPredTable <- renderTable({
    # fix error about cleanBCD
  })
}

# Run the application 
shinyApp(ui = ui, server = server)