# Required libraries
library(shiny)
library(readxl)

# Required R Scripts
source("cleaning.R")
source("hist_create.R")
source("kinase_correlation.R")
source("naive_bayes.R")

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
      
      hr(),
      
      helpText("Note: Please select the inputs before uploading an excel file. Errors may occur if you change the settings after uploading the file."),
      
      # user can choose a sheet location number
      tags$div(title="Sheet: sheet number where the data is located.",
        numericInput("sheet", "Sheet", 1, min = 1, max = 100)
      ),
      
      # when user hovers over this area, text will appear
      tags$div(title="Missing Information Limit: percentage of NA data in columns that will be ignored (threshold). Ex. 40%, will compute correlations with columns that have less than 40% NA.",
        # user can choose a threshold number
        numericInput("threshold", "Missing Information Limit", 40, min = 1, max = 100)
      ),
      
      # user can select (max) the top 5 predictions
      tags$div(title="Top Prediction(s): number of predictions to be calculated.",
        selectInput("topPredcitionNumInput", "Top Prediction(s)",
                    choices = c("5", "1", "2", "3", "4"))
      ),
      
      hr(),
      
      # user can upload an excel file
      fileInput("file", "Choose Excel File", multiple = FALSE,
                accept = c(".xlsx"),
                width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected")
    ),
    
    # results will go here
    mainPanel(
      
      # tabsets
      tabsetPanel(
        # displays a table with the sheet contents
        tabPanel("Data", tableOutput("contents")),
        
        # displays histogram of correlations
        tabPanel("Co-phosphorylation", plotOutput(outputId = "corrPlot"), br(),
                 plotOutput(outputId = "kinasecPlot")),
        
        # displays a table with the desired top predictions
        tabPanel("Predictions", tableOutput("topPredTable")),
        
        tabPanel("Cophosk+", tableOutput("cplus")),
        
        tabPanel("KSEA", plotOutput(outputId = "ksea"))
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
  # this is the actual data that we can use to find correlations
  # note: this will only grab the current sheet that has been selected before upload. It will NOT update hist
  rawData <- eventReactive(input$file, {
    read_excel(input$file$datapath, sheet = currentSheet())
  })
  
  # reactive inputs must be wrapped in a render function
  observe({print(input$sheet)})
  observe({print(input$threshold)})
  observe({print(input$topPredcitionNumInput)})
  observe({print(rawData())})
  
  # Displays current sheet data uploaded onto website (will update if sheet changed)
  output$contents <- renderTable({
    # Dataframe stores the uploaded file
    inFile <- input$file
    #checks if no file has been uploaded
    if(is.null(inFile))
      return(NULL)
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    read_excel(paste(inFile$datapath, ".xlsx", sep=""), currentSheet())
  })
  
  # histogram of correlations of clean data: "Vector A"
  output$corrPlot <- renderPlot({
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
     # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
    #cleans the given data
    cleandata <- clean.bcd(rawData(), currentSheet(), currentThreshold())
    
    # computes correlation & contains upper triangle of corr (stored in dataframe)
    all_corr <- all_paircorr(cleandata)
    
    hist(all_corr, main="Histogram for Correlation of the Clean Data")
  })
  
  # histogram from kinase_correlation.R : "Vector S"
  output$kinasecPlot <- renderPlot({
    #cleans the given data
    cleandata <- clean.bcd(rawData(), currentSheet(), currentThreshold())
    
    # computes correlation & contains upper triangle of corr (stored in dataframe)
    all_corr <- all_paircorr(cleandata)
    
    # computes correlation with kinase_human.txt & cleandata
    vectorS <- kinase.correlation(cleandata)
    
    hist(all_corr, main="Histogram for Correlation of the Kinase_human.txt")
  })
  
  # Displays current sheet data uploaded onto website (will update if sheet changed)
  output$topPredTable <- renderTable({
    #cleans the given data
    cleandata <- clean.bcd(rawData(), currentSheet(), currentThreshold())
    
    # computes correlation & contains upper triangle of corr (stored in dataframe)
    all_corr <- all_paircorr(cleandata)
    
    # computes correlation with kinase_human.txt & cleandata
    vectorS <- kinase.correlation(cleandata)
    
    # need to verify the correct variables for the params; S? A?
    nb <- naive_bayes(S, A, currentTPNI(), test=FAlSE, cleanBCD)
    
    # displays table
    nb
  })
  
  # a table
  output$cplus <- renderTable({
    
  })
  
  # a histogram
  output$ksea <- renderPlot({
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)