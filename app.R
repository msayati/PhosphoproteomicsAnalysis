# Required libraries
library(shiny)
library(readxl)
library(ggplot2)

# Required R Scripts
source("cleaning.R")
source("hist_create.R")
source("kinase_correlation.R")
source("naive_bayes.R")
source("KSEA.R")

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
        tabPanel("Co-phosphorylation", plotOutput(outputId = "corrPlot"), downloadButton("downloadH1", "Download"), br(),
                 plotOutput(outputId = "kinasecPlot"), downloadButton("downloadH2", "Download")),
        
        # displays a table with the desired top predictions
        tabPanel("CoPhosK Predictions", tableOutput("topPredTable")),
        
        # displays KSEA plot
        tabPanel("KSEA", plotOutput(outputId = "ksea"), downloadButton("downloadKSEA", "Download"))
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
  
  cleandata <- reactive({ 
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    #cleans the given data
    clean.bcd(rawData(), currentSheet(), currentThreshold())
  })
  
  all_corr <- reactive({ 
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    # computes correlation & contains upper triangle of corr (stored in dataframe)
    all_paircorr(cleandata())
  })
  
  vectorS <- reactive({ 
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    # computes correlation with kinase_human.txt & cleandata
    kinase.correlation(cleandata())
  })
  
  top10 <- reactive({
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
    KSEA(cleandata())
  })
  # reactive inputs must be wrapped in a render function
  observe({print(input$sheet)})
  observe({print(input$threshold)})
  observe({print(as.numeric(input$topPredcitionNumInput))})
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
    
    hist(all_corr(), main="Histogram for Correlation of the Clean Data")
  })
  
  #filename and type will not display properly in RStudio, but it will in browsers
  output$downloadH1 <- downloadHandler(
    filename = function() {
      paste("histogram1", ".png", sep = "")
    },
    content = function(filename) {
      png(filename)
      hist(all_corr(), main="Histogram for Correlation of the Clean Data")
      dev.off()
    }
  )
  
  # histogram from kinase_correlation.R : "Vector S"
  output$kinasecPlot <- renderPlot({
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
    hist(vectorS(), main="Histogram for Correlation of Shared Substrates")
  })
  
  output$downloadH2 <- downloadHandler(
    filename = function() {
      paste("histogram2", ".png", sep = "")
    },
    content = function(filename) {
      png(filename)
      hist(vectorS(), main="Histogram for Correlation of the Clean Data")
      dev.off()
    }
  )
  
  # Displays current sheet data uploaded onto website (will update if sheet changed)
  output$topPredTable <- renderTable({
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
    nb <- naive_bayes(vectorS(), all_corr(), currentTPNI(), test=TRUE, cleandata())
    
    # displays table
    nb[[2]]
  })
  
  # a histogram
  output$ksea <- renderPlot({
    inFile <- input$file
    print(inFile, digits = NULL,
          quote = FALSE, right = TRUE, row.names = FALSE, max = NULL)
    # if no file has been uploaded (to avoid bugs)
    if (is.null(inFile)) {
      return(NULL)
    }
    
     ggplot(top10(), aes(x=Kinase,y=score)) +
     geom_bar(stat="identity") +
     scale_x_discrete(limits=top10()$Kinase) +
     coord_flip() + scale_color_brewer(palette="Paired") + theme_classic()
  })
  
  # download KSEA plot
  output$downloadKSEA <- downloadHandler(
    filename = function() { paste("ksea", '.png', sep='') },
    content = function(file) {
      ggsave(file,ggplot(top10(), aes(x=Kinase,y=score)) +
               geom_bar(stat="identity") +
               scale_x_discrete(limits=top10()$Kinase) +
               coord_flip() + scale_color_brewer(palette="Paired") + theme_classic())
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)