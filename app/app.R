# Required libraries
library(shiny)
library(readxl)

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
      fileInput("file", "Choose Excel File", multiple = FALSE,
                accept = c(".xlsx"),
                width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected")
    ),
    # results will go here
    mainPanel(tableOutput("contents"))
  )
)

# Server code for app
server <- function(input, output) {
  # Displays data uploaded onto website
  # Note: for now, it's accessing sheet 1, sheet 2 crashes (I think due to size)
  output$contents <- renderTable({
    # Dataframe stores the uploaded file
    inFile <- input$file
    
    # Prints in console
    #print(str(inFile))
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

