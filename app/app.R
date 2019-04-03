# Required libraries
library(shiny)

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
  #FIX: inFile <- input$file
}

# Run the application 
shinyApp(ui = ui, server = server)

