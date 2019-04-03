# Required libraries

library(shiny)

# UI for app
ui <- fluidPage(
   
   # App title
   titlePanel("Phosphoproteomics Analysis")
   
)

# Server code
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

