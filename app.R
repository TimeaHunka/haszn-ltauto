
ibrary(tidyverse)
 brand <- cars_data %>% 
  group_by(brand) %>% 
  nest
 
 
  

library(shiny)

ui <- fluidPage(
  h6("minta")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

