library(tidyverse)
 brand <- cars_data %>% 
  group_by(brand) %>% 
  nest
 
 
  

library(shiny)

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

