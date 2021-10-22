
library(tidyverse)
 brand <- cars_data %>% 
  group_by(brand) %>% 
  nest
 
 
  

library(shiny)
library(shinydashboard)
library(tidyverse)

load("cars_data.RData")
# sf::st_as_sf(maps::map('world', plot = F, fill = T))

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    shiny::sliderInput("year_filter", "Év:", min = 2000, max = 2020, value = c(2000, 2020), step = 1, sep = ""),
    shiny::sliderInput("price_filter", "Ár:", min = 0, max = 5e6, value = c(0, 5e6), step = 1e5, sep = " "),
    shiny::selectInput(inputId = "fuel", "Üzemanyag:", choices = 
                         c("Benzin", "Benzin/Gáz", "CNG", "Dízel", "Elektromos", "Etanol", "Hibrid", "Hibrid (Benzin)", "Hibrid (Dízel)", "LPG", "Egyéb" = NA)),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              tabBox(width = 8,
                     tabPanel(
                       "Márka szerinti elemzése",
              plotOutput("brand")
                     ),
              tabPanel(
                "Szín szerint"
              )
              )
      )
    )
  )
)









server <- function(input, output) {
  
  subdata <- reactive({
    cars_data %>% 
      filter(
        evjarat >= input$year_filter[1] &
        evjarat <= input$year_filter[2] &
        vetelar >= input$price_filter[1] &
        vetelar <= input$price_filter[2] &
        uzemanyag == input$fuel
      )
    
  })
  
  output$brand <- renderPlot({
    subdata() %>% 
      mutate(
        brand = fct_lump(brand, n = 10, other_level = "Egyéb")
      ) %>% 
      ggplot(aes(x = brand, y = vetelar, color = brand)) + 
      geom_boxplot()
  })
  
  

  }

shinyApp(ui, server)









