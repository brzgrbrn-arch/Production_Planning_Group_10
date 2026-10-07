library(shiny)

source("R/ogrenme.R")
source("R/moving_average.R")

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_ui("learning")),
                 tabPanel("Moving Average", moving_average_ui("moving_average"))
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
}

shinyApp(ui, server)