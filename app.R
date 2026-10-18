library(shiny)

source("R/ogrenme.R")
source("R/moving_average.R")
source("R/weighted_moving_average.R")
source("R/exponential_smoothing.R")
source("R/seasonal_stationary.R")
source("R/holt_method.R")
source("R/seasonal_decomposition.R")
source("R/winters_method.R")
source("R/arima_method.R")

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_ui("learning")),
                 
                 navbarMenu("Forecasting",
                            # --- 1. GRUP: STATIONARY ---
                            "Stationary",
                            tabPanel("Moving Average", moving_average_ui("moving_average")),
                            tabPanel("Weighted Moving Average", weighted_moving_average_ui("w_moving_average")),
                            tabPanel("Exponential Smoothing", exponential_smoothing_ui("exp_smoothing")),
                            tabPanel("Quick & Dirty / Seasonal Factors", seasonal_stationary_ui("seasonal_stat")),
                            
                            "----",
                            
                            # --- 2. GRUP: TREND ---
                            "Trend",
                            tabPanel("Holt's Method", holt_method_ui("holt_method")),
                            tabPanel("Seasonal Decomp (Moving Average)", seasonal_decomposition_ui("seasonal_decomp")),
                            tabPanel("Winter's Method", winters_method_ui("winters_method")),
                            
                            "----",
                            
                            # --- 3. GRUP: TIME SERIES & ARIMA ---
                            "Time Series / ARIMA",
                            tabPanel("ACF, PACF & auto.arima", arima_method_ui("arima_method"))
                 )
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
  weighted_moving_average_server("w_moving_average")
  exponential_smoothing_server("exp_smoothing")
  seasonal_stationary_server("seasonal_stat")
  holt_method_server("holt_method")
  seasonal_decomposition_server("seasonal_decomp")
  winters_method_server("winters_method")
  arima_method_server("arima_method")
}

shinyApp(ui, server)