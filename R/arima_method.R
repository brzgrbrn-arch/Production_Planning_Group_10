library(shiny)
library(readxl)
library(forecast)

# UI
arima_method_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        numericInput(ns("freq"), "Time Series Frequency (1 = Non-seasonal, 4 = Quarterly, 12 = Monthly):", 
                     value = 4, min = 1, max = 52, step = 1),
        numericInput(ns("horizon"), "Periods ahead to forecast (h):", 
                     value = 4, min = 1, max = 24, step = 1),
        checkboxInput(ns("seasonal_chk"), "Include Seasonal ARIMA search", value = TRUE),
        helpText("Automatically converts data to a Time Series (ts) object, plots ACF & PACF, and finds the best ARIMA model using auto.arima().")
      ),
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("ACF and PACF Correlograms"),
        fluidRow(
          column(6, plotOutput(ns("acf_plot"))),
          column(6, plotOutput(ns("pacf_plot")))
        ),
        hr(),
        h4("Best Model Summary (auto.arima)"),
        verbatimTextOutput(ns("model_summary")),
        hr(),
        h4("Error measures"),
        tableOutput(ns("error_measures_table")),
        hr(),
        h4("Future Forecasts (with 80% and 95% Confidence Intervals)"),
        tableOutput(ns("future_table")),
        hr(),
        h4("In-Sample Fits and Residuals by period"),
        tableOutput(ns("period_table"))
      )
    )
  )
}

# SERVER
arima_method_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    raw_data <- reactive({
      req(input$file)
      read_excel(input$file$datapath)
    })
    
    output$col_selector <- renderUI({
      req(raw_data())
      cols <- names(raw_data())
      selected_col <- grep("demand|talep|satis", cols, ignore.case = TRUE, value = TRUE)
      if (length(selected_col) == 0) selected_col <- cols[1]
      selectInput(session$ns("demand_col"), "Demand column", choices = cols, selected = selected_col[1])
    })
    
    demand_vector <- reactive({
      req(raw_data(), input$demand_col)
      vec <- as.numeric(raw_data()[[input$demand_col]])
      na.omit(vec)
    })
    
    ts_data <- reactive({
      req(demand_vector(), input$freq)
      ts(demand_vector(), frequency = input$freq)
    })
    
    sonuc <- reactive({
      req(ts_data(), input$horizon)
      y_ts <- ts_data()
      d <- as.numeric(y_ts)
      h <- input$horizon
      
      # auto.arima ile en iyi modeli bul
      fit <- auto.arima(y_ts, seasonal = input$seasonal_chk)
      fc <- forecast(fit, h = h)
      
      F_vals <- as.numeric(fitted(fit))
      future_f <- as.numeric(fc$mean)
      
      list(d = d, y_ts = y_ts, fit = fit, fc = fc, 
           F_vals = F_vals, future_f = future_f, h = h)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Best Model: ", forecast:::arima.string(s$fit), 
             " - Next Period Forecast: ", sprintf("%.2f", s$future_f[1]))
    })
    
    output$forecast_plot <- renderPlot({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      h <- s$h
      N_len <- length(d)
      future_f <- s$future_f
      
      all_vals <- c(d, F_vals, future_f)
      y_min <- min(all_vals, na.rm = TRUE) * 0.92
      y_max <- max(all_vals, na.rm = TRUE) * 1.08
      
      plot(1:N_len, d, type = "o", pch = 16, col = "#555555",
           xlab = "Period", ylab = "Demand", ylim = c(y_min, y_max),
           xlim = c(1, N_len + h),
           main = paste0("ARIMA Forecast (", forecast:::arima.string(s$fit), ")"))
      
      lines(1:N_len, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      points((N_len + 1):(N_len + h), future_f, pch = 17, col = "#b85324", cex = 1.4)
      lines((N_len):(N_len + h), c(F_vals[N_len], future_f), col = "#b85324", lty = 3, lwd = 1.8)
      
      legend("topleft", legend = c("Actual Demand", "ARIMA Fitted", "Future Forecast"),
             col = c("#555555", "#2b7bba", "#b85324"),
             pch = c(16, 17, 17), lty = c(1, 2, 3), bty = "n")
    })
    
    output$acf_plot <- renderPlot({
      req(ts_data())
      acf(as.numeric(ts_data()), main = "ACF (Autocorrelation Function)")
    })
    
    output$pacf_plot <- renderPlot({
      req(ts_data())
      pacf(as.numeric(ts_data()), main = "PACF (Partial Autocorrelation)")
    })
    
    output$model_summary <- renderPrint({
      req(sonuc())
      summary(sonuc()$fit)
    })
    
    output$error_measures_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      N_len <- length(d)
      
      e <- F_vals - d
      mad_val  <- mean(abs(e))
      mse_val  <- mean(e^2)
      mape_val <- mean(abs(e / d)) * 100
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(c(mad_val, mse_val, mape_val), 2),
        "Periods evaluated" = rep(paste0("1-", N_len), 3),
        check.names = FALSE
      )
    })
    
    output$future_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      N_len <- length(s$d)
      h <- s$h
      fc <- s$fc
      
      data.frame(
        Period = (N_len + 1):(N_len + h),
        "Point Forecast" = round(as.numeric(fc$mean), 2),
        "Lo 80" = round(as.numeric(fc$lower[, 1]), 2),
        "Hi 80" = round(as.numeric(fc$upper[, 1]), 2),
        "Lo 95" = round(as.numeric(fc$lower[, 2]), 2),
        "Hi 95" = round(as.numeric(fc$upper[, 2]), 2),
        check.names = FALSE
      )
    })
    
    output$period_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      e <- s$F_vals - d
      
      data.frame(
        Period = 1:length(d),
        "Demand (D)" = sprintf("%.2f", d),
        "Fitted (F)" = sprintf("%.2f", s$F_vals),
        "Error (e)" = sprintf("%.2f", e),
        check.names = FALSE
      )
    })
  })
}