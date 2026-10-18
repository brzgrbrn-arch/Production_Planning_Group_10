library(shiny)
library(readxl)

# UI
holt_method_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        
        sliderInput(ns("alpha"), "Level smoothing (Alpha):", min = 0.01, max = 0.99, value = 0.3, step = 0.01),
        sliderInput(ns("beta"), "Trend smoothing (Beta):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
        
        radioButtons(ns("init_mode"), "Initialization Method:",
                     choices = c("Regression (Linear Fit)" = "regression", 
                                 "Difference (D1 & D2 - D1)" = "difference"),
                     selected = "regression"),
        
        numericInput(ns("horizon"), "Forecast periods ahead (m):", value = 1, min = 1, max = 12, step = 1)
      ),
      
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("Initial Values & Parameters"),
        tableOutput(ns("params_table")),
        hr(),
        h4("Error measures"),
        tableOutput(ns("error_measures_table")),
        hr(),
        h4("Components & Forecasts by period"),
        tableOutput(ns("period_table"))
      )
    )
  )
}

# SERVER
holt_method_server <- function(id) {
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
    
    sonuc <- reactive({
      req(demand_vector(), input$alpha, input$beta, input$init_mode)
      d <- demand_vector()
      alpha <- input$alpha
      beta <- input$beta
      m <- input$horizon
      N_len <- length(d)
      
      # Başlangıç Değerleri (Hocanın notundaki regresyon vs fark kuralı)
      if (input$init_mode == "regression") {
        fit <- lm(d ~ seq_along(d))
        L0 <- as.numeric(coef(fit)[1])
        T0 <- as.numeric(coef(fit)[2])
      } else {
        L0 <- d[1]
        T0 <- if (N_len >= 2) d[2] - d[1] else 0
      }
      
      L <- numeric(N_len)
      T_trend <- numeric(N_len)
      F_vals <- numeric(N_len)
      
      # 1. Dönem
      F_vals[1] <- L0 + T0
      L[1] <- alpha * d[1] + (1 - alpha) * (L0 + T0)
      T_trend[1] <- beta * (L[1] - L0) + (1 - beta) * T0
      
      # Dönemler (2..N)
      if (N_len >= 2) {
        for (t in 2:N_len) {
          F_vals[t] <- L[t - 1] + T_trend[t - 1]
          L[t] <- alpha * d[t] + (1 - alpha) * (L[t - 1] + T_trend[t - 1])
          T_trend[t] <- beta * (L[t] - L[t - 1]) + (1 - beta) * T_trend[t - 1]
        }
      }
      
      # Gelecek m dönemlik tahminler: F_{N+m} = L_N + m * T_N
      future_forecasts <- L[N_len] + (1:m) * T_trend[N_len]
      
      list(d = d, L = L, T_trend = T_trend, F_vals = F_vals, 
           future_forecasts = future_forecasts, L0 = L0, T0 = T0, 
           alpha = alpha, beta = beta, m = m)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Holt's Linear Trend — Next Period Forecast: ", sprintf("%.2f", s$future_forecasts[1]))
    })
    
    output$params_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      data.frame(
        Parameter = c("Alpha (Level)", "Beta (Trend)", "Initial Level (L0)", "Initial Trend (T0)"),
        Value = round(c(s$alpha, s$beta, s$L0, s$T0), 4)
      )
    })
    
    output$forecast_plot <- renderPlot({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      m <- s$m
      N_len <- length(d)
      future_f <- s$future_forecasts
      
      all_vals <- c(d, F_vals, future_f)
      y_min <- min(all_vals, na.rm = TRUE) * 0.95
      y_max <- max(all_vals, na.rm = TRUE) * 1.05
      
      plot(1:N_len, d, type = "o", pch = 16, col = "#555555",
           xlab = "Period", ylab = "Demand", ylim = c(y_min, y_max),
           xlim = c(1, N_len + m),
           main = "Holt's Linear Trend Forecast")
      
      lines(1:N_len, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      
      # Gelecek tahmin noktaları
      points((N_len + 1):(N_len + m), future_f, pch = 17, col = "#b85324", cex = 1.4)
      lines((N_len):(N_len + m), c(F_vals[N_len], future_f), col = "#b85324", lty = 3, lwd = 1.8)
      
      legend("topleft", legend = c("Actual Demand", "In-Sample Forecast", "Future Forecast"),
             col = c("#555555", "#2b7bba", "#b85324"),
             pch = c(16, 17, 17), lty = c(1, 2, 3), bty = "n")
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
    
    output$period_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      e <- s$F_vals - d
      
      data.frame(
        Period = 1:length(d),
        "Demand (D)" = sprintf("%.2f", d),
        "Level (L)" = sprintf("%.2f", s$L),
        "Trend (T)" = sprintf("%.2f", s$T_trend),
        "Forecast (F)" = sprintf("%.2f", s$F_vals),
        "Error (e)" = sprintf("%.2f", e),
        check.names = FALSE
      )
    })
  })
}