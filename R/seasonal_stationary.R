library(shiny)
library(readxl)

# UI
seasonal_stationary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        numericInput(ns("season_len"), "Season length (m - e.g., 4 for Quarterly, 12 for Monthly):", 
                     value = 4, min = 2, max = 52, step = 1),
        numericInput(ns("future_periods"), "Periods ahead to forecast:", 
                     value = 4, min = 1, max = 24, step = 1),
        helpText("Calculates static Seasonal Factors (Indices) using the Simple Averages (Quick & Dirty) method for stationary seasonal series.")
      ),
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("Seasonal Factors (Indices) by Season"),
        tableOutput(ns("factors_table")),
        hr(),
        h4("Error measures"),
        tableOutput(ns("error_measures_table")),
        hr(),
        h4("Forecasts by period"),
        tableOutput(ns("period_table"))
      )
    )
  )
}

# SERVER
seasonal_stationary_server <- function(id) {
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
      req(demand_vector(), input$season_len, input$future_periods)
      d <- demand_vector()
      m <- input$season_len
      h <- input$future_periods
      N_len <- length(d)
      
      # Genel ortalama (Grand Mean)
      overall_mean <- mean(d)
      
      # Her dönemin hangi mevsime (1..m) denk geldiği
      season_idx <- ((0:(N_len - 1)) %% m) + 1
      
      # Mevsimsel Faktörler (Quick & Dirty: Mevsim Ortalaması / Genel Ortalama)
      seasonal_factors <- numeric(m)
      for (s in 1:m) {
        seasonal_factors[s] <- mean(d[season_idx == s]) / overall_mean
      }
      # Toplamları m olacak şekilde normalize et
      seasonal_factors <- seasonal_factors * (m / sum(seasonal_factors))
      
      # Dönem içi tahminler: Genel Ortalama * Mevsim Faktörü
      F_vals <- overall_mean * seasonal_factors[season_idx]
      
      # Gelecek dönem tahminleri
      future_season_idx <- (((N_len):(N_len + h - 1)) %% m) + 1
      future_forecasts <- overall_mean * seasonal_factors[future_season_idx]
      
      list(d = d, F_vals = F_vals, future_forecasts = future_forecasts,
           seasonal_factors = seasonal_factors, season_idx = season_idx,
           overall_mean = overall_mean, m = m, h = h)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Quick & Dirty Seasonal Method - Next Period Forecast: ", sprintf("%.2f", s$future_forecasts[1]))
    })
    
    output$factors_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      data.frame(
        Season = paste0("Season ", 1:s$m),
        "Seasonal Factor (Index)" = round(s$seasonal_factors, 4),
        "Overall Stationary Mean" = round(rep(s$overall_mean, s$m), 2),
        check.names = FALSE
      )
    })
    
    output$forecast_plot <- renderPlot({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      h <- s$h
      N_len <- length(d)
      future_f <- s$future_forecasts
      
      all_vals <- c(d, F_vals, future_f)
      y_min <- min(all_vals, na.rm = TRUE) * 0.92
      y_max <- max(all_vals, na.rm = TRUE) * 1.08
      
      plot(1:N_len, d, type = "o", pch = 16, col = "#555555",
           xlab = "Period", ylab = "Demand", ylim = c(y_min, y_max),
           xlim = c(1, N_len + h),
           main = "Stationary Seasonal Forecast (Quick & Dirty / Seasonal Factors)")
      
      lines(1:N_len, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      points((N_len + 1):(N_len + h), future_f, pch = 17, col = "#b85324", cex = 1.4)
      lines((N_len):(N_len + h), c(F_vals[N_len], future_f), col = "#b85324", lty = 3, lwd = 1.8)
      
      legend("topleft", legend = c("Actual Demand", "Seasonal Fit", "Future Forecast"),
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
        Season = s$season_idx,
        "Demand (D)" = sprintf("%.2f", d),
        "Seasonal Factor (S)" = sprintf("%.4f", s$seasonal_factors[s$season_idx]),
        "Forecast (F)" = sprintf("%.2f", s$F_vals),
        "Error (e)" = sprintf("%.2f", e),
        check.names = FALSE
      )
    })
  })
}