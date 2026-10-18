library(shiny)
library(readxl)

seasonal_decomposition_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        numericInput(ns("season_len"), "Season length (m - e.g., 4 or 12):", value = 4, min = 2, max = 52, step = 1),
        numericInput(ns("future_periods"), "Periods ahead to forecast:", value = 4, min = 1, max = 24, step = 1)
      ),
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("Seasonal Indices and Trend Parameters"),
        tableOutput(ns("factors_table")),
        hr(),
        h4("Error measures"),
        tableOutput(ns("error_measures_table")),
        hr(),
        h4("Decomposition and Forecasts by period"),
        tableOutput(ns("period_table"))
      )
    )
  )
}

seasonal_decomposition_server <- function(id) {
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
      
      cma <- rep(NA, N_len)
      if (N_len > m) {
        if (m %% 2 == 1) {
          k <- (m - 1) / 2
          for (t in (k + 1):(N_len - k)) {
            cma[t] <- mean(d[(t - k):(t + k)])
          }
        } else {
          k <- m / 2
          for (t in (k + 1):(N_len - k)) {
            ma1 <- mean(d[(t - k):(t + k - 1)])
            ma2 <- mean(d[(t - k + 1):(t + k)])
            cma[t] <- (ma1 + ma2) / 2
          }
        }
      }
      
      ratios <- d / cma
      season_idx <- ((0:(N_len - 1)) %% m) + 1
      
      seasonal_factors <- numeric(m)
      for (s in 1:m) {
        vals <- ratios[season_idx == s]
        seasonal_factors[s] <- if (all(is.na(vals))) 1 else mean(vals, na.rm = TRUE)
      }
      seasonal_factors <- seasonal_factors * (m / sum(seasonal_factors))
      
      deseasonalized <- d / seasonal_factors[season_idx]
      t_vec <- 1:N_len
      fit <- lm(deseasonalized ~ t_vec)
      intercept <- as.numeric(coef(fit)[1])
      slope <- as.numeric(coef(fit)[2])
      
      trend_line <- intercept + slope * t_vec
      F_vals <- trend_line * seasonal_factors[season_idx]
      
      future_t <- (N_len + 1):(N_len + h)
      future_season_idx <- (((N_len):(N_len + h - 1)) %% m) + 1
      future_trend <- intercept + slope * future_t
      future_forecasts <- future_trend * seasonal_factors[future_season_idx]
      
      list(d = d, cma = cma, deseasonalized = deseasonalized,
           trend_line = trend_line, F_vals = F_vals, 
           future_forecasts = future_forecasts,
           seasonal_factors = seasonal_factors, season_idx = season_idx,
           intercept = intercept, slope = slope, m = m, h = h)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Seasonal Decomposition (MA) - Next Period Forecast: ", sprintf("%.2f", s$future_forecasts[1]))
    })
    
    output$factors_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      data.frame(
        Season = paste0("Season ", 1:s$m),
        "Seasonal Index (S)" = round(s$seasonal_factors, 4),
        "Trend Intercept (a)" = round(rep(s$intercept, s$m), 2),
        "Trend Slope (b)" = round(rep(s$slope, s$m), 4),
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
           main = "Seasonal Decomposition (Moving Average and Trend)")
      
      lines(1:N_len, s$trend_line, col = "#2ca02c", lty = 4, lwd = 1.5)
      lines(1:N_len, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      points((N_len + 1):(N_len + h), future_f, pch = 17, col = "#b85324", cex = 1.4)
      lines((N_len):(N_len + h), c(F_vals[N_len], future_f), col = "#b85324", lty = 3, lwd = 1.8)
      
      legend("topleft", legend = c("Actual Demand", "Trend Line", "Seasonal Forecast", "Future Forecast"),
             col = c("#555555", "#2ca02c", "#2b7bba", "#b85324"),
             pch = c(16, NA, 17, 17), lty = c(1, 4, 2, 3), bty = "n")
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
        "CMA" = ifelse(is.na(s$cma), "", sprintf("%.2f", s$cma)),
        "Deseasonalized" = sprintf("%.2f", s$deseasonalized),
        "Trend (T)" = sprintf("%.2f", s$trend_line),
        "Seasonal Index (S)" = sprintf("%.4f", s$seasonal_factors[s$season_idx]),
        "Forecast (F)" = sprintf("%.2f", s$F_vals),
        "Error (e)" = sprintf("%.2f", e),
        check.names = FALSE
      )
    })
  })
}