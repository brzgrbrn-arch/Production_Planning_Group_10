library(shiny)
library(readxl)

# UI
winters_method_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        numericInput(ns("season_len"), "Season length (m - e.g., 4 or 12):", value = 4, min = 2, max = 52, step = 1),
        sliderInput(ns("alpha"), "Level smoothing (Alpha):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
        sliderInput(ns("beta"), "Trend smoothing (Beta):", min = 0.01, max = 0.99, value = 0.1, step = 0.01),
        sliderInput(ns("gamma"), "Seasonal smoothing (Gamma):", min = 0.01, max = 0.99, value = 0.1, step = 0.01),
        numericInput(ns("horizon"), "Periods ahead to forecast:", value = 4, min = 1, max = 24, step = 1)
      ),
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("Initial Parameters (Regression Based)"),
        tableOutput(ns("init_table")),
        hr(),
        h4("Error measures"),
        tableOutput(ns("error_measures_table")),
        hr(),
        h4("Winter's Components & Forecasts by period"),
        tableOutput(ns("period_table"))
      )
    )
  )
}

# SERVER
winters_method_server <- function(id) {
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
      req(demand_vector(), input$season_len, input$alpha, input$beta, input$gamma)
      d <- demand_vector()
      m <- input$season_len
      alpha <- input$alpha
      beta <- input$beta
      gamma <- input$gamma
      h <- input$horizon
      N_len <- length(d)
      
      # Baslangic Degerleri: Basit Regresyon ile L0, T0 ve Mevsim Faktoru (S) tahmini
      t_vec <- 1:N_len
      fit <- lm(d ~ t_vec)
      L0 <- as.numeric(coef(fit)[1])
      T0 <- as.numeric(coef(fit)[2])
      
      trend_init <- L0 + T0 * t_vec
      raw_ratios <- d / trend_init
      season_idx <- ((0:(N_len - 1)) %% m) + 1
      
      S_init <- numeric(m)
      for (s in 1:m) {
        S_init[s] <- mean(raw_ratios[season_idx == s], na.rm = TRUE)
      }
      S_init <- S_init * (m / sum(S_init))
      
      # Vektorler
      L <- numeric(N_len)
      Tr <- numeric(N_len)
      S_vec <- numeric(N_len + m)
      S_vec[1:m] <- S_init
      F_vals <- numeric(N_len)
      
      # Dongu (Carpimsal Holt-Winters)
      for (t in 1:N_len) {
        prev_L <- if (t == 1) L0 else L[t - 1]
        prev_T <- if (t == 1) T0 else Tr[t - 1]
        prev_S <- S_vec[t]
        
        F_vals[t] <- (prev_L + prev_T) * prev_S
        L[t] <- alpha * (d[t] / prev_S) + (1 - alpha) * (prev_L + prev_T)
        Tr[t] <- beta * (L[t] - prev_L) + (1 - beta) * prev_T
        S_vec[t + m] <- gamma * (d[t] / L[t]) + (1 - gamma) * prev_S
      }
      
      # Gelecek h donem tahmini
      future_forecasts <- numeric(h)
      for (k in 1:h) {
        season_pos <- N_len + ((k - 1) %% m) + 1
        future_forecasts[k] <- (L[N_len] + k * Tr[N_len]) * S_vec[season_pos]
      }
      
      list(d = d, L = L, Tr = Tr, S_used = S_vec[1:N_len], S_updated = S_vec[(m + 1):(N_len + m)],
           F_vals = F_vals, future_forecasts = future_forecasts,
           L0 = L0, T0 = T0, S_init = S_init, m = m, h = h)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Winter's Method - Next Period Forecast: ", sprintf("%.2f", s$future_forecasts[1]))
    })
    
    output$init_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      data.frame(
        Parameter = c("Initial Level (L0)", "Initial Trend (T0)", paste0("Initial S", 1:s$m)),
        Value = round(c(s$L0, s$T0, s$S_init), 4)
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
           main = "Winter's Multiplicative Method Forecast")
      
      lines(1:N_len, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      points((N_len + 1):(N_len + h), future_f, pch = 17, col = "#b85324", cex = 1.4)
      lines((N_len):(N_len + h), c(F_vals[N_len], future_f), col = "#b85324", lty = 3, lwd = 1.8)
      
      legend("topleft", legend = c("Actual Demand", "Winter's Forecast", "Future Forecast"),
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
        "Trend (T)" = sprintf("%.2f", s$Tr),
        "Seasonal (S_t)" = sprintf("%.4f", s$S_used),
        "Forecast (F)" = sprintf("%.2f", s$F_vals),
        "Error (e)" = sprintf("%.2f", e),
        check.names = FALSE
      )
    })
  })
}