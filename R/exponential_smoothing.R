library(shiny)
library(readxl)

# UI
exponential_smoothing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        sliderInput(ns("alpha"), "Smoothing parameter (Alpha):", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
        radioButtons(ns("init_type"), "Initial forecast (F1):",
                     choices = c("First period actual (D1)" = "d1", "Average of first k periods" = "mean_k")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'mean_k'", ns("init_type")),
          numericInput(ns("k_val"), "k (Periods for average):", value = 3, min = 2, step = 1)
        )
      ),
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
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
exponential_smoothing_server <- function(id) {
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
      req(demand_vector(), input$alpha)
      d <- demand_vector()
      alpha <- input$alpha
      N_len <- length(d)
      
      F_vals <- rep(NA, N_len)
      if (input$init_type == "d1") {
        F_vals[1] <- d[1]
      } else {
        k <- min(input$k_val, N_len)
        F_vals[1] <- mean(d[1:k])
      }
      
      for (t in 2:N_len) {
        F_vals[t] <- alpha * d[t - 1] + (1 - alpha) * F_vals[t - 1]
      }
      next_f <- alpha * d[N_len] + (1 - alpha) * F_vals[N_len]
      
      list(d = d, F_vals = F_vals, next_f = next_f, alpha = alpha)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("Exponential Smoothing (alpha = ", s$alpha, ") - next-period forecast: ", sprintf("%.2f", s$next_f))
    })
    
    output$forecast_plot <- renderPlot({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      n_total <- length(d)
      
      y_min <- min(c(d, F_vals, s$next_f), na.rm = TRUE) * 0.95
      y_max <- max(c(d, F_vals, s$next_f), na.rm = TRUE) * 1.05
      
      plot(1:n_total, d, type = "o", pch = 16, col = "#555555",
           xlab = "Period", ylab = "Demand", ylim = c(y_min, y_max),
           main = paste0("Actual demand and Exp. Smoothing (alpha = ", s$alpha, ") forecast"),
           xlim = c(1, n_total + 1))
      
      lines(1:n_total, F_vals, type = "o", pch = 17, col = "#2b7bba", lty = 2)
      points(n_total + 1, s$next_f, pch = 17, col = "#b85324", cex = 1.6)
      
      legend("topleft", legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("#555555", "#2b7bba", "#b85324"),
             pch = c(16, 17, 17), lty = c(1, 2, NA), bty = "n")
    })
    
    output$error_measures_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      N_len <- length(d)
      
      eval_indices <- 2:N_len
      e <- F_vals[eval_indices] - d[eval_indices]
      
      mad_val  <- mean(abs(e))
      mse_val  <- mean(e^2)
      mape_val <- mean(abs(e / d[eval_indices])) * 100
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(c(mad_val, mse_val, mape_val), 2),
        "Periods evaluated" = rep(paste0("2-", N_len), 3),
        check.names = FALSE
      )
    })
    
    output$period_table <- renderTable({
      req(sonuc())
      d <- sonuc()$d
      F_vals <- sonuc()$F_vals
      e <- F_vals - d
      
      data.frame(
        Period = 1:length(d),
        "Demand (D)" = sprintf("%.2f", d),
        "Forecast (F)" = ifelse(is.na(F_vals), "", sprintf("%.2f", F_vals)),
        "Error (e = F - D)" = ifelse(is.na(e), "", sprintf("%.2f", e)),
        check.names = FALSE
      )
    })
  })
}