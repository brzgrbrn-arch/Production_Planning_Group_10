library(shiny)
library(readxl)

# UI
moving_average_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        
        radioButtons(
          ns("n_choice_type"), 
          "How should N be chosen?",
          choices = c("I will choose N" = "manual", "Find the best N" = "auto"),
          selected = "manual"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("n_choice_type")),
          numericInput(ns("manual_n"), "N (number of past periods)", value = 3, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("n_choice_type")),
          selectInput(ns("kriter"), "Optimization Criterion:", choices = c("MAD", "MSE", "MAPE"), selected = "MAD"),
          numericInput(ns("max_search_n"), "Max N to search:", value = 8, min = 2, max = 20)
        )
      ),
      
      mainPanel(
        h3(textOutput(ns("forecast_title"))),
        plotOutput(ns("forecast_plot")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("n_choice_type")),
          hr(),
          h4("Optimization Results"),
          plotOutput(ns("best_n_plot")),
          tableOutput(ns("n_tablo"))
        ),
        
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
moving_average_server <- function(id) {
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
    
    arama <- reactive({
      req(demand_vector())
      d <- demand_vector()
      N_len <- length(d)
      max_limit <- min(input$max_search_n, N_len - 2)
      
      res_list <- list()
      for (cur_n in 1:max_limit) {
        F_vals <- rep(NA, N_len)
        for (t in (cur_n + 1):N_len) {
          F_vals[t] <- mean(d[(t - cur_n):(t - 1)])
        }
        val_idx <- (cur_n + 1):N_len
        e <- F_vals[val_idx] - d[val_idx]
        
        mad_val  <- mean(abs(e))
        mse_val  <- mean(e^2)
        mape_val <- mean(abs(e / d[val_idx])) * 100
        
        res_list[[length(res_list) + 1]] <- data.frame(
          N = cur_n, 
          MAD = mad_val, 
          MSE = mse_val, 
          MAPE = mape_val
        )
      }
      do.call(rbind, res_list)
    })
    
    secilen_N <- reactive({
      req(arama(), input$kriter)
      tab <- arama()
      k <- input$kriter
      tab$N[which.min(tab[[k]])]
    })
    
    effective_n <- reactive({
      if (input$n_choice_type == "manual") {
        req(input$manual_n)
        return(input$manual_n)
      } else {
        req(secilen_N())
        return(secilen_N())
      }
    })
    
    sonuc <- reactive({
      req(demand_vector(), effective_n())
      d <- demand_vector()
      n_val <- effective_n()
      N_len <- length(d)
      
      F_vals <- rep(NA, N_len)
      if (n_val < N_len) {
        for (t in (n_val + 1):N_len) {
          F_vals[t] <- mean(d[(t - n_val):(t - 1)])
        }
      }
      
      next_f <- mean(tail(d, n_val))
      list(d = d, F_vals = F_vals, next_f = next_f, n = n_val)
    })
    
    output$forecast_title <- renderText({
      req(sonuc())
      s <- sonuc()
      paste0("MA(", s$n, ") - next-period forecast: ", sprintf("%.2f", s$next_f))
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
           main = paste0("Actual demand and MA(", s$n, ") forecast"),
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
      n_val <- s$n
      N_len <- length(d)
      if (n_val >= N_len) return(NULL)
      
      eval_indices <- (n_val + 1):N_len
      e <- F_vals[eval_indices] - d[eval_indices]
      
      mad_val  <- mean(abs(e))
      mse_val  <- mean(e^2)
      mape_val <- mean(abs(e / d[eval_indices])) * 100
      eval_range <- paste0(n_val + 1, "-", N_len)
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(c(mad_val, mse_val, mape_val), 2),
        "Periods evaluated" = rep(eval_range, 3),
        check.names = FALSE
      )
    })
    
    output$period_table <- renderTable({
      req(sonuc())
      s <- sonuc()
      d <- s$d
      F_vals <- s$F_vals
      e <- F_vals - d
      
      data.frame(
        Period = 1:length(d),
        "Demand (D)" = sprintf("%.2f", d),
        "Forecast (F)" = ifelse(is.na(F_vals), "", sprintf("%.2f", F_vals)),
        "Error (e = F - D)" = ifelse(is.na(e), "", sprintf("%.2f", e)),
        check.names = FALSE
      )
    })
    
    output$best_n_plot <- renderPlot({
      req(arama(), secilen_N(), input$kriter)
      tablo <- arama()
      k <- input$kriter
      renk <- ifelse(tablo$N == secilen_N(), "#B4501E", "#9DB4CF")
      barplot(tablo[[k]], names.arg = tablo$N, col = renk, border = NA,
              xlab = "N", ylab = k,
              main = paste0(k, " degerine gore en iyi N = ", secilen_N()))
    })
    
    output$n_tablo <- renderTable({
      req(arama())
      tablo <- arama()
      tablo$MAD  <- round(tablo$MAD, 3)
      tablo$MSE  <- round(tablo$MSE, 3)
      tablo$MAPE <- round(tablo$MAPE, 3)
      tablo$N    <- as.integer(tablo$N)
      tablo
    })
    
  })
}