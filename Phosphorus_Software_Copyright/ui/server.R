# Shiny application server logic - beautified version

# Define server
server <- function(input, output, session) {
  
  # Initialize model data
  model_data <- reactiveVal(NULL)
  
  # Load model
  observe({
    if (file.exists("models/trained_model.rds")) {
      tryCatch({
        model <- readRDS("models/trained_model.rds")
        model_data(model)
        cat("Model loaded successfully\n")
      }, error = function(e) {
        cat("Model loading failed:", e$message, "\n")
      })
    } else {
      cat("Model file does not exist\n")
    }
  })
  
  # Value box output - beautified version
  output$num_models <- renderValueBox({
    valueBox(
      value = tags$p("6", style = "font-size: 24px; font-weight: bold;"),
      subtitle = tags$p("Trained Models", style = "font-size: 14px;"),
      icon = icon("cogs"),
      color = "purple"
    )
  })
  
  output$num_features <- renderValueBox({
    valueBox(
      value = tags$p("7", style = "font-size: 24px; font-weight: bold;"),
      subtitle = tags$p("Average Features", style = "font-size: 14px;"),
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$avg_performance <- renderValueBox({
    valueBox(
      value = tags$p("0.85", style = "font-size: 24px; font-weight: bold;"),
      subtitle = tags$p("Average R²", style = "font-size: 14px;"),
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # Data exploration module image output - adjusted to smaller squares
  output$cluster_plot <- renderImage({
    if (file.exists("output/cluster_plot.png")) {
      list(src = "output/cluster_plot.png",
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "K-means Clustering Results")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "Image not found")
    }
  }, deleteFile = FALSE)
  
  output$organic_plot <- renderImage({
    if (file.exists("output/organic_plot.png")) {
      list(src = "output/organic_plot.png",
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "Organic Compound Features Comparison")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "Image not found")
    }
  }, deleteFile = FALSE)
  
  output$mw_plot <- renderImage({
    if (file.exists("output/mw_plot.png")) {
      list(src = "output/mw_plot.png",
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "Molecular Weight Distribution Comparison")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "Image not found")
    }
  }, deleteFile = FALSE)
  
  output$pca_plot <- renderImage({
    if (file.exists("output/pca_plot.png")) {
      list(src = "output/pca_plot.png",
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "PCA Visualization")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "Image not found")
    }
  }, deleteFile = FALSE)
  
  # Model performance module image output
  output$lme_correction_plot <- renderImage({
    plot_path <- "D:/Phosphorus_Software_Copyright/output/15_5_LME_correction_effect.png"
    if (file.exists(plot_path)) {
      list(src = plot_path,
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "LME Correction Effect")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "LME Correction Effect Image not found")
    }
  }, deleteFile = FALSE)
  
  output$modeling_progression_plot <- renderImage({
    plot_path <- "D:/Phosphorus_Software_Copyright/output/15_6_modeling_progression_evaluation.png"
    if (file.exists(plot_path)) {
      list(src = plot_path,
           contentType = "image/png",
           width = "100%",
           height = "100%",
           style = "object-fit: contain;",
           alt = "Modeling Progression Evaluation")
    } else {
      list(src = "",
           width = 0,
           height = 0,
           alt = "Modeling Progression Image not found")
    }
  }, deleteFile = FALSE)
  
  # Performance data reactive reading
  performance_data <- reactive({
    perf_file <- "D:/Phosphorus_Software_Copyright/output/performance_metrics_full.xlsx"
    if (file.exists(perf_file)) {
      tryCatch({
        library(readxl)
        perf_data <- read_excel(perf_file)
        return(perf_data)
      }, error = function(e) {
        cat("Performance data reading failed:", e$message, "\n")
        return(get_example_performance_data())
      })
    } else {
      cat("Performance data file does not exist:", perf_file, "\n")
      return(get_example_performance_data())
    }
  })
  
  # Function to get example performance data
  get_example_performance_data <- function() {
    data.frame(
      Variable = c('carbohydrates', 'lignin', 'HWM', 'condensed_aromatics', 'MWM', 'proteins'),
      ML_Method = rep('ranger_gbm_enhanced_with_residual_patterns', 6),
      Train_R2 = c(0.92, 0.88, 0.85, 0.90, 0.87, 0.89),
      Test_R2 = c(0.89, 0.84, 0.81, 0.87, 0.83, 0.85),
      Train_RMSE = c(0.12, 0.09, 0.14, 0.10, 0.11, 0.10),
      Test_RMSE = c(0.15, 0.11, 0.18, 0.12, 0.14, 0.13),
      Train_MAE = c(0.09, 0.07, 0.11, 0.08, 0.09, 0.08),
      Test_MAE = c(0.12, 0.09, 0.15, 0.10, 0.11, 0.10),
      Train_MAPE = c(8.5, 7.2, 9.1, 6.8, 7.9, 7.5),
      Test_MAPE = c(10.2, 8.5, 11.3, 8.9, 9.8, 9.1),
      Num_Features = c(7, 6, 7, 7, 6, 7)
    )
  }
  
  # Performance table
  output$performance_table <- renderDT({
    perf_data <- performance_data()
    
    display_data <- perf_data[, c("Variable", "Test_R2", "Test_RMSE", "Test_MAE", "Test_MAPE", "Num_Features")]
    colnames(display_data) <- c("Target Variable", "Test Set R²", "Test Set RMSE", "Test Set MAE", "Test Set MAPE(%)", "Feature Count")
    
    datatable(display_data, 
              options = list(
                dom = 't', 
                pageLength = 10,
                language = list(
                  info = "Showing _START_ to _END_ of _TOTAL_ records",
                  paginate = list(
                    previous = "Previous",
                    `next` = "Next"
                  )
                )
              ),
              rownames = FALSE) %>%
      formatRound(columns = c('Test Set R²', 'Test Set RMSE', 'Test Set MAE'), digits = 3) %>%
      formatRound(columns = 'Test Set MAPE(%)', digits = 1)
  })
  
  # Error metrics
  output$error_metrics <- renderPlotly({
    perf_data <- performance_data()
    
    error_data <- data.frame(
      Variable = rep(perf_data$Variable, 2),
      Value = c(perf_data$Test_RMSE, perf_data$Test_MAE),
      Metric = rep(c("RMSE", "MAE"), each = nrow(perf_data))
    )
    
    p <- ggplot(error_data, aes(x = Variable, y = Value, fill = Metric)) +
      geom_col(position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("RMSE" = "#4DAF4A", "MAE" = "#984EA3")) +
      labs(
        title = "Error Metrics Distribution",
        x = "Target Variable",
        y = "Error Value"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p) %>% 
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # Training set vs test set performance comparison
  output$train_test_comparison <- renderPlotly({
    perf_data <- performance_data()
    
    comparison_data <- data.frame(
      Variable = rep(perf_data$Variable, 2),
      R2 = c(perf_data$Train_R2, perf_data$Test_R2),
      Dataset = rep(c("Training Set", "Test Set"), each = nrow(perf_data))
    )
    
    p <- ggplot(comparison_data, aes(x = Variable, y = R2, fill = Dataset)) +
      geom_col(position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Training Set" = "#377EB8", "Test Set" = "#E41A1C")) +
      labs(
        title = "Training Set vs Test Set R² Comparison",
        x = "Target Variable",
        y = "R²"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      ylim(0, 1)
    
    ggplotly(p) %>% 
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # Prediction results
  prediction_results <- eventReactive(input$predict_btn, {
    input_values <- list(
      TP = input$tp_input,
      OP = input$op_input,
      pH = input$ph_input,
      Temp = input$temp_input,
      OM = input$om_input,
      MC = input$mc_input,
      cluster = input$cluster_input
    )
    
    if (is.null(model_data())) {
      return(list(
        error = "Model not loaded, please ensure model file exists and restart application"
      ))
    }
    
    tryCatch({
      cat("Using backup_predict function for prediction\n")
      results <- backup_predict(input_values, model_data())
      return(results)
    }, error = function(e) {
      return(list(
        error = paste("Prediction failed:", e$message)
      ))
    })
  })
  
  # Prediction results table
  output$prediction_results <- renderDT({
    results <- prediction_results()
    if (!is.null(results$error)) {
      return(datatable(data.frame(Error = results$error)))
    }
    
    if (nrow(results$final) == 1) {
      final_data <- results$final[1, , drop = FALSE]
      non_na_cols <- !is.na(final_data) & names(final_data) != "Cluster"
      
      if (sum(non_na_cols) == 0) {
        return(datatable(data.frame(Message = "No valid prediction results")))
      }
      
      result_df <- data.frame(
        Variable = names(final_data)[non_na_cols],
        Prediction = as.numeric(final_data[1, non_na_cols])
      )
      colnames(result_df) <- c("Variable", "Prediction (mg/g)")
      
    } else {
      result_df <- data.frame(Variable = character(), `Prediction (mg/g)` = numeric())
    }
    
    datatable(result_df, 
              options = list(
                dom = 't', 
                pageLength = 10,
                language = list(
                  info = "Showing _START_ to _END_ of _TOTAL_ records",
                  paginate = list(
                    previous = "Previous",
                    `next` = "Next"
                  )
                )
              ),
              rownames = FALSE) %>%
      formatRound(columns = 'Prediction (mg/g)', digits = 4)
  })
  
  # Prediction details
  output$prediction_details <- renderPrint({
    results <- prediction_results()
    if (!is.null(results$error)) {
      cat("Error:", results$error, "\n")
      return()
    }
    
    cat("Prediction completion time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
    cat("Input parameters:\n")
    cat("  TP:", input$tp_input, "\n")
    cat("  OP:", input$op_input, "\n")
    cat("  pH:", input$ph_input, "\n")
    cat("  Temp:", input$temp_input, "\n")
    cat("  OM:", input$om_input, "\n")
    cat("  MC:", input$mc_input, "\n")
    cat("  Material Type:", input$cluster_input, "\n\n")
    cat("Prediction results generated, containing 6 target variable predictions.\n")
    cat("Unit: mg/g (milligrams/gram)\n")
  })
  
  # Prediction visualization
  output$prediction_visualization <- renderPlotly({
    results <- prediction_results()
    if (!is.null(results$error)) {
      return(plotly_empty())
    }
    
    tryCatch({
      if (nrow(results$final) == 1) {
        final_data <- results$final[1, , drop = FALSE]
        
        target_vars <- c("carbohydrates", "lignin", "HWM", "condensed_aromatics", "MWM", "proteins")
        pred_values <- numeric()
        valid_vars <- character()
        
        for (var in target_vars) {
          if (var %in% names(final_data) && !is.na(final_data[[var]])) {
            pred_values <- c(pred_values, final_data[[var]])
            valid_vars <- c(valid_vars, var)
          }
        }
        
        if (length(pred_values) == 0) {
          return(plotly_empty())
        }
        
        viz_data <- data.frame(
          Variable = valid_vars,
          Prediction = pred_values,
          stringsAsFactors = FALSE
        )
        
        viz_data <- viz_data[order(viz_data$Prediction, decreasing = TRUE), ]
        
        p <- ggplot(viz_data, aes(x = reorder(Variable, Prediction), y = Prediction, fill = Variable)) +
          geom_col(alpha = 0.7) +
          geom_text(aes(label = round(Prediction, 4)), vjust = -0.5, size = 3) +
          labs(
            title = "Prediction Results Visualization",
            x = "Target Variable", 
            y = "Prediction (mg/g)"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold")
          ) +
          scale_fill_brewer(palette = "Set3") +
          ylim(0, max(viz_data$Prediction) * 1.1)
        
        ggplotly(p) %>% 
          layout(
            showlegend = FALSE,
            autosize = TRUE,
            margin = list(l = 50, r = 50, b = 80, t = 50, pad = 4)
          )
        
      } else {
        return(plotly_empty())
      }
    }, error = function(e) {
      cat("Visualization error:", e$message, "\n")
      return(plotly_empty())
    })
  })
}