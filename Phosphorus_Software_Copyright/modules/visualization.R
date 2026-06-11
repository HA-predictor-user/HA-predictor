# Visualization module

generate_visualizations <- function(model_results, evaluation_results) {
  cat("Generating visualization results...\n")
  
  # Create output directory
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # Simplified visualization - only generate key charts
  
  # 1. Performance comparison chart
  generate_performance_plot(evaluation_results)
  
  # 2. Feature importance chart
  generate_feature_importance_plots(model_results)
  
  # 3. Residual patterns chart
  generate_residual_patterns_plot(model_results)
  
  cat("Visualization results generated\n")
}

generate_performance_plot <- function(evaluation_results) {
  if (nrow(evaluation_results) == 0) return()
  
  p <- ggplot(evaluation_results, aes(x = Variable, y = Test_R2, fill = Variable)) +
    geom_col(alpha = 0.7) +
    geom_text(aes(label = round(Test_R2, 3)), vjust = -0.3, size = 3) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Model Performance Comparison - Test Set R²",
      x = "Target Variable",
      y = "R²"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("output/performance_comparison.png", p, width = 8, height = 6, dpi = 300)
  cat("Performance comparison chart saved: output/performance_comparison.png\n")
}

generate_feature_importance_plots <- function(model_results) {
  if (length(model_results$models) == 0) return()
  
  for (target_name in names(model_results$models)) {
    model_info <- model_results$models[[target_name]]
    
    # Get feature importance
    if ("importance" %in% names(model_info$enhanced_deviation_model)) {
      importance_data <- data.frame(
        Feature = names(model_info$enhanced_deviation_model$variable.importance),
        Importance = model_info$enhanced_deviation_model$variable.importance
      )
      
      importance_data <- importance_data[order(-importance_data$Importance), ]
      importance_data$Feature <- factor(importance_data$Feature, levels = importance_data$Feature)
      
      p <- ggplot(importance_data[1:10, ], aes(x = Feature, y = Importance, fill = Feature)) +
        geom_col(alpha = 0.7) +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = paste("Feature Importance -", target_name),
          x = "Feature",
          y = "Importance"
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
      filename <- paste0("output/feature_importance_", target_name, ".png")
      ggsave(filename, p, width = 8, height = 6, dpi = 300)
      cat("Feature importance chart saved:", filename, "\n")
    }
  }
}

generate_residual_patterns_plot <- function(model_results) {
  if (length(model_results$models) == 0) return()
  
  residual_data <- data.frame()
  
  for (target_name in names(model_results$models)) {
    model_info <- model_results$models[[target_name]]
    patterns <- model_info$residual_patterns
    patterns$Variable <- target_name
    patterns$Cluster <- factor(patterns$cluster, 
                               levels = c(1, 2, 3),
                               labels = c("Protein-Lignocellulose", "Protein", "Lignocellulose"))
    residual_data <- rbind(residual_data, patterns)
  }
  
  if (nrow(residual_data) > 0) {
    p <- ggplot(residual_data, aes(x = Cluster, y = residual_trend, fill = Cluster)) +
      geom_col(alpha = 0.7) +
      geom_errorbar(aes(ymin = residual_trend - residual_variability, 
                        ymax = residual_trend + residual_variability), 
                    width = 0.2) +
      facet_wrap(~Variable, scales = "free_y") +
      scale_fill_manual(values = CLUSTER_COLORS) +
      labs(
        title = "Residual Pattern Analysis",
        x = "Material Type",
        y = "Residual Trend"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("output/residual_patterns.png", p, width = 10, height = 8, dpi = 300)
    cat("Residual patterns chart saved: output/residual_patterns.png\n")
  }
}