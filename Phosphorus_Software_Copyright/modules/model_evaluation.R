# Model evaluation module

evaluate_models <- function(model_results, test_data) {
  cat("Evaluating model performance...\n")
  
  performance_metrics <- data.frame()
  ml_method <- optimization_measures()$ml_method
  
  for (target_name in names(model_results$models)) {
    cat("Evaluating model:", target_name, "\n")
    
    model_info <- model_results$models[[target_name]]
    current_predictors <- model_info$selected_features
    
    # Prepare test set data
    test_df <- test_data[, c(target_name, current_predictors, "cluster")]
    test_df <- na.omit(test_df)
    
    # Make predictions for test set
    test_df_with_centers <- test_df %>%
      left_join(model_info$cluster_centers, by = "cluster", suffix = c("", "_center")) %>%
      left_join(model_info$cluster_avg_target, by = "cluster") %>%
      mutate(
        cluster_feature = as.numeric(as.character(cluster))
      )
    
    # Calculate feature differences for test set
    for (pred in current_predictors) {
      diff_col <- paste0(pred, "_diff")
      center_col <- paste0(pred, "_center")
      test_df_with_centers[[diff_col]] <- test_df_with_centers[[pred]] - test_df_with_centers[[center_col]]
    }
    
    # Add residual pattern features
    test_df_with_centers <- test_df_with_centers %>%
      left_join(model_info$residual_patterns, by = "cluster")
    
    # Calculate three-part predictions for test set
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      test_df_with_centers$base_pred <- predict(
        model_info$base_model, 
        data = model_info$cluster_centers[match(test_df_with_centers$cluster, model_info$cluster_centers$cluster), ]
      )$predictions
      
      test_df_with_centers$deviation_pred <- predict(
        model_info$enhanced_deviation_model, 
        data = test_df_with_centers
      )$predictions
      
    } else if (ml_method == "gbm") {
      test_df_with_centers$base_pred <- predict(
        model_info$base_model, 
        newdata = model_info$cluster_centers[match(test_df_with_centers$cluster, model_info$cluster_centers$cluster), ],
        n.trees = 100
      )
      
      test_df_with_centers$deviation_pred <- predict(
        model_info$enhanced_deviation_model, 
        newdata = test_df_with_centers,
        n.trees = 100
      )
    }
    
    test_df_with_centers$ml_pred <- test_df_with_centers$base_pred + test_df_with_centers$deviation_pred
    test_df_with_centers$full_pred <- test_df_with_centers$ml_pred + predict(model_info$lme_model, newdata = test_df_with_centers, re.form = NULL)
    
    # Performance evaluation
    test_metrics <- enhanced_performance_metrics(test_df_with_centers[[target_name]], test_df_with_centers$full_pred)
    
    # Mixed effects model specific metrics
    icc <- tryCatch({
      icc(model_info$lme_model)
    }, error = function(e) {
      list(ICC_adjusted = NA)
    })
    
    r2_nakagawa <- tryCatch({
      r2_nakagawa(model_info$lme_model)
    }, error = function(e) {
      list(R2_marginal = NA, R2_conditional = NA)
    })
    
    # Store performance metrics
    performance_metrics <- rbind(performance_metrics, data.frame(
      Variable = target_name,
      ML_Method = paste0(ml_method, "_enhanced_with_residual_patterns"),
      Test_R2 = test_metrics$R2,
      Test_RMSE = test_metrics$RMSE,
      Test_MAE = test_metrics$MAE,
      Test_MAPE = test_metrics$MAPE,
      ICC = ifelse(is.null(icc$ICC_adjusted), NA, icc$ICC_adjusted),
      R2_marginal = ifelse(is.null(r2_nakagawa$R2_marginal), NA, r2_nakagawa$R2_marginal),
      R2_conditional = ifelse(is.null(r2_nakagawa$R2_conditional), NA, r2_nakagawa$R2_conditional),
      Num_Features = length(current_predictors),
      Num_Main_Features = sum(!grepl("_X_|_sq", current_predictors)),
      Num_Interactions = sum(grepl("_X_", current_predictors)),
      Num_Squared = sum(grepl("_sq", current_predictors))
    ))
    
    cat("  ", target_name, "test set R²:", round(test_metrics$R2, 3), "\n")
  }
  
  cat("\nModel evaluation completed\n")
  print(performance_metrics)
  
  return(performance_metrics)
}