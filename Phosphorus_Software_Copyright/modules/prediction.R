# Prediction module - fixed version

# Standalone prediction function
predict_new_data_standalone <- function(new_data, model_list, opt_config, predictor_vars, target_list, 
                                        train_stats = NULL, lambda_values = NULL, target_norm_params = NULL) {
  cat("Performing standalone prediction...\n")
  
  # Use exactly the same standardization parameters as original model
  STANDARDIZATION_PARAMS <- data.frame(
    variable = c("TP", "OP", "pH", "Temp", "OM", "MC"),
    mean = c(10.44361, 6.7111, 7.7233, 40.731, 57.2535, 51.7758),
    sd = c(7.2095, 4.3846, 0.7235, 12.9241, 9.3372, 13.4723)
  )
  
  # Use exactly the same Yeo-Johnson parameters as original model
  LAMBDA_VALUES <- data.frame(
    variable = c("condensed_aromatics", "lipids", "proteins", "carbohydrates", "lignin", "HWM", "MWM", "LWM"),
    lambda = c(-4.673755099, -1.948520643, -1.086931304, -0.174533003, -0.532851837, -0.569464719, 0.082737414, -0.099631573)
  )
  
  # Use exactly the same target variable normalization parameters as original model
  TARGET_NORM_PARAMS <- data.frame(
    variable = c("condensed_aromatics", "lipids", "proteins", "carbohydrates", "lignin", "tannins", "HWM", "MWM", "LWM"),
    mean = c(0.07142683, 0.19559078, 0.337579902, 0.762754283, 0.355062055, 0.047193189, 0.621115303, 1.556900767, 0.84438554),
    sd = c(0.066589385, 0.137491733, 0.230393731, 0.466386527, 0.265895418, 0.073029609, 0.297619968, 0.760826397, 0.533946513)
  )
  
  # Force using same configuration as original model
  opt_config <- list(
    feature_selection = list(
      enable = TRUE,
      top_n_total = 7,
      method = "permutation",
      include_interactions = FALSE,
      include_squared = TRUE
    ),
    ml_method = "ranger_gbm"
  )
  
  # Ensure new data contains necessary columns
  required_cols <- c(predictor_vars, "cluster")
  missing_cols <- setdiff(required_cols, names(new_data))
  if (length(missing_cols) > 0) {
    stop("Missing necessary columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure cluster is factor type
  new_data$cluster <- as.factor(new_data$cluster)
  
  # Data standardization: use exactly the same parameters as original model
  cat("=== Input data standardization processing (using original model parameters) ===\n")
  new_data_standardized <- new_data
  
  for (var in predictor_vars) {
    if (var %in% STANDARDIZATION_PARAMS$variable) {
      stats <- STANDARDIZATION_PARAMS[STANDARDIZATION_PARAMS$variable == var, ]
      original_vals <- new_data_standardized[[var]]
      new_data_standardized[[var]] <- (original_vals - stats$mean) / stats$sd
      cat(sprintf("Variable %s: original value %.4f → standardized value %.4f\n", 
                  var, original_vals, new_data_standardized[[var]]))
    }
  }
  
  # Get configuration (using forced configuration)
  include_interactions <- opt_config$feature_selection$include_interactions
  include_squared <- opt_config$feature_selection$include_squared
  ml_method <- opt_config$ml_method
  
  # Data preprocessing - generate interaction terms and squared terms (on standardized data)
  new_data_processed <- new_data_standardized
  
  if (include_interactions) {
    interactions <- combn(predictor_vars, 2, simplify = FALSE)
    for (pair in interactions) {
      int_name <- paste(pair, collapse = "_X_")
      new_data_processed[[int_name]] <- new_data_processed[[pair[1]]] * new_data_processed[[pair[2]]]
      cat("Generated interaction term:", int_name, "\n")
    }
  }
  
  if (include_squared) {
    for (pred in predictor_vars) {
      sq_name <- paste0(pred, "_sq")
      new_data_processed[[sq_name]] <- new_data_processed[[pred]]^2
      cat("Generated squared term:", sq_name, "\n")
    }
  }
  
  # Store prediction results - fix: pre-create all columns
  predictions_transformed <- data.frame(
    Cluster = new_data_processed$cluster,
    carbohydrates = NA_real_,
    lignin = NA_real_,
    HWM = NA_real_,
    condensed_aromatics = NA_real_,
    MWM = NA_real_,
    proteins = NA_real_,
    stringsAsFactors = FALSE
  )
  
  predictions_final <- data.frame(
    Cluster = new_data_processed$cluster,
    carbohydrates = NA_real_,
    lignin = NA_real_,
    HWM = NA_real_,
    condensed_aromatics = NA_real_,
    MWM = NA_real_,
    proteins = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Predict for each sample
  for (i in 1:nrow(new_data_processed)) {
    current_cluster <- as.character(new_data_processed$cluster[i])
    cat("\n=== Predicting sample", i, "(Cluster", current_cluster, ") ===\n")
    
    # Find target variables corresponding to this cluster
    target_vars_for_cluster <- c()
    for (target_name in names(target_list)) {
      if (as.numeric(current_cluster) %in% target_list[[target_name]]) {
        target_vars_for_cluster <- c(target_vars_for_cluster, target_name)
      }
    }
    
    cat("Target variables to predict for this cluster:", paste(target_vars_for_cluster, collapse = ", "), "\n")
    
    # Predict for each target variable that needs prediction
    for (target_name in target_vars_for_cluster) {
      if (!target_name %in% names(model_list)) {
        cat("  Warning: Model for", target_name, "not found\n")
        next
      }
      
      model_info <- model_list[[target_name]]
      current_predictors <- model_info$selected_features
      
      # Prepare single sample prediction data
      single_pred_data <- new_data_processed[i, c(current_predictors, "cluster"), drop = FALSE]
      
      # Get cluster-related information
      cluster_centers <- model_info$cluster_centers
      cluster_avg_target <- model_info$cluster_avg_target
      residual_patterns <- model_info$residual_patterns
      cluster_ranges <- model_info$cluster_ranges
      
      # Add cluster center features to prediction data
      single_pred_with_centers <- single_pred_data %>%
        left_join(cluster_centers, by = "cluster", suffix = c("", "_center")) %>%
        left_join(cluster_avg_target, by = "cluster") %>%
        mutate(
          cluster_feature = as.numeric(as.character(cluster))
        )
      
      # Calculate feature differences
      for (pred in current_predictors) {
        diff_col <- paste0(pred, "_diff")
        center_col <- paste0(pred, "_center")
        if (center_col %in% names(single_pred_with_centers)) {
          single_pred_with_centers[[diff_col]] <- single_pred_with_centers[[pred]] - single_pred_with_centers[[center_col]]
        }
      }
      
      # Add residual pattern features
      single_pred_with_centers <- single_pred_with_centers %>%
        left_join(residual_patterns, by = "cluster")
      
      # Build enhanced predictor variables
      diff_predictors <- paste0(current_predictors, "_diff")
      enhanced_predictors <- c(diff_predictors, "cluster_feature", "residual_trend", "residual_variability")
      
      # Check missing variables and set to 0
      missing_vars <- setdiff(enhanced_predictors, names(single_pred_with_centers))
      for (var in missing_vars) {
        single_pred_with_centers[[var]] <- 0
        cat("  Warning: Missing variable", var, ", set to 0\n")
      }
      
      # Three-stage prediction - exactly the same as original model
      if (ml_method == "ranger" || ml_method == "ranger_gbm") {
        # Baseline prediction
        base_pred_data <- cluster_centers[match(single_pred_with_centers$cluster, cluster_centers$cluster), ]
        single_pred_with_centers$base_pred <- predict(
          model_info$base_model, 
          data = base_pred_data
        )$predictions
        
        # Enhanced deviation prediction
        single_pred_with_centers$enhanced_deviation_pred <- predict(
          model_info$enhanced_deviation_model, 
          data = single_pred_with_centers
        )$predictions
        
      } else if (ml_method == "gbm") {
        # Baseline prediction
        base_pred_data <- cluster_centers[match(single_pred_with_centers$cluster, cluster_centers$cluster), ]
        single_pred_with_centers$base_pred <- predict(
          model_info$base_model, 
          newdata = base_pred_data,
          n.trees = 100
        )
        
        # Enhanced deviation prediction
        single_pred_with_centers$enhanced_deviation_pred <- predict(
          model_info$enhanced_deviation_model, 
          newdata = single_pred_with_centers,
          n.trees = 100
        )
      }
      
      # Stage 2 prediction: baseline + enhanced deviation
      single_pred_with_centers$stage2_pred <- single_pred_with_centers$base_pred + single_pred_with_centers$enhanced_deviation_pred
      
      # Stage 3 prediction: add random effects
      single_pred_with_centers$random_effect <- predict(
        model_info$lme_model, 
        newdata = single_pred_with_centers, 
        re.form = NULL
      )
      
      final_pred_transformed <- single_pred_with_centers$stage2_pred + single_pred_with_centers$random_effect
      
      # Get actual range of target variable in this cluster
      current_range <- cluster_ranges[cluster_ranges$cluster == current_cluster, ]
      
      if (nrow(current_range) > 0) {
        min_val <- current_range$min_value
        max_val <- current_range$max_value
        mean_val <- current_range$mean_value
        
        cat("  ", target_name, "actual range in cluster", current_cluster, ": [", 
            round(min_val, 4), ", ", round(max_val, 4), "], mean: ", round(mean_val, 4), "\n")
        cat("  Transformed predicted value:", round(final_pred_transformed, 4), "\n")
        
        # Fine-tuning based on random effects - exactly the same as original model
        random_effect_val <- single_pred_with_centers$random_effect[1]
        
        # If predicted value is out of range, make reasonable adjustment (same logic as original model)
        if (!is.na(final_pred_transformed) && final_pred_transformed < min_val) {
          adjustment <- min(abs(random_effect_val), (mean_val - min_val) * 0.5)
          final_pred_adjusted <- min_val + adjustment
          cat("    * Predicted value below minimum, adjusted to:", round(final_pred_adjusted, 4), 
              "(adjustment:", round(adjustment, 4), ")\n")
          final_pred_transformed <- final_pred_adjusted
        } else if (!is.na(final_pred_transformed) && final_pred_transformed > max_val) {
          adjustment <- min(abs(random_effect_val), (max_val - mean_val) * 0.5)
          final_pred_adjusted <- max_val - adjustment
          cat("    * Predicted value above maximum, adjusted to:", round(final_pred_adjusted, 4), 
              "(adjustment:", round(adjustment, 4), ")\n")
          final_pred_transformed <- final_pred_adjusted
        }
        
        # Ensure predicted value is not too extreme (same logic as original model)
        if (!is.na(final_pred_transformed) && final_pred_transformed < min_val * 0.5) {
          final_pred_transformed <- min_val * 0.8
          cat("    * Predicted value too low, adjusted to 80% of minimum:", round(final_pred_transformed, 4), "\n")
        } else if (!is.na(final_pred_transformed) && final_pred_transformed > max_val * 1.5) {
          final_pred_transformed <- max_val * 1.2
          cat("    * Predicted value too high, adjusted to 120% of maximum:", round(final_pred_transformed, 4), "\n")
        }
      }
      
      # Yeo-Johnson inverse transformation - use exactly the same function and parameters as original model
      if (target_name %in% LAMBDA_VALUES$variable) {
        lambda <- LAMBDA_VALUES$lambda[LAMBDA_VALUES$variable == target_name]
        
        # Inverse standardization
        if (target_name %in% TARGET_NORM_PARAMS$variable) {
          norm_param <- TARGET_NORM_PARAMS[TARGET_NORM_PARAMS$variable == target_name, ]
          final_pred_denormalized <- final_pred_transformed * norm_param$sd + norm_param$mean
          cat("    Inverse standardized value:", round(final_pred_denormalized, 4), "\n")
          
          # Yeo-Johnson inverse transformation
          final_pred_original <- inverse_yeojohnson(final_pred_denormalized, lambda)
        } else {
          final_pred_original <- inverse_yeojohnson(final_pred_transformed, lambda)
        }
        
        # Ensure minimum value is at least 0.01 (same as original model)
        if (!is.na(final_pred_original) && final_pred_original < 0.01) {
          final_pred_original <- 0.01
          cat("    * Applied minimum constraint: adjusted to 0.01 mg/g\n")
        } else if (is.na(final_pred_original)) {
          final_pred_original <- 0.01
          cat("    * Warning: Inverse transformation result is NA, using default value 0.01 mg/g\n")
        }
        
        cat("    Inverse transformed real value:", round(final_pred_original, 4), "mg/g\n")
        
      } else {
        final_pred_original <- final_pred_transformed
        
        # Ensure minimum value is at least 0.01 (same as original model)
        if (!is.na(final_pred_original) && final_pred_original < 0.01) {
          final_pred_original <- 0.01
          cat("    * Applied minimum constraint: adjusted to 0.01 mg/g\n")
        } else if (is.na(final_pred_original)) {
          final_pred_original <- 0.01
          cat("    * Warning: Predicted value is NA, using default value 0.01 mg/g\n")
        }
        
        cat("    Real value:", round(final_pred_original, 4), "mg/g (no transformation)\n")
      }
      
      # Store prediction results
      predictions_transformed[i, target_name] <- final_pred_transformed
      predictions_final[i, target_name] <- final_pred_original
    }
  }
  
  # Return data frame containing transformed and inverse transformed results
  result <- list(
    transformed = predictions_transformed,
    final = predictions_final
  )
  
  cat("\n=== Standalone prediction completed ===\n")
  return(result)
}

# Maintain original function compatibility
predict_new_data <- function(new_data, model_list, opt_config, predictor_vars, target_list) {
  # Call standalone version, using same parameters as original model
  return(predict_new_data_standalone(
    new_data, model_list, opt_config, predictor_vars, target_list
  ))
}

# Simplified prediction function for Shiny app
simple_predict <- function(input_values, model_path = "models/trained_model.rds") {
  if (!file.exists(model_path)) {
    stop("Model file does not exist: ", model_path)
  }
  
  # Load model
  model_data <- readRDS(model_path)
  model_list <- model_data$models
  
  # Create input data frame
  new_data <- data.frame(
    TP = input_values$TP,
    OP = input_values$OP,
    pH = input_values$pH,
    Temp = input_values$Temp,
    OM = input_values$OM,
    MC = input_values$MC,
    cluster = as.numeric(input_values$cluster)
  )
  
  # Force using same configuration as original model
  opt_config <- list(
    feature_selection = list(
      enable = TRUE,
      top_n_total = 7,
      method = "permutation",
      include_interactions = FALSE,
      include_squared = TRUE
    ),
    ml_method = "ranger_gbm"
  )
  
  # Use same predictor variables and target variables as original model
  predictor_vars <- c('TP', 'OP', 'pH', 'Temp', 'OM', 'MC')
  target_list <- list(
    "carbohydrates" = c(1, 2, 3),
    "lignin" = c(3),
    "HWM" = c(1),
    "condensed_aromatics" = c(1), 
    "MWM" = c(2, 3),
    "proteins" = c(2)
  )
  
  # Use hard-coded parameters (ensure consistency with original model)
  train_stats <- NULL  # Use hard-coded parameters inside function
  lambda_values <- NULL  # Use hard-coded parameters inside function
  target_norm_params <- NULL  # Use hard-coded parameters inside function
  
  # Perform prediction
  results <- predict_new_data_standalone(
    new_data, model_list, opt_config, predictor_vars, target_list,
    train_stats, lambda_values, target_norm_params
  )
  
  return(results)
}