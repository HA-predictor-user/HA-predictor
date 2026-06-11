# Helper utility functions

# Outlier detection function
detect_outliers <- function(x, method = "iqr", threshold = 1.5) {
  if (method == "iqr") {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qnt[2] - qnt[1]
    lower <- qnt[1] - threshold * iqr
    upper <- qnt[2] + threshold * iqr
    return(x < lower | x > upper)
  } else if (method == "zscore") {
    z_scores <- scale(x)
    return(abs(z_scores) > 3)
  }
}

# Winsorize processing function
winsorize <- function(x, method = "iqr", threshold = 1.5) {
  if (method == "iqr") {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qnt[2] - qnt[1]
    lower <- qnt[1] - threshold * iqr
    upper <- qnt[2] + threshold * iqr
    x[x < lower] <- lower
    x[x > upper] <- upper
  } else if (method == "zscore") {
    z <- scale(x)
    x[abs(z) > 3] <- mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE) * sign(z[abs(z) > 3])
  }
  return(x)
}

# Performance evaluation function
enhanced_performance_metrics <- function(actual, predicted) {
  r2 <- cor(actual, predicted)^2
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted)/actual)) * 100
  return(list(R2 = r2, RMSE = rmse, MAE = mae, MAPE = mape))
}

# Optimization configuration function
optimization_measures <- function() {
  feature_selection <- list(
    "enable" = TRUE,
    "top_n_total" = 7,
    "method" = "permutation",
    "include_interactions" = FALSE,  # Consistent with original model
    "include_squared" = TRUE         # Consistent with original model
  )
  ml_method <- "ranger_gbm"  # Consistent with original model
  return(list(feature_selection = feature_selection, ml_method = ml_method))
}

# Feature selection function
select_top_features <- function(data, target, predictors, top_n_total = 7, 
                                include_interactions = FALSE, include_squared = TRUE) {
  all_features <- predictors
  
  if (include_interactions) {
    interactions <- combn(predictors, 2, simplify = FALSE)
    for (pair in interactions) {
      int_name <- paste(pair, collapse = "_X_")
      data[[int_name]] <- data[[pair[1]]] * data[[pair[2]]]
      all_features <- c(all_features, int_name)
    }
  }
  
  if (include_squared) {
    for (pred in predictors) {
      sq_name <- paste0(pred, "_sq")
      data[[sq_name]] <- data[[pred]]^2
      all_features <- c(all_features, sq_name)
    }
  }
  
  formula <- as.formula(paste(target, "~", paste(all_features, collapse = " + ")))
  base_model <- ranger(
    formula = formula,
    data = data,
    num.trees = 100,
    importance = "permutation",
    seed = 123
  )
  
  imp <- importance(base_model)
  imp_df <- data.frame(
    feature = names(imp),
    importance = as.numeric(imp),
    type = ifelse(grepl("_X_", names(imp)), "interaction", 
                  ifelse(grepl("_sq", names(imp)), "squared", "main"))
  )
  imp_df <- imp_df[order(-imp_df$importance), ]
  selected_features <- imp_df$feature[1:min(top_n_total, nrow(imp_df))]
  
  cat("Original feature count:", length(predictors), 
      ifelse(include_interactions, paste0("Interaction term count:", length(combn(predictors, 2, simplify = FALSE))), ""),
      ifelse(include_squared, paste0("Squared term count:", length(predictors)), ""),
      "Filtered feature count:", length(selected_features), "\n")
  cat("Top", length(selected_features), "most important features:", paste(selected_features, collapse = ", "), "\n")
  
  return(selected_features)
}

# Yeo-Johnson inverse transformation function - exactly the same as original model
inverse_yeojohnson <- function(y, lambda) {
  if (is.na(y)) return(NA)
  
  if (abs(lambda) < 1e-8) {
    return(exp(y) - 1)
  } else if (y >= 0) {
    base_val <- y * lambda + 1
    if (base_val <= 0) {
      return(y * (1 + 0.5 * lambda * y))
    }
    return(base_val^(1/lambda) - 1)
  } else {
    base_val <- -y * (2 - lambda) + 1
    if (base_val <= 0) {
      return(y * (1 + 0.5 * lambda * y))
    }
    return(1 - base_val^(1/(2 - lambda)))
  }
}

# Check file existence
check_file_exists <- function(file_path) {
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(FALSE)
  }
  return(TRUE)
}

# Backup prediction function (for server.R)
backup_predict <- function(input_values, model_data) {
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
  train_stats <- NULL
  lambda_values <- NULL
  target_norm_params <- NULL
  
  # Perform prediction
  results <- predict_new_data_standalone(
    new_data, model_data$models, opt_config, predictor_vars, target_list,
    train_stats, lambda_values, target_norm_params
  )
  
  return(results)
}