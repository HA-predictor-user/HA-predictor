# Model saving module - enhanced version

save_model_results <- function(model_results, file_path) {
  cat("Saving model to:", file_path, "\n")
  
  # Ensure directory exists
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  # Add training data statistics to model results
  model_results$train_stats <- TRAIN_STATS
  model_results$lambda_values <- LAMBDA_VALUES
  model_results$target_norm_params <- TARGET_NORM_PARAMS
  model_results$predictor_vars <- PREDICTOR_VARS
  model_results$target_list <- TARGET_LIST
  model_results$model_config <- MODEL_CONFIG
  model_results$visualization_config <- VISUALIZATION_CONFIG
  
  # Save complete model results
  saveRDS(model_results, file = file_path)
  
  # Also save a simplified version for Shiny app
  shiny_model <- list(
    models = model_results$models,
    config = list(
      predictor_vars = PREDICTOR_VARS,
      target_list = TARGET_LIST,
      model_config = MODEL_CONFIG
    ),
    constants = list(
      train_stats = TRAIN_STATS,
      lambda_values = LAMBDA_VALUES,
      target_norm_params = TARGET_NORM_PARAMS,
      cluster_colors = CLUSTER_COLORS,
      stage_colors = STAGE_COLORS
    )
  )
  
  shiny_file_path <- "models/shiny_model.rds"
  saveRDS(shiny_model, file = shiny_file_path)
  
  cat("Model saving completed:\n")
  cat("  Complete model:", file_path, "\n")
  cat("  Shiny model:", shiny_file_path, "\n")
  cat("  Included statistical parameters:", nrow(TRAIN_STATS), "variables\n")
  cat("  Included transformation parameters:", nrow(LAMBDA_VALUES), "target variables\n")
  
  return(TRUE)
}

# Enhanced model loading function
load_trained_model <- function(file_path = "models/trained_model.rds") {
  if (!file.exists(file_path)) {
    stop("Model file does not exist: ", file_path)
  }
  
  model_data <- readRDS(file_path)
  
  # Check if necessary parameters are included
  required_components <- c("train_stats", "lambda_values", "target_norm_params")
  missing_components <- setdiff(required_components, names(model_data))
  
  if (length(missing_components) > 0) {
    warning("Model file missing the following components: ", paste(missing_components, collapse = ", "))
    warning("This may affect prediction functionality. Recommend retraining the model.")
  }
  
  cat("Model loaded successfully:", file_path, "\n")
  cat("  Number of models:", length(model_data$models), "\n")
  cat("  Contains statistical parameters:", ifelse("train_stats" %in% names(model_data), "Yes", "No"), "\n")
  
  return(model_data)
}

# Create standalone predictor
create_standalone_predictor <- function(model_path = "models/trained_model.rds") {
  if (!file.exists(model_path)) {
    stop("Model file does not exist: ", model_path)
  }
  
  # Load model data
  model_data <- readRDS(model_path)
  
  # Create predictor object
  predictor <- list(
    model_data = model_data,
    
    # Prediction function
    predict = function(new_data) {
      cat("=== Using standalone predictor for prediction ===\n")
      
      # Get configuration and parameters
      if (!is.null(model_data$model_config)) {
        opt_config <- list(
          feature_selection = model_data$model_config$feature_selection,
          ml_method = model_data$model_config$ml_method
        )
      } else {
        # Fallback configuration
        opt_config <- list(
          feature_selection = list(
            enable = TRUE,
            include_interactions = FALSE,
            include_squared = TRUE
          ),
          ml_method = "ranger_gbm"
        )
      }
      
      # Get predictor variables and target variable list
      predictor_vars <- if (!is.null(model_data$predictor_vars)) {
        model_data$predictor_vars
      } else {
        c('TP', 'OP', 'pH', 'Temp', 'OM', 'MC')
      }
      
      target_list <- if (!is.null(model_data$target_list)) {
        model_data$target_list
      } else {
        list(
          "carbohydrates" = c(1, 2, 3),
          "lignin" = c(3),
          "HWM" = c(1),
          "condensed_aromatics" = c(1), 
          "MWM" = c(2, 3),
          "proteins" = c(2)
        )
      }
      
      # Call prediction function
      results <- predict_new_data_standalone(
        new_data, 
        model_data$models, 
        opt_config, 
        predictor_vars, 
        target_list,
        model_data$train_stats,
        model_data$lambda_values,
        model_data$target_norm_params
      )
      
      return(results)
    },
    
    # Get model information
    get_info = function() {
      info <- list(
        model_count = length(model_data$models),
        target_variables = names(model_data$models),
        has_stats = !is.null(model_data$train_stats),
        has_transforms = !is.null(model_data$lambda_values),
        config_present = !is.null(model_data$model_config)
      )
      return(info)
    }
  )
  
  class(predictor) <- "StandalonePredictor"
  return(predictor)
}

# Print method for standalone predictor
print.StandalonePredictor <- function(x) {
  info <- x$get_info()
  cat("Standalone Phosphorus Humic Acid Predictor\n")
  cat("Model count:", info$model_count, "\n")
  cat("Target variables:", paste(info$target_variables, collapse = ", "), "\n")
  cat("Contains statistical parameters:", ifelse(info$has_stats, "Yes", "No"), "\n")
  cat("Contains transformation parameters:", ifelse(info$has_transforms, "Yes", "No"), "\n")
  cat("Contains configuration information:", ifelse(info$config_present, "Yes", "No"), "\n")
}