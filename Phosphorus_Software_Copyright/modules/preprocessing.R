# Data preprocessing module

preprocess_data <- function(train_data, test_data) {
  cat("Data preprocessing...\n")
  
  # Use raw data directly
  train_data_processed <- train_data
  test_data_processed <- test_data
  
  # Get configuration
  opt_config <- optimization_measures()
  include_interactions <- opt_config$feature_selection$include_interactions
  include_squared <- opt_config$feature_selection$include_squared
  
  # Generate interaction terms and squared terms
  if (include_interactions) {
    interactions <- combn(PREDICTOR_VARS, 2, simplify = FALSE)
    for (pair in interactions) {
      int_name <- paste(pair, collapse = "_X_")
      train_data_processed[[int_name]] <- train_data_processed[[pair[1]]] * train_data_processed[[pair[2]]]
      test_data_processed[[int_name]] <- test_data_processed[[pair[1]]] * test_data_processed[[pair[2]]]
    }
  }
  
  if (include_squared) {
    for (pred in PREDICTOR_VARS) {
      sq_name <- paste0(pred, "_sq")
      train_data_processed[[sq_name]] <- train_data_processed[[pred]]^2
      test_data_processed[[sq_name]] <- test_data_processed[[pred]]^2
    }
  }
  
  # Get all features
  all_features <- PREDICTOR_VARS
  if (include_interactions) {
    all_features <- c(all_features, sapply(combn(PREDICTOR_VARS, 2, simplify = FALSE), 
                                           function(pair) paste(pair, collapse = "_X_")))
  }
  if (include_squared) {
    all_features <- c(all_features, paste0(PREDICTOR_VARS, "_sq"))
  }
  
  # Detect and handle outliers
  for (var in all_features) {
    outliers <- detect_outliers(train_data_processed[[var]])
    if (sum(outliers, na.rm = TRUE) > 0) {
      cat("Variable", var, "found", sum(outliers, na.rm = TRUE), "outliers, performing Winsorize processing\n")
      train_data_processed[[var]] <- winsorize(train_data_processed[[var]])
      qnt <- quantile(train_data_processed[[var]], probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- qnt[2] - qnt[1]
      lower <- qnt[1] - 1.5 * iqr
      upper <- qnt[2] + 1.5 * iqr
      test_data_processed[[var]] <- pmin(pmax(test_data_processed[[var]], lower), upper)
    }
  }
  
  # Create standardized target variables
  for (target_name in names(TARGET_LIST)) {
    # Calculate mean and standard deviation for each cluster on training set
    cluster_means <- train_data_processed %>%
      group_by(cluster) %>%
      summarise(mean_val = mean(.data[[target_name]], na.rm = TRUE),
                sd_val = sd(.data[[target_name]], na.rm = TRUE))
    
    # Create standardized variables for training set
    train_data_processed <- train_data_processed %>%
      left_join(cluster_means, by = "cluster") %>%
      mutate("{target_name}_scaled" := (.data[[target_name]] - mean_val) / sd_val) %>%
      dplyr::select(-mean_val, -sd_val)
    
    # Create standardized variables for test set (using training set mean and standard deviation)
    test_data_processed <- test_data_processed %>%
      left_join(cluster_means, by = "cluster") %>%
      mutate("{target_name}_scaled" := (.data[[target_name]] - mean_val) / sd_val) %>%
      dplyr::select(-mean_val, -sd_val)
  }
  
  cat("Data preprocessing completed\n")
  return(list(
    train_data = train_data_processed,
    test_data = test_data_processed
  ))
}