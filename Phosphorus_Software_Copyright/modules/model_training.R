# Model training module

train_models <- function(train_data, selected_features) {
  cat("Starting model training...\n")
  
  model_list <- list()
  performance_metrics <- data.frame()
  cluster_effects <- list()
  cluster_effects_scaled <- list()
  feature_importance_list <- list()
  
  # Get configuration
  opt_config <- optimization_measures()
  ml_method <- opt_config$ml_method
  
  for (target_name in names(TARGET_LIST)) {
    cat("\n=== Training model:", target_name, "===\n")
    
    # Get features for this target variable
    current_predictors <- selected_features[[target_name]]
    
    # Prepare data
    train_df <- train_data[, c(target_name, current_predictors, "cluster")]
    train_df <- na.omit(train_df)
    
    # Calculate cluster center features
    cluster_centers <- train_df %>%
      group_by(cluster) %>%
      summarise(across(all_of(current_predictors), ~ mean(.x, na.rm = TRUE))) %>%
      ungroup()
    
    # Calculate average target variable for each cluster
    cluster_avg_target <- train_df %>%
      group_by(cluster) %>%
      summarise(target_avg = mean(.data[[target_name]], na.rm = TRUE))
    
    # Calculate actual range of target variable in each cluster
    cluster_ranges <- train_df %>%
      group_by(cluster) %>%
      summarise(
        min_value = min(.data[[target_name]], na.rm = TRUE),
        max_value = max(.data[[target_name]], na.rm = TRUE),
        mean_value = mean(.data[[target_name]], na.rm = TRUE),
        sd_value = sd(.data[[target_name]], na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Split training subset and validation subset (80% training, 20% validation)
    set.seed(123)
    train_indices <- createDataPartition(train_df$cluster, p = 0.8, list = FALSE)
    train_subset <- train_df[train_indices, ]
    val_subset <- train_df[-train_indices, ]
    
    cat("Training subset size:", nrow(train_subset), "Validation subset size:", nrow(val_subset), "\n")
    
    # Calculate feature differences and deviation target values for training subset
    train_subset_with_centers <- train_subset %>%
      left_join(cluster_centers, by = "cluster", suffix = c("", "_center")) %>%
      left_join(cluster_avg_target, by = "cluster") %>%
      mutate(
        target_deviation = .data[[target_name]] - target_avg,
        cluster_feature = as.numeric(as.character(cluster))
      )
    
    # Create feature difference columns
    for (pred in current_predictors) {
      diff_col <- paste0(pred, "_diff")
      center_col <- paste0(pred, "_center")
      train_subset_with_centers[[diff_col]] <- train_subset_with_centers[[pred]] - train_subset_with_centers[[center_col]]
    }
    
    # Train baseline model: predict cluster average level
    cluster_model_data <- cluster_centers %>%
      left_join(cluster_avg_target, by = "cluster")
    
    base_formula <- as.formula(paste("target_avg ~", paste(current_predictors, collapse = " + ")))
    
    # Train baseline model
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      base_model <- ranger(
        formula = base_formula,
        data = cluster_model_data,
        num.trees = ifelse(ml_method == "ranger", 200, 500),
        mtry = max(1, floor(length(current_predictors) / 3)),
        importance = "impurity",
        seed = 123
      )
    } else if (ml_method == "gbm") {
      base_model <- gbm(
        formula = base_formula,
        data = cluster_model_data,
        distribution = "gaussian",
        n.trees = 100,
        interaction.depth = 3,
        shrinkage = 0.1,
        cv.folds = 0,
        verbose = FALSE
      )
    }
    
    # Train basic individual adjustment model: predict individual deviation from cluster average
    diff_predictors <- paste0(current_predictors, "_diff")
    enhanced_predictors <- c(diff_predictors, "cluster_feature")
    
    deviation_formula <- as.formula(paste("target_deviation ~", paste(enhanced_predictors, collapse = " + ")))
    
    # Train basic individual adjustment model
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      base_deviation_model <- ranger(
        formula = deviation_formula,
        data = train_subset_with_centers,
        num.trees = ifelse(ml_method == "ranger", 200, 500),
        mtry = max(1, floor(length(enhanced_predictors) / 3)),
        importance = "impurity",
        seed = 123
      )
    } else if (ml_method == "gbm") {
      base_deviation_model <- gbm(
        formula = deviation_formula,
        data = train_subset_with_centers,
        distribution = "gaussian",
        n.trees = 100,
        interaction.depth = 3,
        shrinkage = 0.1,
        cv.folds = 0,
        verbose = FALSE
      )
    }
    
    # Use validation set to calculate residual patterns
    val_subset_with_centers <- val_subset %>%
      left_join(cluster_centers, by = "cluster", suffix = c("", "_center")) %>%
      left_join(cluster_avg_target, by = "cluster") %>%
      mutate(
        cluster_feature = as.numeric(as.character(cluster))
      )
    
    # Calculate feature differences for validation set
    for (pred in current_predictors) {
      diff_col <- paste0(pred, "_diff")
      center_col <- paste0(pred, "_center")
      val_subset_with_centers[[diff_col]] <- val_subset_with_centers[[pred]] - val_subset_with_centers[[center_col]]
    }
    
    # Calculate baseline predictions and basic individual adjustment predictions for validation set
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      val_subset_with_centers$base_pred <- predict(
        base_model, 
        data = cluster_centers[match(val_subset_with_centers$cluster, cluster_centers$cluster), ]
      )$predictions
      
      val_subset_with_centers$base_deviation_pred <- predict(
        base_deviation_model, 
        data = val_subset_with_centers
      )$predictions
      
    } else if (ml_method == "gbm") {
      val_subset_with_centers$base_pred <- predict(
        base_model, 
        newdata = cluster_centers[match(val_subset_with_centers$cluster, cluster_centers$cluster), ],
        n.trees = 100
      )
      
      val_subset_with_centers$base_deviation_pred <- predict(
        base_deviation_model, 
        newdata = val_subset_with_centers,
        n.trees = 100
      )
    }
    
    # Calculate residuals on validation set
    val_subset_with_centers$base_ml_pred <- val_subset_with_centers$base_pred + val_subset_with_centers$base_deviation_pred
    val_subset_with_centers$residual <- val_subset_with_centers[[target_name]] - val_subset_with_centers$base_ml_pred
    
    # Calculate residual patterns (by cluster)
    residual_patterns <- val_subset_with_centers %>%
      group_by(cluster) %>%
      summarise(
        residual_trend = mean(residual, na.rm = TRUE),
        residual_variability = sd(residual, na.rm = TRUE)
      ) %>%
      ungroup()
    
    cat("Residual pattern calculation completed\n")
    
    # Train enhanced individual adjustment model using full training set (including residual pattern features)
    train_df_with_centers <- train_df %>%
      left_join(cluster_centers, by = "cluster", suffix = c("", "_center")) %>%
      left_join(cluster_avg_target, by = "cluster") %>%
      mutate(
        target_deviation = .data[[target_name]] - target_avg,
        cluster_feature = as.numeric(as.character(cluster))
      )
    
    # Create feature difference columns
    for (pred in current_predictors) {
      diff_col <- paste0(pred, "_diff")
      center_col <- paste0(pred, "_center")
      train_df_with_centers[[diff_col]] <- train_df_with_centers[[pred]] - train_df_with_centers[[center_col]]
    }
    
    # Add residual pattern features
    train_df_with_centers <- train_df_with_centers %>%
      left_join(residual_patterns, by = "cluster")
    
    # Create enhanced predictor variable list (including residual pattern features)
    enhanced_predictors_with_residual <- c(enhanced_predictors, "residual_trend", "residual_variability")
    
    deviation_formula_enhanced <- as.formula(paste("target_deviation ~", paste(enhanced_predictors_with_residual, collapse = " + ")))
    
    # Train enhanced individual adjustment model (using full training set)
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      enhanced_deviation_model <- ranger(
        formula = deviation_formula_enhanced,
        data = train_df_with_centers,
        num.trees = ifelse(ml_method == "ranger", 200, 500),
        mtry = max(1, floor(length(enhanced_predictors_with_residual) / 3)),
        importance = "impurity",
        seed = 123
      )
    } else if (ml_method == "gbm") {
      enhanced_deviation_model <- gbm(
        formula = deviation_formula_enhanced,
        data = train_df_with_centers,
        distribution = "gaussian",
        n.trees = 100,
        interaction.depth = 3,
        shrinkage = 0.1,
        cv.folds = 0,
        verbose = FALSE
      )
    }
    
    # Calculate machine learning predictions
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      train_df_with_centers$deviation_pred <- predict(enhanced_deviation_model, data = train_df_with_centers)$predictions
      train_df_with_centers$base_pred <- predict(
        base_model, 
        data = cluster_centers[match(train_df_with_centers$cluster, cluster_centers$cluster), ]
      )$predictions
    } else if (ml_method == "gbm") {
      train_df_with_centers$deviation_pred <- predict(
        enhanced_deviation_model, 
        newdata = train_df_with_centers,
        n.trees = 100
      )
      train_df_with_centers$base_pred <- predict(
        base_model, 
        newdata = cluster_centers[match(train_df_with_centers$cluster, cluster_centers$cluster), ],
        n.trees = 100
      )
    }
    
    train_df_with_centers$ml_pred <- train_df_with_centers$base_pred + train_df_with_centers$deviation_pred
    train_df_with_centers$residual <- train_df_with_centers[[target_name]] - train_df_with_centers$ml_pred
    
    # Use linear mixed model to fit remaining residuals
    lme_formula <- as.formula("residual ~ 1 + (1 | cluster)")
    lme_model <- lmer(lme_formula, data = train_df_with_centers)
    
    # Get random effects
    ranefs <- ranef(lme_model)$cluster
    cluster_effects[[target_name]] <- ranefs
    
    # Store model
    model_list[[target_name]] <- list(
      base_model = base_model,
      base_deviation_model = base_deviation_model,
      enhanced_deviation_model = enhanced_deviation_model,
      lme_model = lme_model,
      cluster_centers = cluster_centers,
      cluster_avg_target = cluster_avg_target,
      residual_patterns = residual_patterns,
      cluster_ranges = cluster_ranges,
      selected_features = current_predictors,
      enhanced_predictors = enhanced_predictors_with_residual
    )
    
    cat("Model", target_name, "training completed\n")
  }
  
  # Mixed effects analysis for each standardized target variable
  target_list_scaled <- setNames(
    lapply(names(TARGET_LIST), function(x) paste0(x, "_scaled")),
    names(TARGET_LIST)
  )
  
  for (target_name in names(target_list_scaled)) {
    cat("\n=== Processing standardized variable:", target_name, "===\n")
    
    # Use same feature set and residual patterns as absolute content
    original_target <- gsub("_scaled$", "", target_name)
    if (original_target %in% names(model_list)) {
      current_predictors <- model_list[[original_target]]$selected_features
      residual_patterns <- model_list[[original_target]]$residual_patterns
    } else {
      # If corresponding absolute content model not found, perform feature selection
      important_vars <- select_top_features(
        train_data, 
        target_name,
        PREDICTOR_VARS,
        top_n_total = opt_config$feature_selection$top_n_total,
        include_interactions = opt_config$feature_selection$include_interactions,
        include_squared = opt_config$feature_selection$include_squared
      )
      current_predictors <- important_vars
      
      # Calculate residual patterns (simplified version)
      residual_patterns <- data.frame(
        cluster = c(1, 2, 3),
        residual_trend = c(0, 0, 0),
        residual_variability = c(1, 1, 1)
      )
    }
    
    # Prepare data
    train_df_scaled <- train_data[, c(target_name, current_predictors, "cluster")]
    train_df_scaled <- na.omit(train_df_scaled)
    
    # Calculate cluster centers and average target values (for standardized variables)
    cluster_centers_scaled <- train_df_scaled %>%
      group_by(cluster) %>%
      summarise(across(all_of(current_predictors), ~ mean(.x, na.rm = TRUE))) %>%
      ungroup()
    
    cluster_avg_target_scaled <- train_df_scaled %>%
      group_by(cluster) %>%
      summarise(target_avg = mean(.data[[target_name]], na.rm = TRUE))
    
    # Calculate feature differences and deviation target values for training set
    train_df_scaled_with_centers <- train_df_scaled %>%
      left_join(cluster_centers_scaled, by = "cluster", suffix = c("", "_center")) %>%
      left_join(cluster_avg_target_scaled, by = "cluster") %>%
      mutate(
        target_deviation = .data[[target_name]] - target_avg,
        cluster_feature = as.numeric(as.character(cluster))
      )
    
    # Create feature difference columns
    for (pred in current_predictors) {
      diff_col <- paste0(pred, "_diff")
      center_col <- paste0(pred, "_center")
      train_df_scaled_with_centers[[diff_col]] <- train_df_scaled_with_centers[[pred]] - train_df_scaled_with_centers[[center_col]]
    }
    
    # Add residual pattern features
    train_df_scaled_with_centers <- train_df_scaled_with_centers %>%
      left_join(residual_patterns, by = "cluster")
    
    # Create predictor variable list including residual pattern features
    diff_predictors_scaled <- paste0(current_predictors, "_diff")
    enhanced_predictors_scaled <- c(diff_predictors_scaled, "cluster_feature", "residual_trend", "residual_variability")
    
    # Train machine learning model
    ml_formula_scaled <- as.formula(paste("target_deviation ~", paste(enhanced_predictors_scaled, collapse = " + ")))
    
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      ml_model_scaled <- ranger(
        formula = ml_formula_scaled,
        data = train_df_scaled_with_centers,
        num.trees = ifelse(ml_method == "ranger", 200, 500),
        mtry = max(1, floor(length(enhanced_predictors_scaled) / 3)),
        importance = "impurity",
        seed = 123
      )
    } else if (ml_method == "gbm") {
      ml_model_scaled <- gbm(
        formula = ml_formula_scaled,
        data = train_df_scaled_with_centers,
        distribution = "gaussian",
        n.trees = 100,
        interaction.depth = 3,
        shrinkage = 0.1,
        cv.folds = 0,
        verbose = FALSE
      )
    }
    
    # Calculate residuals
    if (ml_method == "ranger" || ml_method == "ranger_gbm") {
      train_df_scaled_with_centers$ml_pred <- predict(ml_model_scaled, data = train_df_scaled_with_centers)$predictions
    } else if (ml_method == "gbm") {
      train_df_scaled_with_centers$ml_pred <- predict(ml_model_scaled, train_df_scaled_with_centers, n.trees = 100)
    }
    
    train_df_scaled_with_centers$residual <- train_df_scaled_with_centers[[target_name]] - train_df_scaled_with_centers$ml_pred
    
    # Use linear mixed model to fit residuals
    lme_formula_scaled <- as.formula("residual ~ 1 + (1 | cluster)")
    lme_model_scaled <- lmer(lme_formula_scaled, data = train_df_scaled_with_centers)
    
    # Get random effects (standardized content)
    ranefs_scaled <- ranef(lme_model_scaled)$cluster
    cluster_effects_scaled[[original_target]] <- ranefs_scaled
    
    cat("Standardized variable", target_name, "processing completed\n")
  }
  
  cat("\nAll model training completed\n")
  return(list(
    models = model_list,
    performance = performance_metrics,
    cluster_effects = cluster_effects,
    cluster_effects_scaled = cluster_effects_scaled
  ))
}