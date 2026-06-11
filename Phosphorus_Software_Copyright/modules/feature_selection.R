# Feature selection module

perform_feature_selection <- function(train_data) {
  cat("Performing feature selection...\n")
  
  if (!MODEL_CONFIG$feature_selection$enable) {
    cat("Feature selection disabled, using all predictor variables\n")
    selected_features <- list()
    for (target_name in names(TARGET_LIST)) {
      selected_features[[target_name]] <- PREDICTOR_VARS
    }
    return(selected_features)
  }
  
  selected_features <- list()
  opt_config <- optimization_measures()
  
  for (target_name in names(TARGET_LIST)) {
    cat("Selecting features for variable", target_name, "...\n")
    
    important_vars <- select_top_features(
      train_data, 
      target_name,
      PREDICTOR_VARS,
      top_n_total = opt_config$feature_selection$top_n_total,
      include_interactions = opt_config$feature_selection$include_interactions,
      include_squared = opt_config$feature_selection$include_squared
    )
    
    selected_features[[target_name]] <- important_vars
    cat("  Selected features for", target_name, ":", length(important_vars), "\n")
  }
  
  cat("Feature selection completed\n")
  return(selected_features)
}