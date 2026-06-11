# Phosphorus Humic Acid Analysis System - Main Program
# Author: [Your Name]
# Date: [Current Date]

# Set working directory and load necessary libraries
setwd("D:/Phosphorus_Software_Copyright")

cat("=== Phosphorus Humic Acid Analysis System ===\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Load configuration
cat("Loading configuration...\n")
source("config/config.R")
source("config/constants.R")

# Load utility functions
cat("Loading utility functions...\n")
source("utils/helpers.R")

# Load all modules
cat("Loading functional modules...\n")
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (module_file in module_files) {
  source(module_file)
  cat("  Loaded:", basename(module_file), "\n")
}

# Main function
main <- function(force_retrain = FALSE, run_shiny = FALSE) {
  cat("\n=== Starting main program execution ===\n")
  
  # Check if Shiny app needs to be run
  if (run_shiny) {
    cat("Starting Shiny application...\n")
    source("ui/ui.R")
    source("ui/server.R")
    shiny::shinyApp(ui = ui, server = server)
    return(invisible())
  }
  
  # Check if model needs to be retrained
  model_file <- "models/trained_model.rds"
  if (!force_retrain && file.exists(model_file)) {
    cat("Found trained model, loading model...\n")
    model_results <- load_trained_model(model_file)
    return(list(
      models = model_results,
      message = "Using trained model"
    ))
  }
  
  cat("Starting new model training...\n")
  
  # 1. Initialize system
  cat("\n[Step 1/8] Initializing system...\n")
  init_result <- initialize_system()
  if (!init_result) {
    stop("System initialization failed")
  }
  
  # 2. Load data
  cat("[Step 2/8] Loading data...\n")
  data_list <- load_data()
  if (is.null(data_list)) {
    stop("Data loading failed")
  }
  
  # 3. Data preprocessing
  cat("[Step 3/8] Data preprocessing...\n")
  processed_data <- preprocess_data(data_list$train_data, data_list$test_data)
  
  # 4. Feature selection
  cat("[Step 4/8] Feature selection...\n")
  selected_features <- perform_feature_selection(processed_data$train_data)
  
  # 5. Model training
  cat("[Step 5/8] Model training...\n")
  model_results <- train_models(processed_data$train_data, selected_features)
  
  # 6. Model evaluation
  cat("[Step 6/8] Model evaluation...\n")
  evaluation_results <- evaluate_models(model_results, processed_data$test_data)
  
  # 7. Generate visualizations
  cat("[Step 7/8] Generating visualizations...\n")
  generate_visualizations(model_results, evaluation_results)
  
  # 8. Save model
  cat("[Step 8/8] Saving model...\n")
  save_success <- save_model_results(model_results, model_file)
  
  if (save_success) {
    cat("\n=== System execution completed ===\n")
    cat("✓ Model saved to:", model_file, "\n")
    cat("✓ Visualization results generated\n")
    cat("✓ System ready\n")
  } else {
    cat("\n=== System execution completed (model saving failed) ===\n")
  }
  
  return(list(
    models = model_results,
    evaluation = evaluation_results,
    data = processed_data
  ))
}

# Command line argument processing
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  if ("--shiny" %in% args) {
    cat("Starting Shiny application mode...\n")
    main(run_shiny = TRUE)
  } else if ("--retrain" %in% args) {
    cat("Forcing model retraining...\n")
    main(force_retrain = TRUE)
  } else if ("--help" %in% args) {
    cat("Usage:\n")
    cat("  Rscript main.R           # Normal mode (use existing model or train new model)\n")
    cat("  Rscript main.R --shiny   # Start Shiny application\n")
    cat("  Rscript main.R --retrain # Force model retraining\n")
    cat("  Rscript main.R --help    # Show this help information\n")
  }
} else {
  # Interactive run
  if (interactive()) {
    cat("\nSelect run mode:\n")
    cat("1. Normal mode (train/load model)\n")
    cat("2. Start Shiny application\n")
    cat("3. Force model retraining\n")
    
    choice <- readline("Please enter choice (1-3, default is 1): ")
    
    if (choice == "2") {
      main(run_shiny = TRUE)
    } else if (choice == "3") {
      main(force_retrain = TRUE)
    } else {
      main()
    }
  } else {
    # Non-interactive mode, default run
    main()
  }
}