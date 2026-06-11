# Standalone model test script
cat("=== Testing standalone model prediction function ===\n")

# Load necessary functions
source("config/config.R")
source("config/constants.R") 
source("utils/helpers.R")
source("modules/model_saving.R")
source("modules/prediction.R")

# Test standalone predictor
test_standalone_prediction <- function() {
  cat("1. Creating standalone predictor...\n")
  predictor <- create_standalone_predictor("models/trained_model.rds")
  
  cat("2. Getting model information...\n")
  print(predictor)
  
  cat("3. Creating test data...\n")
  test_data <- data.frame(
    TP = 12.5,
    OP = 7.2,
    pH = 7.8,
    Temp = 42.1,
    OM = 58.3,
    MC = 53.2,
    cluster = 1
  )
  
  cat("4. Making prediction...\n")
  results <- predictor$predict(test_data)
  
  cat("5. Showing prediction results:\n")
  print(results$final)
  
  return(results)
}

# Run test
if (file.exists("models/trained_model.rds")) {
  test_results <- test_standalone_prediction()
  cat("✓ Standalone prediction test completed\n")
} else {
  cat("⚠ Model file does not exist, please run main.R to train model first\n")
}