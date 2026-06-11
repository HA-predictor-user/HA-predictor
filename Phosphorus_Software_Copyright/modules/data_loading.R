# Data loading module

load_data <- function() {
  cat("Loading data...\n")
  
  # Check if data files exist
  if (!check_file_exists(PATHS$train_data)) {
    stop("Training data file does not exist: ", PATHS$train_data)
  }
  if (!check_file_exists(PATHS$test_data)) {
    stop("Test data file does not exist: ", PATHS$test_data)
  }
  
  # Load data
  train_data <- readxl::read_excel(PATHS$train_data)
  test_data <- readxl::read_excel(PATHS$test_data)
  
  # Ensure cluster is factor type
  train_data$cluster <- as.factor(train_data$cluster)
  test_data$cluster <- as.factor(test_data$cluster)
  
  cat("Data loading completed:\n")
  cat("  Training set:", nrow(train_data), "rows,", ncol(train_data), "columns\n")
  cat("  Test set:", nrow(test_data), "rows,", ncol(test_data), "columns\n")
  
  return(list(
    train_data = train_data,
    test_data = test_data
  ))
}