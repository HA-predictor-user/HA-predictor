# System initialization module

initialize_system <- function() {
  cat("Initializing system...\n")
  
  # Load necessary libraries
  required_packages <- c(
    "lme4", "lmerTest", "dplyr", "performance", "ggplot2", 
    "caret", "purrr", "tidyr", "ggrepel", "MASS", "ranger", 
    "gbm", "patchwork", "cowplot", "readxl"
  )
  
  # Install missing packages
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Set random seed
  set.seed(MODEL_CONFIG$seed)
  
  cat("System initialization completed\n")
  return(TRUE)
}