# run_app.R - Phosphorus Humic Acid Analysis Shiny Application Launcher

cat("=== Starting Phosphorus Humic Acid Analysis Shiny Application ===\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set working directory to project root
setwd("D:/Phosphorus_Software_Copyright")

# Load necessary libraries
cat("1. Checking and loading necessary packages...\n")
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "plotly", "dplyr", 
                       "readxl", "cluster", "factoextra", "NbClust", "tidyr", 
                       "RColorBrewer", "ggrepel", "patchwork", "ggcorrplot",
                       "lme4", "lmerTest", "performance", "caret", "purrr", 
                       "MASS", "ranger", "gbm", "cowplot")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("   Installing package:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    cat("   ✓", pkg, "\n")
  }
}

# Check and create necessary directories
cat("\n2. Checking directory structure...\n")
required_dirs <- c("models", "output", "data")
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("   Created directory:", dir, "\n")
  } else {
    cat("   ✓", dir, "\n")
  }
}

# Check if clustering images exist, if not run clustering analysis
cat("\n3. Checking clustering charts...\n")
clustering_plots <- c(
  "output/cluster_plot.png",
  "output/organic_plot.png", 
  "output/mw_plot.png",
  "output/pca_plot.png"
)

all_plots_exist <- all(file.exists(clustering_plots))

if (!all_plots_exist) {
  cat("   ⚠ Clustering charts incomplete, running clustering analysis...\n")
  
  # Check clustering data files
  clustering_data_files <- c(
    "../clustering.xlsx",
    "data/clustering.xlsx",
    "clustering.xlsx"
  )
  
  data_file_found <- FALSE
  data_file_path <- NULL
  
  for (file_path in clustering_data_files) {
    if (file.exists(file_path)) {
      data_file_found <- TRUE
      data_file_path <- file_path
      cat("   Found clustering data file:", file_path, "\n")
      break
    }
  }
  
  if (!data_file_found) {
    cat("   ⚠ Warning: Clustering data file not found, will generate sample charts\n")
    # Generate sample charts
    source("generate_sample_clustering_plots.R")
  } else {
    # Run actual clustering analysis
    cat("   Running clustering analysis...\n")
    tryCatch({
      # Execute clustering analysis
      source("run_clustering_analysis.R")
      cat("   ✓ Clustering analysis completed\n")
    }, error = function(e) {
      cat("   ⚠ Clustering analysis failed:", e$message, "\n")
      cat("   Generating sample charts as replacement...\n")
      source("generate_sample_clustering_plots.R")
    })
  }
} else {
  cat("   ✓ Clustering charts exist\n")
}

# Check model files
cat("\n4. Checking model files...\n")
if (file.exists("models/shiny_model.rds")) {
  cat("   ✓ Found Shiny model file: models/shiny_model.rds\n")
} else if (file.exists("models/trained_model.rds")) {
  cat("   ✓ Found complete model file: models/trained_model.rds\n")
  cat("     Tip: Can run main.R to generate dedicated Shiny model file\n")
} else {
  cat("   ⚠ Warning: Model file not found, prediction function will be unavailable\n")
  cat("     Please run main.R to train model first\n")
}

# Check necessary files
cat("\n5. Checking necessary files...\n")
required_files <- c(
  "ui.R",
  "server.R", 
  "config.R",
  "constants.R",
  "helpers.R"
)

# Check if files exist, if not in current directory, check module directories
all_files_exist <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat("   ✓", file, "\n")
  } else if (file.exists(file.path("config", file))) {
    cat("   ✓ config/", file, "\n")
  } else if (file.exists(file.path("utils", file))) {
    cat("   ✓ utils/", file, "\n")
  } else if (file.exists(file.path("ui", file))) {
    cat("   ✓ ui/", file, "\n")
  } else {
    cat("   ✗", file, "does not exist\n")
    all_files_exist = FALSE
  }
}

if (!all_files_exist) {
  cat("\n❌ Error: Missing necessary files, cannot start application\n")
  stop("Please ensure all necessary files exist")
}

# Load configuration and utility functions
cat("\n6. Loading configuration and functions...\n")

# Try loading files from different locations
load_if_exists <- function(file_path) {
  if (file.exists(file_path)) {
    source(file_path, local = FALSE)  # Important: use local = FALSE to ensure functions in global environment
    return(TRUE)
  }
  return(FALSE)
}

# Load configuration
if (!load_if_exists("config.R") && 
    !load_if_exists("config/config.R")) {
  stop("Cannot find config.R file")
}

# Load constants
if (!load_if_exists("constants.R") && 
    !load_if_exists("config/constants.R")) {
  stop("Cannot find constants.R file")
}

# Load utility functions
if (!load_if_exists("helpers.R") && 
    !load_if_exists("utils/helpers.R")) {
  stop("Cannot find helpers.R file")
}

# Explicitly load prediction module - this is the key fix!
cat("7. Explicitly loading prediction module...\n")
if (file.exists("prediction.R")) {
  source("prediction.R", local = FALSE)
  cat("   ✓ Loaded prediction.R\n")
} else if (file.exists("modules/prediction.R")) {
  source("modules/prediction.R", local = FALSE)
  cat("   ✓ Loaded modules/prediction.R\n")
} else {
  cat("   ⚠ Warning: prediction.R file not found\n")
}

# Verify functions exist
cat("8. Verifying necessary functions...\n")
required_functions <- c("backup_predict", "predict_new_data_standalone", "inverse_yeojohnson")
for (func in required_functions) {
  if (exists(func)) {
    cat("   ✓", func, "function loaded\n")
  } else {
    cat("   ✗", func, "function not found\n")
  }
}

cat("   ✓ Configuration loading completed\n")

# Load UI components
cat("\n9. Loading UI components...\n")

# Load UI file
if (file.exists("ui.R")) {
  source("ui.R", local = FALSE)
} else if (file.exists("ui/ui.R")) {
  source("ui/ui.R", local = FALSE)
} else {
  stop("Cannot find ui.R file")
}

# Load server file
if (file.exists("server.R")) {
  source("server.R", local = FALSE)
} else if (file.exists("ui/server.R")) {
  source("ui/server.R", local = FALSE)
} else {
  stop("Cannot find server.R file")
}

cat("   ✓ UI components loading completed\n")

# Start application
cat("\n10. Starting Shiny application...\n")
cat("   -----------------------------------------\n")
cat("   Application Information:\n")
cat("   - Name: Phosphorus Humic Acid Analysis Model\n")
cat("   - Version: 2.0\n")
cat("   - Address: http://127.0.0.1:8314\n")
cat("   -----------------------------------------\n")
cat("   Application will automatically open in browser...\n")
cat("   Press Ctrl+C or close window to stop application\n")
cat("   -----------------------------------------\n\n")

# Run Shiny application
shiny::runApp(
  shinyApp(ui = ui, server = server),
  launch.browser = TRUE,    # Automatically open in browser
  host = "127.0.0.1",       # Local host
  port = 8314,              # Specify port
  quiet = FALSE             # Show startup information
)