# System configuration parameters

# File path configuration
PATHS <- list(
  train_data = "data/train(1).xlsx",
  test_data = "data/test(1).xlsx", 
  model_save = "models/trained_model.rds",
  output_dir = "output/"
)

# Model parameter configuration
MODEL_CONFIG <- list(
  seed = 123,
  ml_method = "ranger_gbm",  # "ranger", "ranger_gbm", "gbm"
  feature_selection = list(
    enable = TRUE,
    top_n_total = 7,
    method = "permutation", 
    include_interactions = FALSE,
    include_squared = TRUE
  )
)

# Predictor variable configuration
PREDICTOR_VARS <- c('TP', 'OP', 'pH', 'Temp', 'OM', 'MC')

# Target variable configuration
TARGET_LIST <- list(
  "carbohydrates" = c(1, 2, 3),
  "lignin" = c(3),
  "HWM" = c(1),
  "condensed_aromatics" = c(1), 
  "MWM" = c(2, 3),
  "proteins" = c(2)
)

# Visualization configuration
VISUALIZATION_CONFIG <- list(
  theme = theme_bw(),
  colors = c("#E41A1C", "#377EB8", "#4DAF4A"),
  cluster_labels = c("1" = "Protein-Lignocellulose", 
                     "2" = "Protein", 
                     "3" = "Lignocellulose")
)

# Create necessary directories
create_directories <- function() {
  dirs_to_create <- c("models", "output", "data")
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }
}

# Create directories during initialization
create_directories()