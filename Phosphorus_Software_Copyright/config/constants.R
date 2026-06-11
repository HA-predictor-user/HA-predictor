# System constant definitions

# Statistical parameters
TRAIN_STATS <- data.frame(
  variable = c("TP", "OP", "pH", "Temp", "OM", "MC"),
  mean = c(10.44361, 6.7111, 7.7233, 40.731, 57.2535, 51.7758),
  sd = c(7.2095, 4.3846, 0.7235, 12.9241, 9.3372, 13.4723)
)

# Yeo-Johnson transformation parameters  
LAMBDA_VALUES <- data.frame(
  variable = c("condensed_aromatics", "lipids", "proteins", "carbohydrates", "lignin", "HWM", "MWM", "LWM"),
  lambda = c(-4.673755099, -1.948520643, -1.086931304, -0.174533003, -0.532851837, -0.569464719, 0.082737414, -0.099631573)
)

# Target variable normalization parameters
TARGET_NORM_PARAMS <- data.frame(
  variable = c("condensed_aromatics", "lipids", "proteins", "carbohydrates", "lignin", "tannins", "HWM", "MWM", "LWM"),
  mean = c(0.07142683, 0.19559078, 0.337579902, 0.762754283, 0.355062055, 0.047193189, 0.621115303, 1.556900767, 0.84438554),
  sd = c(0.066589385, 0.137491733, 0.230393731, 0.466386527, 0.265895418, 0.073029609, 0.297619968, 0.760826397, 0.533946513)
)

# Cluster color configuration
CLUSTER_COLORS <- c(
  "Protein-Lignocellulose" = "#E41A1C",
  "Protein" = "#377EB8", 
  "Lignocellulose" = "#4DAF4A"
)

# Model stage colors
STAGE_COLORS <- c(
  "Basic Deviation" = "#E41A1C",
  "Enhanced Deviation" = "#377EB8",
  "+Random Effect" = "#4DAF4A"
)