# Shiny application UI interface - optimized layout and beautified version

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = span(icon("flask"), "Phosphorus Humic Acid Analysis Model"), 
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Model Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("chart-bar")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Prediction Analysis", tabName = "prediction", icon = icon("calculator")),
      menuItem("About System", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Overall styles */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .main-header .logo {
          background-color: #2c3e50 !important;
          font-weight: bold;
        }
        .main-header .navbar {
          background-color: #34495e !important;
        }
        
        /* Box styles */
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border-top: 3px solid #3498db;
        }
        .box.box-solid {
          border-top: 3px solid #2980b9;
        }
        
        /* Header beautification */
        .box-header {
          background: linear-gradient(135deg, #3498db, #2980b9);
          color: white;
          border-radius: 8px 8px 0 0;
          padding: 12px 15px;
        }
        .box-header .box-title {
          font-weight: bold;
          font-size: 16px;
        }
        
        /* Value box beautification */
        .small-box {
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .small-box .icon {
          font-size: 60px;
          opacity: 0.3;
        }
        
        /* Sidebar beautification */
        .sidebar-menu > li > a {
          border-left: 4px solid transparent;
          font-weight: 500;
        }
        .sidebar-menu > li.active > a {
          border-left-color: #3498db;
          background-color: #ecf0f1;
        }
        
        /* Data exploration image container - smaller squares */
        .small-square-image {
          width: 100%;
          padding-bottom: 80%; /* Slightly compressed */
          position: relative;
          overflow: hidden;
          background: white;
          border-radius: 8px;
        }
        .small-square-image img {
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          object-fit: contain;
          padding: 8px;
        }
        
        /* Performance module image container */
        .square-image {
          width: 100%;
          padding-bottom: 100%;
          position: relative;
          overflow: hidden;
          background: white;
          border-radius: 8px;
        }
        .square-image img {
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          object-fit: contain;
          padding: 10px;
        }
        
        /* Prediction visualization container - 4:3 ratio */
        .prediction-visualization {
          position: relative;
          padding-bottom: 75%;
          height: 0;
          overflow: hidden;
          background: white;
          border-radius: 8px;
        }
        
        /* Table beautification */
        .dataTables_wrapper {
          border-radius: 8px;
          overflow: hidden;
        }
        table.dataTable {
          border-radius: 8px;
        }
        
        /* Button beautification */
        .btn-primary {
          background: linear-gradient(135deg, #3498db, #2980b9);
          border: none;
          border-radius: 6px;
          font-weight: bold;
        }
        .btn-primary:hover {
          background: linear-gradient(135deg, #2980b9, #3498db);
        }
      "))
    ),
    tabItems(
      # Model overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = span(icon("info-circle"), "Model Introduction"), 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  h3("Phosphorus Humic Acid Molecular Prediction Model for Different Types of Compost Products"),
                  p("This system uses advanced mixed-effects machine learning methods, combining random forests, gradient boosting, and linear mixed models to predict phosphorus humic acid component content under different clustering conditions."),
                  br(),
                  h4(icon("star"), "Main Features:"),
                  tags$ul(
                    tags$li(icon("check"), "Multi-stage modeling: Baseline model + Deviation adjustment + Random effects correction"),
                    tags$li(icon("check"), "Supports 6 target variables: carbohydrates, lignin, HWM, condensed_aromatics, MWM, proteins"),
                    tags$li(icon("check"), "Cluster-based hierarchical modeling (3 material types)"),
                    tags$li(icon("check"), "Interaction terms and squared terms feature engineering"),
                    tags$li(icon("check"), "Residual pattern feature enhancement")
                  ),
                  br(),
                  h4(icon("cogs"), "Technical Architecture:"),
                  p("The system adopts modular design, including data preprocessing, model training, performance evaluation, and prediction analysis modules, supporting both command line and web interface usage modes.")
                )
              ),
              fluidRow(
                valueBoxOutput("num_models", width = 4),
                valueBoxOutput("num_features", width = 4), 
                valueBoxOutput("avg_performance", width = 4)
              )
      ),
      
      # Data exploration tab - smaller square images
      tabItem(tabName = "explore",
              fluidRow(
                box(
                  title = span(icon("project-diagram"), "K-means Clustering Results (k=3)"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "small-square-image",
                      imageOutput("cluster_plot", height = "100%")
                  )
                ),
                box(
                  title = span(icon("chart-line"), "PCA Clustering Visualization"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "small-square-image",
                      imageOutput("pca_plot", height = "100%")
                  )
                )
              ),
              fluidRow(
                box(
                  title = span(icon("atom"), "Organic Compound Features Cluster Comparison"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "small-square-image",
                      imageOutput("organic_plot", height = "100%")
                  )
                ),
                box(
                  title = span(icon("weight"), "Molecular Weight Distribution Cluster Comparison"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "small-square-image",
                      imageOutput("mw_plot", height = "100%")
                  )
                )
              )
      ),
      
      # Model performance tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = span(icon("table"), "Model Performance Overview"), 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  DTOutput("performance_table")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("chart-bar"), "Error Metrics Distribution"), 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 6,
                  plotlyOutput("error_metrics", height = "400px")
                ),
                box(
                  title = span(icon("exchange-alt"), "Training Set vs Test Set Performance Comparison"), 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 6,
                  plotlyOutput("train_test_comparison", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("random"), "Linear Mixed Model Correction Effect"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "square-image",
                      imageOutput("lme_correction_plot")
                  )
                ),
                box(
                  title = span(icon("project-diagram"), "Complete Modeling Process Evaluation"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  div(class = "square-image",
                      imageOutput("modeling_progression_plot")
                  )
                )
              )
      ),
      
      # Prediction analysis tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = span(icon("sliders-h"), "Input Parameters"), 
                  status = "danger", 
                  solidHeader = TRUE, 
                  width = 4,
                  numericInput("tp_input", "TP (Total Phosphorus):", value = 10.44, min = 0, max = 50, step = 0.1),
                  numericInput("op_input", "OP (Organic Phosphorus):", value = 6.71, min = 0, max = 30, step = 0.1),
                  numericInput("ph_input", "pH:", value = 7.72, min = 5, max = 10, step = 0.1),
                  numericInput("temp_input", "Temperature (Temp):", value = 40.73, min = 20, max = 80, step = 0.1),
                  numericInput("om_input", "Organic Matter (OM):", value = 57.25, min = 30, max = 80, step = 0.1),
                  numericInput("mc_input", "Moisture Content (MC):", value = 51.78, min = 20, max = 80, step = 0.1),
                  selectInput("cluster_input", "Material Type:", 
                              choices = list(
                                "Protein-Lignocellulose" = 1,
                                "Protein" = 2,
                                "Lignocellulose" = 3
                              ), selected = 1),
                  actionButton("predict_btn", 
                               span(icon("calculator"), "Make Prediction"), 
                               class = "btn-primary", 
                               width = "100%"),
                  br(), br(),
                  helpText(icon("info-circle"), "Note: Prediction results based on trained mixed-effects machine learning model")
                ),
                box(
                  title = span(icon("chart-bar"), "Prediction Results"), 
                  status = "danger", 
                  solidHeader = TRUE, 
                  width = 8,
                  h4(icon("vial"), "Predicted Values (mg/g):"),
                  DTOutput("prediction_results"),
                  br(),
                  h4(icon("info-circle"), "Prediction Details:"),
                  verbatimTextOutput("prediction_details")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("chart-line"), "Prediction Visualization"), 
                  status = "danger", 
                  solidHeader = TRUE, 
                  width = 12,
                  div(class = "prediction-visualization",
                      plotlyOutput("prediction_visualization", 
                                   height = "100%", 
                                   width = "100%")
                  )
                )
              )
      ),
      
      # About system tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = span(icon("info-circle"), "About System"), 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  h3(icon("flask"), "Phosphorus Humic Acid Molecular Content Analysis System"),
                  p(icon("tag"), "Version: 1.0"),
                  p(icon("calendar"), "Development Date: 2025"),
                  br(),
                  h4(icon("cog"), "System Requirements:"),
                  tags$ul(
                    tags$li(icon("r-project"), "R Version: >= 4.0.0"),
                    tags$li(icon("box"), "Required R Packages: shiny, shinydashboard, lme4, ranger, gbm, etc."),
                    tags$li(icon("memory"), "Memory: >= 8GB RAM"),
                    tags$li(icon("hdd"), "Disk Space: >= 1GB")
                  ),
                  br(),
                  h4(icon("book"), "Usage Instructions:"),
                  p(icon("check"), "1. For first use, ensure data files are in data/ directory"),
                  p(icon("check"), "2. System will automatically detect and load trained models"),
                  p(icon("check"), "3. If retraining models is needed, run main program's forced training mode"),
                  p(icon("check"), "4. Prediction function requires complete model file support")
                )
              )
      )
    )
  )
)