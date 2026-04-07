# =========================================================
# TRAFFIC FORECASTER - MISSION CONTROL (R VERSION)
# WITH DATASET SELECTOR
# =========================================================

# --- STEP 1: LOAD LIBRARIES ---
library(shiny)
library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(bslib)

# --- STEP 2: DATA GENERATION (Default dataset if none uploaded) ---
if (!file.exists("traffic_data.csv")) {
  message(">> GENERATING SYNTHETIC DATASET...")
  
  set.seed(42)
  n <- 1200
  
  sample_data <- data.frame(
    hour = sample(0:23, n, replace = TRUE),
    is_weekend = sample(c(0, 1), n, replace = TRUE),
    is_holiday = sample(c(0, 1), n, replace = TRUE),
    temperature_c = runif(n, 10, 38),
    rainfall_mm = runif(n, 0, 15),
    visibility_km = runif(n, 0.5, 12),
    is_raining = sample(c(0, 1), n, replace = TRUE),
    road_incident = sample(c(0, 1), n, replace = TRUE),
    construction_zone = sample(c(0, 1), n, replace = TRUE)
  )
  
  sample_data$vehicle_count <- with(sample_data,
    800 +
    (hour %in% 7:9 | hour %in% 16:19) * 2200 -
    (is_weekend == 1) * 600 -
    (road_incident == 1) * 500 +
    (temperature_c > 30) * 200 +
    rnorm(n, 0, 150)
  )
  
  sample_data$vehicle_count[sample_data$vehicle_count < 0] <- 0
  
  write_csv(sample_data, "traffic_data.csv")
}

# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cyborg",
    primary = "#059669",
    base_font = font_google("JetBrains Mono")
  ),
  
  titlePanel(
    div(
      style="padding:20px 0;",
      "TRAFFIC FORECASTER // NEURAL STREAM-08"
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      style="border:1px solid #059669; background-color:#0a0a0a;",
      
      # Dataset selector
      h5("DATA SOURCE", style="color:#059669;"),
      
      fileInput(
        "dataset_file",
        "Upload CSV Dataset",
        accept = ".csv"
      ),
      
      helpText("Default dataset will be used if no file uploaded."),
      
      hr(),
      
      h5("ENVIRONMENTAL TELEMETRY", style="color:#059669;"),
      
      sliderInput(
        "hour",
        "Time of Day (24h)",
        min = 0,
        max = 23,
        value = 8
      ),
      
      numericInput(
        "temperature_c",
        "Temperature (°C)",
        25
      ),
      
      numericInput(
        "rainfall_mm",
        "Rainfall (mm)",
        0
      ),
      
      numericInput(
        "visibility_km",
        "Visibility (km)",
        10
      ),
      
      hr(),
      
      h5("SITUATIONAL FACTORS", style="color:#059669;"),
      
      radioButtons(
        "is_weekend",
        "Weekend?",
        choices = c("YES" = 1, "NO" = 0),
        selected = 0,
        inline = TRUE
      ),
      
      radioButtons(
        "is_holiday",
        "Holiday?",
        choices = c("YES" = 1, "NO" = 0),
        selected = 0,
        inline = TRUE
      ),
      
      radioButtons(
        "is_raining",
        "Raining?",
        choices = c("YES" = 1, "NO" = 0),
        selected = 0,
        inline = TRUE
      ),
      
      radioButtons(
        "road_incident",
        "Incident?",
        choices = c("YES" = 1, "NO" = 0),
        selected = 0,
        inline = TRUE
      ),
      
      radioButtons(
        "construction_zone",
        "Construction?",
        choices = c("YES" = 1, "NO" = 0),
        selected = 0,
        inline = TRUE
      ),
      
      br(),
      
      actionButton(
        "predict",
        "EXECUTE PREDICTION",
        class="btn-primary w-100"
      )
    ),
    
    
    mainPanel(
      
      div(
        style="
        padding:30px;
        border:1px solid #333;
        background:#050505;
        border-radius:10px;",
        
        verbatimTextOutput("dataset_info"),
        
        br(),
        
        uiOutput("density_status"),
        
        br(),
        
        plotOutput(
          "bar_chart",
          height="300px"
        ),
        
        br(),
        
        h5(
          "SYSTEM LOGS",
          style="color:#059669; font-size:12px;"
        ),
        
        verbatimTextOutput("logs")
        
      )
      
    )
    
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # Load dataset (uploaded or default)
  
  dataset_reactive <- reactive({
    
    if (!is.null(input$dataset_file)) {
      
      data <- read_csv(
        input$dataset_file$datapath
      )
      
    } else {
      
      data <- read_csv(
        "traffic_data.csv"
      )
      
    }
    
    data %>%
      mutate(across(
        c(
          is_weekend,
          is_holiday,
          is_raining,
          road_incident,
          construction_zone
        ),
        as.factor
      ))
    
  })
  
  
  # Train model automatically
  
  model_reactive <- reactive({
    
    data <- dataset_reactive()
    
    set.seed(123)
    
    trainIndex <- createDataPartition(
      data$vehicle_count,
      p = 0.8,
      list = FALSE
    )
    
    randomForest(
      vehicle_count ~ .,
      data = data[trainIndex, ],
      ntree = 100
    )
    
  })
  
  
  # show dataset info
  
  output$dataset_info <- renderText({
    
    if (!is.null(input$dataset_file)) {
      
      paste(
        "CUSTOM DATASET LOADED:",
        input$dataset_file$name
      )
      
    } else {
      
      "DEFAULT SYNTHETIC DATASET LOADED"
      
    }
    
  })
  
  
  # prediction logic
  
  results <- reactiveValues(
    pred = NULL,
    density = NULL,
    color = NULL
  )
  
  
  observeEvent(
    input$predict,
    {
      
      newdata <- data.frame(
        
        hour = as.numeric(input$hour),
        
        is_weekend = factor(
          input$is_weekend,
          levels = c(0,1)
        ),
        
        is_holiday = factor(
          input$is_holiday,
          levels = c(0,1)
        ),
        
        temperature_c =
          as.numeric(input$temperature_c),
        
        rainfall_mm =
          as.numeric(input$rainfall_mm),
        
        visibility_km =
          as.numeric(input$visibility_km),
        
        is_raining = factor(
          input$is_raining,
          levels = c(0,1)
        ),
        
        road_incident = factor(
          input$road_incident,
          levels = c(0,1)
        ),
        
        construction_zone = factor(
          input$construction_zone,
          levels = c(0,1)
        )
        
      )
      
      
      results$pred <- round(
        predict(
          model_reactive(),
          newdata
        )
      )
      
      
      if (results$pred > 2500) {
        
        results$density <- "HIGH DENSITY"
        results$color <- "#dc3545"
        
      }
      
      else if (results$pred > 1000) {
        
        results$density <- "MEDIUM DENSITY"
        results$color <- "#fd7e14"
        
      }
      
      else {
        
        results$density <- "LOW DENSITY"
        results$color <- "#059669"
        
      }
      
      
      # density display
      
      output$density_status <- renderUI({
        
        div(
          
          style = paste0(
            "border:2px solid ",
            results$color,
            ";
            padding:20px;
            text-align:center;"
          ),
          
          h2(
            results$density,
            
            style = paste0(
              "color:",
              results$color,
              ";
              font-weight:900;"
            )
          ),
          
          p(
            paste(
              "ESTIMATED VOLUME:",
              results$pred,
              "VEHICLES / HR"
            )
          )
          
        )
        
      })
      
      
      # bar chart
      
      output$bar_chart <- renderPlot({
        
        ggplot(
          data.frame(
            x = "Traffic Flow",
            y = results$pred
          ),
          
          aes(x, y)
          
        ) +
          
          geom_bar(
            stat = "identity",
            fill = results$color,
            width = 0.5
          ) +
          
          coord_flip() +
          
          ylim(0, 5000) +
          
          theme_minimal() +
          
          theme(
            text =
              element_text(color = "white"),
            
            axis.title =
              element_blank()
          )
        
      },
      
      bg = "transparent"
      
      )
      
      
      # logs
      
      output$logs <- renderText({
        
        paste0(
          
          ">> [",
          Sys.time(),
          "] SIMULATION EXECUTED\n",
          
          ">> RESULT_VOLUME: ",
          results$pred,
          "\n",
          
          ">> MODEL: RANDOM FOREST\n",
          
          ">> STATUS: OK"
          
        )
        
      })
      
    }
    
  )
  
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)