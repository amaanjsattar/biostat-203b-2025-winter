# app.R in hw4/mimiciv_shiny
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(here)
library(bigrquery)
library(DBI)

here::i_am("hw4/mimiciv_shiny/app.R")

# -----------------------------
# BigQuery Authentication
# -----------------------------
satoken <- here::here("hw4", "biostat-203b-2025-winter-4e58ec6e5579.json")
bq_auth(path = satoken)

# -----------------------------
# Connect to BigQuery
# -----------------------------
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

# -----------------------------
# TAB 1: Numerical and Graphical Summaries
# -----------------------------
icu_cohort <- readRDS(here("hw4", "mimiciv_shiny", "mimic_icu_cohort.rds"))

# Define Variable Choices for Tab 1
demo_vars_cat <- c("race", "language", "insurance", "marital_status", "gender")
demo_vars_num <- c("age_intime")
lab_vars <- c("bicarbonate", "chloride", "creatinine", "glucose", 
              "potassium", "sodium", "hematocrit", "wbc_count")
vital_vars <- c("Heart Rate", "Noninvasive BP Systolic", 
                "Noninvasive BP Diastolic", "Temperature_F", "Respiratory Rate")
var_choices <- c(demo_vars_cat, demo_vars_num, lab_vars, vital_vars)

# UI for Tab 1
tab1_ui <- fluidPage(
  titlePanel('Numerical and Graphical Summaries'),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable",
                  choices = var_choices,
                  selected = demo_vars_num[1])
    ),
    mainPanel(
      h3("Numerical Summary"),
      tableOutput("num_summary"),
      h3("Graphical Summary"),
      plotOutput("graph_summary")
    )
  )
)

# Server Logic for Tab 1
tab1_server <- function(input, output) {
  output$num_summary <- renderTable({
    var <- input$var
    if(var %in% demo_vars_num || var %in% lab_vars || var %in% vital_vars) {
      icu_cohort %>%
        summarise(
          Min    = min(!!sym(var), na.rm = TRUE),
          Q1     = quantile(!!sym(var), 0.25, na.rm = TRUE),
          Median = median(!!sym(var), na.rm = TRUE),
          Mean   = mean(!!sym(var), na.rm = TRUE),
          Q3     = quantile(!!sym(var), 0.75, na.rm = TRUE),
          Max    = max(!!sym(var), na.rm = TRUE)
        )
    } else if(var %in% demo_vars_cat) {
      icu_cohort %>%
        group_by(!!sym(var)) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    }
  })
  
  output$graph_summary <- renderPlot({
    var <- input$var
    if(var %in% demo_vars_num || var %in% lab_vars || var %in% vital_vars) {
      ggplot(icu_cohort, aes_string(x = var)) +
        geom_histogram(fill='midnightblue', color = 'black') +
        labs(title = paste("Histogram of", var),
             x = var, y = "Frequency") +
        theme_minimal()
    } else if(var %in% demo_vars_cat) {
      ggplot(icu_cohort, aes_string(x = var)) +
        geom_bar(fill = "midnightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Bar Plot of", var),
             x = var, y = "Count") +
        theme_minimal()
    }
  })
}

# -----------------------------
# TAB 2: Patient-Specific ADT and ICU Stay Events
# -----------------------------

# UI for Tab 2
tab2_ui <- fluidPage(
  titlePanel("Patient-Specific ADT and ICU Stay Information"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("subject_id", "Select Subject ID",
                     choices = NULL, multiple = FALSE, options = list(placeholder = "Start typing a Subject ID...")
      )
    ),
    mainPanel(
      h3("Patient Demographics"),
      tableOutput("patient_info"),
      hr(),
      h3("Combined Timeline of Events"),
      plotOutput("timeline_plot")
    )
  )
)

# Server Logic for Tab 2
tab2_server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "subject_id", choices = unique(icu_cohort$subject_id), server = TRUE)
  })
  
  # Retrieve Patient Demographics
  patient_info <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT DISTINCT p.subject_id, p.gender, p.anchor_age, a.race
      FROM `biostat-203b-2025-winter.mimiciv_3_1.patients` p
      LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.admissions` a 
      ON p.subject_id = a.subject_id
      WHERE p.subject_id = ", input$subject_id
    )
    dbGetQuery(con_bq, query)
  })
  
  # Retrieve ADT Events
  adt_data <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT subject_id, TIMESTAMP(admittime) AS admittime, TIMESTAMP(dischtime) AS dischtime, 'ADT' AS event_type
      FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
      WHERE subject_id = ", input$subject_id
    )
    dbGetQuery(con_bq, query)
  })
  
  # Combine All Events for Timeline
  timeline_events_viz <- reactive({
    req(adt_data())
    df <- adt_data()
    df <- df %>% mutate(
      admittime = as.POSIXct(admittime, tz = "UTC"),
      dischtime = as.POSIXct(dischtime, tz = "UTC"),
      y_position = 3
    )
    df
  })
  
  output$patient_info <- renderTable({ patient_info() })
  
  output$timeline_plot <- renderPlot({
    req(timeline_events_viz())
    df <- timeline_events_viz()
    
    ggplot(df, aes(x = admittime, y = y_position)) +
      geom_segment(aes(xend = dischtime, yend = y_position), linewidth = 2, color = "blue") +
      geom_point(size = 3, color = "black") +
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
      labs(title = paste("Patient", input$subject_id, "Timeline"),
           x = "Calendar Time",
           y = "Event Type") +
      theme_minimal()
  })
}

# -----------------------------
# Combine UI and Server
# -----------------------------
ui <- navbarPage("MIMIC-IV ICU Data Explorer",
                 tabPanel("Summary", tab1_ui),
                 tabPanel("Patient Info", tab2_ui)
)

server <- function(input, output, session) {
  tab1_server(input, output)
  tab2_server(input, output, session)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
