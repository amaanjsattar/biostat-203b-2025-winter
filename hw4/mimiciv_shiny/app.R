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
# TAB 1: Numerical and Graphical Summaries
# -----------------------------
# Load ICU Cohort Data (preprocessed RDS file)

icu_cohort <- readRDS(here("hw4", "mimiciv_shiny", "mimic_icu_cohort.rds"))

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)
dbListTables(con_bq)

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

# Tab 2: ADT and ICU Stay Events per Patient
# UI for Tab 2
# UI for Tab 2
tab2_ui <- fluidPage(
  titlePanel("Patient-Specific ADT and ICU Stay Information"),
  sidebarLayout(
    sidebarPanel(
      selectInput("subject_id", "Select Subject ID",
                  choices = NULL, selected = NULL) # Populate dynamically
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
  
  # Populate Subject ID Dropdown Dynamically
  observe({
    query <- "SELECT DISTINCT subject_id FROM `biostat-203b-2025-winter.mimiciv_3_1.patients` LIMIT 1000"
    subject_ids <- dbGetQuery(con_bq, query)
    updateSelectInput(session, "subject_id", choices = subject_ids$subject_id)
  })
  
  # Retrieve Patient Demographics
  patient_info <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT p.subject_id, p.gender, p.anchor_age, a.race
      FROM `biostat-203b-2025-winter.mimiciv_3_1.patients` p
      LEFT JOIN (SELECT subject_id, race FROM `biostat-203b-2025-winter.mimiciv_3_1.admissions`) a 
      ON p.subject_id = a.subject_id
      WHERE p.subject_id = ", input$subject_id, "
      LIMIT 1
    ")
    dbGetQuery(con_bq, query)
  })
  
  # Retrieve ADT Events (Admissions & Transfers)
  adt_data <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT subject_id, admittime, dischtime, 'Admission' AS event_type, NULL AS careunit
      FROM `biostat-203b-2025-winter.mimiciv_3_1.admissions`
      WHERE subject_id = ", input$subject_id, "
      UNION ALL
      SELECT subject_id, intime AS admittime, outtime AS dischtime, 'Transfer' AS event_type, careunit
      FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
      WHERE subject_id = ", input$subject_id, "
      ORDER BY admittime
    ")
    df <- dbGetQuery(con_bq, query)
    df <- df %>% mutate(
      careunit = ifelse(is.na(careunit) | careunit == '', 'UNKNOWN', careunit),
      is_icu_ccu = ifelse(str_detect(tolower(careunit), "icu|ccu"), "ICU/CCU", "Other"),
      admittime = as.POSIXct(admittime, tz = "UTC"),
      dischtime = as.POSIXct(dischtime, tz = "UTC")
    )
    df
  })
  
  # Retrieve Lab Events
  lab_data <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT subject_id, charttime AS admittime, 'Lab' AS event_type
      FROM `biostat-203b-2025-winter.mimiciv_3_1.labevents`
      WHERE subject_id = ", input$subject_id, "
      ORDER BY charttime
    ")
    df <- dbGetQuery(con_bq, query)
    df
  })
  
  # Retrieve Procedure Events
  procedure_data <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT p.subject_id, p.chartdate AS admittime, 'Procedure' AS event_type, COALESCE(dp.long_title, 'Unknown') AS procedure_desc
      FROM `biostat-203b-2025-winter.mimiciv_3_1.procedures_icd` p
      LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.d_icd_procedures` dp 
      ON p.icd_code = dp.icd_code
      WHERE p.subject_id = ", input$subject_id, "
      ORDER BY p.chartdate
    ")
    df <- dbGetQuery(con_bq, query)
    df
  })
  
  # Combine All Events for Timeline
  timeline_events_viz <- reactive({
    req(adt_data(), lab_data(), procedure_data())
    df <- bind_rows(
      adt_data(),
      lab_data(),
      procedure_data()
    ) %>%
      arrange(admittime)
    
    df$event_type <- factor(df$event_type, levels = c("Admission", "Transfer", "Lab", "Procedure"), ordered = TRUE)
    
    df <- df %>% mutate(
      y_position = case_when(
        event_type == "Admission" ~ 4,
        event_type == "Transfer" ~ 3,
        event_type == "Lab" ~ 2,
        event_type == "Procedure" ~ 1,
        TRUE ~ NA_real_
      )
    )
    df
  })
  
  # Render Patient Info Table
  output$patient_info <- renderTable({
    patient_info()
  })
  
  # Render Timeline Plot
  output$timeline_plot <- renderPlot({
    req(timeline_events_viz())
    df <- timeline_events_viz()
    ggplot(df, aes(x = admittime, y = y_position, color = event_type)) +
      geom_segment(data = df %>% filter(event_type %in% c("Admission", "Transfer")),
                   aes(xend = dischtime, yend = y_position),
                   linewidth = 2) +
      geom_point(data = df %>% filter(event_type == "Procedure"),
                 aes(shape = procedure_desc),
                 size = 3, color = "black") +
      geom_point(data = df %>% filter(event_type == "Lab"),
                 shape = 3, size = 3, color = "black") +
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
      scale_y_continuous(
        name = NULL,
        breaks = c(1, 2, 3, 4),
        limits = c(0.5, 4.5),
        labels = c("Procedure", "Lab", "Transfer", "Admission")
      ) +
      labs(
        title = paste("Patient", input$subject_id, "Timeline"),
        x = "Calendar Time",
        y = NULL,
        color = "Event Type",
        shape = "Procedure Type"
      ) +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical"
      )
  })
}

    
ui <- navbarPage("MIMIC-IV ICU Data Explorer",
                 tabPanel("Summary", tab1_ui),
                 tabPanel("Patient Info", tab2_ui)
)

server <- function(input, output, session) {
  tab1_server(input, output)
  tab2_server(input, output, session)
}

shinyApp(ui = ui, server = server)


