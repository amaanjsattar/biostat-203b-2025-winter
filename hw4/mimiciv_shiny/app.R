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

# UI for Tab 1
tab1_ui <- fluidPage(
  titlePanel('Numerical and Graphical Summaries'),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable",
                  choices = names(icu_cohort),
                  selected = "age_intime")
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
    if (is.numeric(icu_cohort[[var]])) {
      icu_cohort %>%
        summarise(
          Min    = min(.data[[var]], na.rm = TRUE),
          Q1     = quantile(.data[[var]], 0.25, na.rm = TRUE),
          Median = median(.data[[var]], na.rm = TRUE),
          Mean   = mean(.data[[var]], na.rm = TRUE),
          Q3     = quantile(.data[[var]], 0.75, na.rm = TRUE),
          Max    = max(.data[[var]], na.rm = TRUE)
        )
    } else {
      icu_cohort %>%
        count(.data[[var]]) %>%
        mutate(Percentage = n / sum(n) * 100)
    }
  })
  
  output$graph_summary <- renderPlot({
    var <- input$var
    if (is.numeric(icu_cohort[[var]])) {
      ggplot(icu_cohort, aes(x = .data[[var]])) +
        geom_histogram(fill = 'midnightblue', color = 'black') +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal()
    } else {
      ggplot(icu_cohort, aes(x = .data[[var]])) +
        geom_bar(fill = "midnightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
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

# Tab 2
# Function to get patient demographics [VERIFIED]
get_patient_info <- function(subject_id) {
  query <- paste0("
    SELECT p.subject_id, p.gender, p.anchor_age, a.race
    FROM `biostat-203b-2025-winter.mimiciv_3_1.patients` p
    LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.admissions` a 
    ON p.subject_id = a.subject_id
    WHERE p.subject_id = ", subject_id, "
    LIMIT 1
  ")
  dbGetQuery(con_bq, query)
}

# Function to get admissions [VERIFIED]
get_admissions <- function(subject_id) {
  query <- paste0("
    SELECT subject_id, admittime, dischtime, 'ADT' AS event_type
    FROM `biostat-203b-2025-winter.mimiciv_3_1.admissions`
    WHERE subject_id = ", subject_id
  )
  
  dbGetQuery(con_bq, query) %>%
    mutate(
      admittime = ymd_hms(admittime),
      dischtime = ymd_hms(dischtime)
    ) %>%
    select(subject_id, admittime, dischtime, event_type)
}

# Function to get transfers [VERIFIED]
get_transfers <- function(subject_id) {
  query <- paste0("
    SELECT subject_id, intime AS admittime, outtime AS dischtime, 'ADT' AS event_type, careunit
    FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
    WHERE subject_id = ", subject_id
  )

  dbGetQuery(con_bq, query) %>%
    mutate(
      admittime = ymd_hms(admittime),
      dischtime = ymd_hms(dischtime),
      careunit = ifelse(is.na(careunit), "UNKNOWN", careunit),
      is_icu_ccu = ifelse(str_detect(str_to_lower(careunit), "icu|ccu"), "ICU/CCU", "Other")
    ) %>%
    select(subject_id, admittime, dischtime, event_type, careunit, is_icu_ccu)
}

# Function to merge admissions and transfers [VERIFIED]
get_adt_events <- function(subject_id) {
  admissions_viz <- get_admissions(subject_id)
  transfers_viz <- get_transfers(subject_id)
  
  bind_rows(admissions_viz, transfers_viz) %>%
    arrange(admittime) %>%
    mutate(careunit = ifelse(is.na(careunit), "UNKNOWN", careunit)
    )
}




# Function to get lab events [VERIFIED]
get_labs <- function(subject_id) {
  query <- paste0("
    SELECT subject_id, charttime AS admittime, 'Lab' AS event_type
    FROM `biostat-203b-2025-winter.mimiciv_3_1.labevents`
    WHERE subject_id = ", subject_id
  )
  
  dbGetQuery(con_bq, query) %>%
    mutate(admittime = with_tz(ymd_hms(admittime), "UTC")) %>%
    select(subject_id, admittime, event_type)
}

# Function to get procedure events [VERIFIED]
get_procedures <- function(subject_id) {
  query <- paste0("
    SELECT p.subject_id, 
           p.chartdate AS admittime, 
           'Procedure' AS event_type, 
           COALESCE(dp.long_title, 'Unknown') AS procedure_desc
    FROM `biostat-203b-2025-winter.mimiciv_3_1.procedures_icd` p
    LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.d_icd_procedures` dp 
    ON p.icd_code = dp.icd_code
    WHERE p.subject_id = ", subject_id
  )
  
  dbGetQuery(con_bq, query) %>%
    mutate(
      admittime = ymd(admittime)
    ) %>%
    select(subject_id, admittime, event_type, procedure_desc)
}

# Function to get top 3 diagnoses [VERIFIED]
get_top_diagnoses <- function(subject_id) {
  query <- paste0("
    SELECT DISTINCT dd.long_title, d.seq_num
    FROM `biostat-203b-2025-winter.mimiciv_3_1.diagnoses_icd` d
    LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.d_icd_diagnoses` dd 
    ON d.icd_code = dd.icd_code
    WHERE d.subject_id = ", subject_id, "
    ORDER BY d.seq_num
    LIMIT 3
  ")
  # Execute the query and get the result
  result <- dbGetQuery(con_bq, query)
  
  # Return only the long_title column as a vector
  result %>%
    pull(long_title)  
}

# Get timeline events [VERIFIED]
get_timeline_events <- function(subject_id) {
  adt_events <- get_adt_events(subject_id)
  lab_events <- get_labs(subject_id)
  procedure_events <- get_procedures(subject_id)
  
  bind_rows(adt_events, lab_events, procedure_events) %>%
    arrange(admittime) %>%
    mutate(
      y_position = case_when(
        event_type == "ADT" ~ 3,
        event_type == "Lab" ~ 2,
        event_type == "Procedure" ~ 1,
        TRUE ~ NA_real_
      )
    )
}

# Format title for demographics [VERIFIED]
get_demo_title <- function(subject_id) {
  pid <- get_patient_info(subject_id)
  paste0("Patient ID: ", pid$subject_id,   ', ',
         pid$gender, ', ',
         pid$anchor_age, ' years old, ',
         pid$race)
}

# Format subtitle with diagnoses
format_subtitle <- function(subject_id) {
  paste(
    get_top_diagnoses, 
    collapse = '\n') 
}









# Server Logic for Tab 2
tab2_server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "subject_id", choices = unique(icu_cohort$subject_id), server = TRUE)
  })
  
  # Retrieve Patient Info [VERIFIED]
  # Gives you a table with subject_id, gender, anchor_age, race
  patient_info <- reactive({
    req(input$subject_id)
    query <- paste0("
      SELECT p.subject_id, p.gender, p.anchor_age, a.race
      FROM `biostat-203b-2025-winter.mimiciv_3_1.patients` p
      LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.admissions` a 
      ON p.subject_id = a.subject_id
      WHERE p.subject_id = ", input$subject_id, "
      LIMIT 1
    ")
    dbGetQuery(con_bq, query)
  })
  
  # Run Queries and Store as Data Frames
  transfers_reactive <- function(subject_id) {
    query <- paste0("
    SELECT subject_id, intime AS admittime, outtime AS dischtime, 'ADT' AS event_type, careunit
    FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
    WHERE subject_id = ", subject_id
    )
    
    # Store query result in a data frame
    transfers_viz <- dbGetQuery(con_bq, query) %>%
      mutate(
        admittime = ymd_hms(admittime),
        dischtime = ymd_hms(dischtime),
        careunit = ifelse(is.na(careunit), "UNKNOWN", careunit),
        is_icu_ccu = ifelse(str_detect(str_to_lower(careunit), "icu|ccu"), "ICU/CCU", "Other")
      ) %>%
      select(subject_id, admittime, dischtime, event_type, careunit, is_icu_ccu)
    
    return(transfers_viz)  # Returns df with the desired name
  }
}
  
  
  # Render Patient Info Table
  output$patient_info <- renderTable({
    patient_info()
  })
  
}  

                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
#     )
#       SELECT subject_id, admittime, dischtime, NULL AS careunit
#       FROM `biostat-203b-2025-winter.mimiciv_3_1.admissions`
#       WHERE subject_id = ", input$subject_id, "
#     ")
#     dbGetQuery(con_bq, query)
#   })
#   # Retrieve ADT, Lab, and Procedure Events
#   # Query components: transfers, admissions, 
#   timeline_events_viz <- reactive({
#     req(input$subject_id)
#     
#     query <- paste0("
#       SELECT subject_id, intime AS admittime, outtime AS dischtime, 'ADT' AS event_type, careunit
#       FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
#       WHERE subject_id = ", input$subject_id, "
#       UNION ALL
#       SELECT subject_id, admittime, dischtime, 'ADT' AS event_type
#       FROM 'biostat-203b-2025-winter.mimiciv_3_1.admissions'
#       WHERE subject_id = ", input$subject_id, "
#       UNION ALL
#       SELECT subject_id, charttime AS admittime, NULL AS dischtime, 'Lab' AS event_type, NULL AS careunit
#       FROM `biostat-203b-2025-winter.mimiciv_3_1.labevents`
#       WHERE subject_id = ", input$subject_id, "
#       UNION ALL
#       SELECT p.subject_id, p.chartdate AS admittime, NULL AS dischtime, 'Procedure' AS event_type, COALESCE(dp.long_title, 'Unknown') AS procedure_desc
#       FROM `biostat-203b-2025-winter.mimiciv_3_1.procedures_icd` p
#       LEFT JOIN `biostat-203b-2025-winter.mimiciv_3_1.d_icd_procedures` dp 
#       ON p.icd_code = dp.icd_code
#       WHERE p.subject_id = ", input$subject_id, "
#       ORDER BY admittime
#     ")
#     
#     df <- dbGetQuery(con_bq, query)
#     
#     df <- df %>%
#       mutate(
#         y_position = case_when(
#           event_type == "ADT" ~ 3,
#           event_type == "Lab" ~ 2,
#           event_type == "Procedure" ~ 1,
#           TRUE ~ NA_real_
#         )
#       )
#     
#     return(df)
#   })
#   
#   # Render Timeline Plot
#   output$timeline_plot <- renderPlot({
#     req(timeline_events_viz())
#     df <- timeline_events_viz()
#     
#     ggplot(df) +
#       geom_segment(data = df %>% filter(event_type == "ADT"),
#                    aes(x = admittime, xend = dischtime, y = y_position, yend = y_position, color = careunit),
#                    linewidth = 2) +
#       geom_point(data = df %>% filter(event_type == "Procedure"),
#                  aes(x = admittime, y = y_position, shape = procedure_desc),
#                  size = 3, color = "black") +
#       geom_point(data = df %>% filter(event_type == "Lab"),
#                  aes(x = admittime, y = y_position),
#                  shape = 3, size = 3, color = "black") +
#       scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
#       theme_minimal()
#   })
# }

# -----------------------------
# Run the App
# -----------------------------
ui <- navbarPage("MIMIC-IV ICU Data Explorer",
                 tabPanel("Summary", tab1_ui),
                 tabPanel("Patient Info", tab2_ui))

server <- function(input, output, session) {
  tab1_server(input, output)
  tab2_server(input, output, session)
}

shinyApp(ui = ui, server = server)
