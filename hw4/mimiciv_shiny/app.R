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



