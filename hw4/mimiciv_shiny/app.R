# app.R in hw4/mimiciv_shiny
library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(tidyverse)
library(lubridate)
library(here)

# BigQuery Authentication
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)

# TAB 1: Numerical and Graphical Summaries
# Load ICU Cohort Data
icu_cohort <- readRDS(here("hw4",
                           "mimiciv_shiny",
                           "mimic_icu_cohort.rds"))

# Define Variable Choices for Tab 1 (Summary Tab)

# Demographic Variables
demo_vars_cat <- c("race",
                      "language",
                      "insurance",
                      "marital_status",
                      "gender")
demo_vars_num <- c("age_intime")

# Lab Variables
lab_vars <- c("bicarbonate",
              "chloride",
              "creatinine",
              "glucose", 
              "potassium",
              "sodium",
              "hematocrit",
              "wbc_count")
# Vital Variables
vital_vars <- c("Heart Rate",
              "Noninvasive BP Systolic",
              "Noninvasive BP Diastolic",
              "Temperature_F",
              "Respiratory Rate")

# Create an overall list of variables
var_choices <- c(demo_vars_cat, demo_vars_num, lab_vars, vital_vars)

# Define UI for Tab 1
tab1_ui <- fluidPage(
  titlePanel('Numerical and Graphical Summaries'),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable",
                  choices = var_choices,
                  selected = demo_vars_num[1]),
    ),
    mainPanel(
      h3("Numerical Summary"),
      tableOutput("num_summary"),
      h3("Graphical Summary"),
      plotOutput("graph_summary")
    )
  )
)

# Define Server Logic for Tab 1
tab1_server <- function(input,
                        output) {
  # Numerical Summary
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
      # Categorical Variables
      icu_cohort %>%
        group_by(!!sym(var)) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    }
  })
  # Graphical Summary: 
  # Histogram for Numerical Variables
  output$graph_summary <- renderPlot({
    var <- input$var
    if(var %in% demo_vars_num || var %in% lab_vars || var %in% vital_vars) {
      ggplot(icu_cohort, aes_string(x = var)) +
        geom_histogram(fill='midnightblue', color = 'black') +
        labs(title = paste("Histogram of", var),
             x = var,
             y = "Frequency")
    } else if(var %in% demo_vars_cat) {
      # Bar Plot for Categorical Variables
      ggplot(icu_cohort, aes_string(x = var)) +
        geom_bar(fill = "midnightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Bar Plot of", var),
             x = var,
             y = "Count")
    }
  })
}

shinyApp(ui = tab1_ui, server = tab1_server)

# TAB 2: ADT and ICU Visualizations for Each Patient
# Goal: Dynamically retrive patient ADT and ICU Stay Information
# Let users choose a specific patient

