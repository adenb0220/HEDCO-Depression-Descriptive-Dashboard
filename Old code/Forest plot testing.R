#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, meta)

# # Specify update dates
# last_search <- "April 2025"
# next_search <- "April 2026"

# Import data
df <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which= "Depression Symptoms")
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level")

# output$forest_plot <- renderPlot({

# Install necessary packages if not already installed
# Install necessary packages if not already installed
# Install if needed
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(plotly)
library(dplyr)

# Prepare your data as before
se <- sqrt(df$vi)
df$lower <- df$yi - 1.96 * se
df$upper <- df$yi + 1.96 * se
df$author <- sub("^(\\S+).*", "\\1", df$study)
df <- df %>% arrange(author)

# Table columns and header
table_data <- df[, c("study", "intervention", "comparison", "outcome_measure", "outcome_timepoint", "yi")]
colnames(table_data) <- c("Study", "Intervention", "Comparison", "Outcome", "Weeks", "SMD")

table_plot <- plot_ly(
  type = 'table',
  header = list(
    values = colnames(table_data),
    align = 'left',
    font = list(size = 12, family = 'Arial', color = 'white'),
    fill = list(color = 'grey'),
    line = list(color = 'white')
  ),
  cells = list(
    values = t(as.matrix(table_data)),
    align = 'left',
    font = list(size = 11, family = 'Arial', color = 'black'),
    fill = list(color = c('white', 'lightgrey')),
    line = list(color = 'grey')
  )
)

# Scatter plot for SMDs, y is row index
scatter_plot <- plot_ly(
  df,
  x = ~yi,
  y = ~seq_along(yi), # row index
  type = "scatter",
  mode = "markers",
  marker = list(
    size = ~abs(yi)*12,
    color = ~yi < 0,
    colorscale = list(c(0, 'yellow'), c(1, 'darkgreen')),
    line = list(width = 1, color = "black")
  ),
  text = ~paste0(
    "<b>Study:</b> ", study, "<br>",
    "<b>Intervention:</b> ", intervention, "<br>",
    "<b>Comparison:</b> ", comparison, "<br>",
    "<b>Outcome:</b> ", outcome_measure, "<br>",
    "<b>Weeks:</b> ", outcome_timepoint, "<br>",
    "<b>SMD:</b> ", round(yi, 3), " [", round(lower, 3), ", ", round(upper, 3), "]"
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Interactive Forest Plot",
    xaxis = list(title = "SMD"),
    yaxis = list(
      title = "",
      tickvals = seq_along(df$study),
      ticktext = df$study,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    margin = list(l = 20)
  )

# Combine side-by-side (table left, plot right)
subplot(
  table_plot, scatter_plot,
  widths = c(0.55, 0.45), # Table wider
  titleX = TRUE,
  titleY = FALSE
) %>%
  layout(height = 30 * nrow(df) + 120)