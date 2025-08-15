library(shiny)
library(plotly)
library(dplyr)
library(rio)
library(here)

# --- Data prep ---
# Import data
df <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which= "Depression Symptoms")
# studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level") # Not needed for plot

# Prepare your data
se <- sqrt(df$vi)
df$lower <- df$yi - 1.96 * se
df$upper <- df$yi + 1.96 * se
df$author <- sub("^(\\S+).*", "\\1", df$study)
df <- df %>% arrange(author)

# Table columns and header
table_data <- df[, c("study", "intervention", "comparison", "outcome_measure", "outcome_timepoint", "yi")]
colnames(table_data) <- c("Study", "Intervention", "Comparison", "Outcome", "Weeks", "SMD")

# Build table row as HTML to mimic Tableau-like blending
get_table_row <- function(row) {
  sprintf(
    "<span style='font-weight:bold;'>%s</span> <span style='color: #444;'>%s</span> <span style='color: #444;'>%s</span> <span style='color: #444;'>%s</span> <span style='color: #666;'>%s</span> <span style='color: #222;'>%s</span>",
    row[["Study"]], 
    row[["Intervention"]], 
    row[["Comparison"]],
    row[["Outcome"]],
    row[["Weeks"]],
    formatC(row[["SMD"]], digits = 3, format = "f")
  )
}

table_cols <- apply(table_data, 1, get_table_row)

# --- Shiny App ---
ui <- fluidPage(
  h3("Standardized Mean Difference in Depression Symptoms"),
  p("A negative SMD indicates an intervention benefit."),
  plotlyOutput("forest", height = paste0(40 * nrow(df) + 150, "px"))
)

server <- function(input, output, session) {
  output$forest <- renderPlotly({
    plot_ly() %>%
      add_trace(
        x = df$yi,
        y = seq_along(df$yi),
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 16,
          color = ifelse(df$yi < 0, "darkgreen", "yellow"),
          line = list(width = 1, color = "black")
        ),
        error_x = list(
          type = "data",
          array = df$upper - df$yi,
          arrayminus = df$yi - df$lower,
          color = "black"
        ),
        text = paste0(
          "<b>Study:</b> ", df$study, "<br>",
          "<b>Intervention:</b> ", df$intervention, "<br>",
          "<b>Comparison:</b> ", df$comparison, "<br>",
          "<b>Outcome:</b> ", df$outcome_measure, "<br>",
          "<b>Weeks:</b> ", df$outcome_timepoint, "<br>",
          "<b>SMD:</b> ", round(df$yi, 3), " [", round(df$lower, 3), ", ", round(df$upper, 3), "]"
        ),
        hoverinfo = "text",
        name = "SMD"
      ) %>%
      add_trace(
        x = rep(min(df$yi, na.rm=TRUE) - 0.6, nrow(df)), # Place table text to the left
        y = seq_along(df$yi),
        type = "scatter",
        mode = "text",
        text = table_cols,
        textfont = list(family = "Arial", size = 14, color = "black"),
        textposition = "middle right",
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      layout(
        xaxis = list(
          title = "SMD",
          zeroline = TRUE,
          zerolinewidth = 2,
          range = c(min(df$yi, na.rm=TRUE) - 0.8, max(df$yi, na.rm=TRUE) + 0.2),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          tickvals = seq_along(df$study),
          ticktext = rep("", nrow(df)), # hide y-ticks
          showticklabels = FALSE,
          autorange = "reversed"
        ),
        margin = list(l = 340, r = 40, t = 60, b = 40),
        height = 40 * nrow(df) + 150
      )
  })
}

shinyApp(ui, server)