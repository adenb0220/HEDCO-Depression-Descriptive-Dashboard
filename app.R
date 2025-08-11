#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot)

# # Specify update dates
# last_search <- "April 2025"
# next_search <- "April 2026"

# Import data
df <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which= "Depression Symptoms")
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level")

# Define UI for application
ui <- fluidPage(
  # HTML Customization
  tags$head(
    tags$style(
      HTML('
        .title-panel {
          text-align: left;
          padding-left: 10px; 
          font-size: 40px;
          color: #007030;
          font-weight: 700;
          font-family: "Open Sans", sans-serif;
        }
        .dt-center.dt-body-center.column-Title {
          width: 700px !important; 
        }
        body {
          font-family: "Source Sans", sans-serif; 
        }
        .studies-panel {
          background-color: #fff;
          padding: 16px;
          border-radius: 8px;
        }
        .studies-count {
          font-size: 60px;
          color: #007030 ;
          display: block;
          margin-top: 8px;
        }
         .tooltiptext {
          visibility: hidden;
          width: 320px;
          background-color: #fff;
          color: #000000;
          text-align: left;
          border-radius: 6px;
          border: 1px solid #000;
          border-color: #000000;
          padding: 8px;
          position: absolute;
          z-index: 1;
          top: 100%;
          left: 0;
          opacity: 0;
          transition: opacity 0.3s;
          font-size: 15px;
        }
        .studies-panel:hover .tooltiptext {
          visibility: visible;
          opacity: 1;
        }
        .table-container {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 5px;
        }
        .table {
          padding: 5px;
        }
        .fluid-row {
        margin-bottom: 30px;
        }
  ')
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans"),
    HTML('<!-- Google tag (gtag.js) -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-8W2N0L5B8P"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-8W2N0L5B8P");
      </script>')
  ),
  ################################################################################################################################################
  ################################################################################################################################################
  ################################################################################################################################################
  ################################################################################################################################################
  
  
  # Application title
  fluidRow(
    column(12,
           div(class = "title-panel", "School-based Interventions to Reduce Depression")
    )
  ),
 ### Row 1
   # Count of studies  
  fluidRow(
    column(2,
           uiOutput("studies_panel")
    ),
    
    # World Map
    column(6,
           div(
             style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
             "Country"
           ),
           plotlyOutput("world_map")
    ),
    
    # School Level Bar Chart
    column(4,
           div(
             style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
             "School Level"
           ),
           plotlyOutput("school_level")
  )
),

### Row 2
fluidRow(
  column(2,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "No. of Schools"
         ),
         plotlyOutput("num_schools_plot", height = "350px")),
  column(2,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "No. of Classsrooms"
         ),
         plotlyOutput("num_class_plot", height = "350px")),
  column(4,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "No. of Students"
         ),
         plotlyOutput("num_students_tile", height = "350px")),
  column(2,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "Average Age"
         ),
         plotlyOutput("avg_age", height = "350px")),
  column(2,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "Female"
         ),
         plotlyOutput("pct_fem", height = "350px")),
),
### Row 3
fluidRow(
  column(12,
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
           "Standardized Mean Difference in Depression Symptoms"
         ),
         div(
           style = "margin-left: 10px; margin-top: 22px; font-size: 12px",
           "A negative SMD indicates an intervention benefit"
         ),
         plotlyOutput("forest_plot")
  )
))
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# Define server logic required 
server <- function(input, output, session) {
  # Import data
  df <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which= "Depression Symptoms")
  
  output$studies_panel <- renderUI({
    n_studies <- length(unique(df$study))
    div(class = "studies-panel",
        h4("No. of Studies"),
        span(class = "studies-count", n_studies),
        span(class = "tooltiptext", HTML(sprintf("Distinct count of Study Author Year: <b>%d</b>", n_studies)))
    )
  })
  
  # Count studies per country
  country_counts <- table(studies$country)
  country_names <- names(country_counts)
  country_iso3 <- countrycode(country_names, origin = "country.name", destination = "iso3c")
  country_name_lookup <- setNames(country_names, country_iso3)   # <-- ADD THIS LINE
  
  all_iso3 <- na.omit(countrycode::codelist$iso3c)
  all_iso3 <- setdiff(all_iso3, "ATA") # Remove Antarctica
  
  country_vals <- setNames(rep(0, length(all_iso3)), all_iso3)
  country_vals[country_iso3] <- as.numeric(country_counts)
  
  label_data <- data.frame(
    iso3c = country_iso3,
    count = as.numeric(country_counts)
  )
  
  green_scale <- list(
    c(0, "#8ABB40"),
    c(0.33, "#489D46"),
    c(0.66, "#007030"),
    c(1,   "#104735")
  )
  
  selected_country <- reactiveVal(NULL)
  
  # Prepare vectors for countries with studies
  studied_iso3 <- names(country_vals)[country_vals > 0]
  studied_counts <- country_vals[studied_iso3]
  
  # For choropleth: only countries with studies > 0
  choropleth_z <- ifelse(country_vals > 0, country_vals, NA)
  choropleth_z[is.na(choropleth_z)] <- NA  # ensures countries with zero studies are not interactive
  
  # Only include countries with studies > 0
  studied_names <- country_names[match(studied_iso3, country_iso3)]
  
  output$world_map <- renderPlotly({
    plot_geo() %>%
      add_trace(
        z = ifelse(country_vals > 0, country_vals, NA),
        locations = names(country_vals),
        type = "choropleth",
        locationmode = "ISO-3",
        colorscale = green_scale,
        marker = list(line = list(color = 'black', width = 2)),
        zmin = 0,
        zmax = max(country_vals, na.rm = TRUE),
        showscale = FALSE,
        text = country_name_lookup[names(country_vals)],
        hoverinfo = "text+z",
        hovertemplate = paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>%{text}</b><br>",
          "No. of Studies: <b>%{z}</b></span><extra></extra>"
        ),
        autocolorscale = FALSE
      ) %>%
      add_trace(
        type = "scattergeo",
        mode = "text",
        locations = studied_iso3,
        locationmode = "ISO-3",
        text = studied_counts,
        hovertext = paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>", studied_names, "</b><br>",
          "No. of Studies: <b>", studied_counts, "</b></span>"
        ),
        hoverinfo = "text",
        textfont = list(size = 22, color = "black"),
        showlegend = FALSE
      ) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(
            family = "Open Sans, sans-serif",
            size = 15,
            color = "black"
          )
        ),
        geo = list(
          scope = "world",
          showland = TRUE,
          landcolor = "white",
          showocean = TRUE,
          oceancolor = "#C5DEDF",
          bgcolor = "rgb(180,205,250)",
          projection = list(type = "equirectangular"),  # <-- Must be a list
          lonaxis = list(range = c(-150, 150)),  # Optional: keep all longitudes
          lataxis = list(range = c(-50, 70))            # <-- Crops Antarctica
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
  
  # Click event will only work for scattergeo labels
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    if (!is.null(event)) {
      # For scattergeo, iso3c is in event$key, for choropleth it's not fired in R
      clicked_country <- event$key
      if (is.null(clicked_country)) {
        # Try location too
        clicked_country <- event$location
      }
      if (!is.null(clicked_country) && clicked_country %in% names(country_vals)) {
        selected_country(clicked_country)
      }
    }
  })
  
  ########################################################
  
  classify_grade_level <- function(x) {
    # Remove spaces for easier matching
    x <- gsub(" ", "", x)
    
    # Early exit for unclear/cannot tell
    if (tolower(x) %in% c("cannot tell", "unclear", "")) return("Unclear")
    
    # Split grades if there are commas
    grades_split <- unlist(strsplit(x, ","))
    
    # Check if all are numeric
    if (any(is.na(suppressWarnings(as.numeric(grades_split))))) return("Unclear")
    
    grades_num <- as.numeric(grades_split)
    min_g <- min(grades_num)
    max_g <- max(grades_num)
    
    # Apply rules
    if (min_g >= 1 && max_g <= 5) return("Elementary")
    if (min_g >= 6 && max_g <= 8) return("Middle")
    if (min_g >= 9 && max_g <= 12) return("High")
    if (min_g <= 5 && max_g >= 6 && max_g <= 8) return("Elementary+Middle")
    if (min_g <= 8 && max_g >= 9 && max_g <= 12) return("Middle+High")
    if (min_g <= 5 && max_g >= 9) return("Unclear") # spans all ranges
    return("Unclear")
  }
  
  grade_categories <- sapply(studies$grade_level, classify_grade_level)
  
  table(studies$school_level)
  table(grade_categories, useNA = "ifany")
  studies$grade_category <- grade_categories
  
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735")
  

  # ## School Level Graph
  output$school_level <- renderPlotly({
    # Prepare data for plot
    school_level_plot_data <- studies %>%
      mutate(grade_category = factor(
        grade_category,
        levels = c("Unclear", "High", "Middle+High", "Middle", "Elementary+Middle", "Elementary")
      )) %>%
      group_by(grade_category) %>%
      summarise(grade_n = n()) %>%
      mutate(
        hover = sprintf(
          "School Level: <b>%s</b><br>Number of Studies: <b>%d</b>",
          grade_category, grade_n
        )
      )
    
    # Make the plot
    school_level_plot <- ggplot(school_level_plot_data, aes(
      x = grade_category,
      y = grade_n,
      fill = grade_n,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, fill = NULL)
    
    # Convert to interactive plotly object with custom hover
    ggplotly(school_level_plot, tooltip = "text") %>%
      layout(
        margin = list(l = 90, r = 5, t = 5, b = 5),
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      ) %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  #########################################################################
  # Number of Schools Graph
  output$num_schools_plot <- renderPlotly({
    studies_clean <- studies %>%
      mutate(number_schools = ifelse(number_schools == -999, NA, number_schools)) %>%
      filter(!is.na(number_schools)) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Schools: <b>", number_schools, "</b>")
      )      
    
    median_schools <- median(studies_clean$number_schools, na.rm = TRUE)
    
    # Dense grid for markers, but exclude x positions near study bubbles
    x_dense <- seq(0.8, 1.4, length.out = 40)
    exclude_window <- 0.05 # <-- adjust for bubble size!
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_schools, length(x_dense_no_bubbles))
    
    num_schools_plot <- ggplot() +
      geom_point(
        data = studies_clean,
        aes(x = 1, y = number_schools, text = hover_text),
        size = 4,
        alpha = 0.3,
        color = "#007030"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        panel.grid = element_blank(),
        panel.border = element_blank()
      ) +
      scale_y_continuous(
        limits = c(-1, NA),
        breaks = c(0, 20, 40),
        labels = c("0", "20", "40"),
        expand = c(0, 2)
      ) +
      labs(y = "Schools", x = "") +
      coord_cartesian(xlim = c(0.8, 1.4))
    
    p <- ggplotly(num_schools_plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black", size = 12)
        ),
        showlegend = FALSE,
        xaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE,
          range = c(-1, max(studies_clean$number_schools, na.rm = TRUE) + 2),
          tickvals = c(0, 20, 40, 60),
          ticktext = c("0", "20", "40", "60")
        ),
        annotations = list(
          list(
            x = 1.3,
            y = median_schools + 100,
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            xref = "x",
            yref = "y",
            hovertext = paste0("Median = ", median_schools, "%"),
            captureevents = TRUE
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add solid line (median)
    p <- p %>%
      plotly::add_trace(
        x = c(0.8, 1.4),
        y = c(median_schools, median_schools),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        text = paste0("Median = ", median_schools),
        hoverinfo = "none", # Only markers will trigger hover
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers along the line for hover, excluding region near bubbles
    p <- p %>%
      plotly::add_trace(
        x = x_dense_no_bubbles,
        y = y_dense_no_bubbles,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "rgba(0,0,0,0)", # invisible
          size = 12                # larger size for bigger hover hitbox
        ),
        text = paste0("Median = ", median_schools),
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      )
    p <- p %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
    p
  })
##########################################################################################

#Number of classrooms graph
output$num_class_plot <- renderPlotly({
  studies_clean <- studies %>%
    mutate(number_classrooms = ifelse(number_classrooms == -999, NA, number_classrooms)) %>%
    filter(!is.na(number_classrooms)) %>%
    mutate(
      hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Classrooms: <b>", number_classrooms, "</b>")
    )
  
  median_class <- median(studies_clean$number_classrooms, na.rm = TRUE)
  
  # Dense grid for markers, but exclude x positions near study bubbles
  x_dense <- seq(0.8, 1.4, length.out = 40)
  exclude_window <- 0.05 # <-- adjust for bubble size!
  x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
  y_dense_no_bubbles <- rep(median_class, length(x_dense_no_bubbles))
  
  num_class_plot <- ggplot() +
    geom_point(
      data = studies_clean,
      aes(x = 1, y = number_classrooms, text = hover_text),
      size = 4,
      alpha = 0.3,
      color = "#007030"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    scale_y_continuous(
      limits = c(-1, NA),
      breaks = c(0, 20, 40),
      labels = c("0", "20", "40"),
      expand = c(0, 2)
    ) +
    labs(y = "Classrooms", x = "") +
    coord_cartesian(xlim = c(0.8, 1.4))
  
  p <- ggplotly(num_class_plot, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black", size = 12)
      ),
      showlegend = FALSE,
      xaxis = list(
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        showgrid = FALSE
      ),
      yaxis = list(
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        showgrid = FALSE,
        range = c(-1, max(studies_clean$number_classrooms, na.rm = TRUE) + 2),
        tickvals = c(0, 20, 40, 60),
        ticktext = c("0", "20", "40", "60")
      ),
      annotations = list(
        list(
          x = 1.3,
          y = median_class + 2,
          text = "Median",
          showarrow = FALSE,
          font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
          align = "center",
          xref = "x",
          yref = "y",
          hovertext = paste0("Median = ", median_class),
          captureevents = TRUE
        )
      )
    ) %>%
    config(displayModeBar = FALSE)
  
  # Add solid line (median)
  p <- p %>%
    plotly::add_trace(
      x = c(0.8, 1.4),
      y = c(median_class, median_class),
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#8ABB40",
        dash = "dot",
        width = 4
      ),
      text = paste0("Median = ", median_class),
      hoverinfo = "none", # Only markers will trigger hover
      showlegend = FALSE,
      inherit = FALSE
    )
  
  # Add invisible markers along the line for hover, excluding region near bubbles
  p <- p %>%
    plotly::add_trace(
      x = x_dense_no_bubbles,
      y = y_dense_no_bubbles,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = "rgba(0,0,0,0)", # invisible
        size = 12                # larger size for bigger hover hitbox
      ),
      text = paste0("Median = ", median_class),
      hoverinfo = "text",
      showlegend = FALSE,
      inherit = FALSE
    )
  p <- p %>%
    layout(dragmode = FALSE) %>%
    config(displayModeBar = FALSE)
  p
})
  ############################################################################
  # Number of students plot
  output$num_students_tile <- renderPlotly({
    studies_clean <- studies %>%
      mutate(number_participants = ifelse(number_participants == -999, NA, number_participants)) %>%
      filter(!is.na(number_participants) & number_participants > 0) %>%
      mutate(
        label = paste0(study_author_year, "\nn=", number_participants),
        hover_text = paste0("Study: <b>", study_author_year, "</b><br> No. of Students:<b> ", number_participants, "</b>")
      )
    
    min_size <- 50
    studies_clean <- studies_clean %>%
      mutate(
        adj_number_participants = ifelse(number_participants < min_size, min_size, number_participants)
      )
    
    plot_ly(
      data = studies_clean,
      type = "treemap",
      labels = ~label,
      values = ~adj_number_participants,
      parents = rep("", nrow(studies_clean)),
      textinfo = "label",
      marker = list(
        line = list(width = 2, color = "white"),
        colors = ~number_participants,
        colorscale = list(
          c(0, "#8ABB40"),    # lightest for few students
          c(1, "#104735")     # darkest for many students
        ),
        reversescale = FALSE
      ),
      hoverinfo = "text",
      text = ~hover_text,
      tilingmethod = "squarify"
    ) %>%
      layout(
        margin = list(t = 0, l = 0, r = 0, b = 0),
        font = list(family = "Open Sans", size = 16),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        pathbar = list(visible = FALSE)
      ) %>%
      config(displayModeBar = FALSE)
  })
##################################################################################
  # Average age graph
  output$avg_age <- renderPlotly({
    studies_clean <- studies %>%
      mutate(average_age = ifelse(average_age == -999, NA, average_age)) %>%
      filter(!is.na(average_age)) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Average Age: <b>", average_age, "</b>")
      )
    
    median_class <- round(median(studies_clean$average_age, na.rm = TRUE), 1)
    
    # Dense grid for markers, but exclude x positions near study bubbles
    x_dense <- seq(0.8, 1.4, length.out = 40)
    exclude_window <- 0.05 # <-- adjust for bubble size!
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_class, length(x_dense_no_bubbles))
    
    num_class_plot <- ggplot() +
      geom_point(
        data = studies_clean,
        aes(x = 1, y = average_age, text = hover_text),
        size = 4,
        alpha = 0.3,
        color = "#007030"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        panel.grid = element_blank(),
        panel.border = element_blank()
      ) +
      scale_y_continuous(
        limits = c(-1, NA),
        breaks = c(0, 20, 40),
        labels = c("0", "20", "40"),
        expand = c(0, 2)
      ) +
      labs(y = "Age, years", x = "") +
      coord_cartesian(xlim = c(0.8, 1.4))
    
    p <- ggplotly(num_class_plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black", size = 12)
        ),
        showlegend = FALSE,
        xaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE,
          range = c(8, max(studies_clean$average_age, na.rm = TRUE) + 1),
        tickvals = c(9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0),
        ticktext = c("9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0", "16.0", "17.0")
        ),
        annotations = list(
          list(
            x = 1.3,
            y = median_class +.5,
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            xref = "x",
            yref = "y",
            hovertext = paste0("Median = ", median_class),
            captureevents = TRUE
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add solid line (median)
    p <- p %>%
      plotly::add_trace(
        x = c(0.8, 1.4),
        y = c(median_class, median_class),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        text = paste0("Median = ", median_class),
        hoverinfo = "none", # Only markers will trigger hover
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers along the line for hover, excluding region near bubbles
    p <- p %>%
      plotly::add_trace(
        x = x_dense_no_bubbles,
        y = y_dense_no_bubbles,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "rgba(0,0,0,0)", # invisible
          size = 12                # larger size for bigger hover hitbox
        ),
        text = paste0("Median = ", median_class),
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      )
    p <- p %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
    p
  })
  ##################################################################################
  # Female graph
  output$pct_fem <- renderPlotly({
    studies_clean <- studies %>%
      mutate(percent_female = ifelse(percent_female == -999, NA, percent_female)) %>%
      filter(!is.na(percent_female)) %>%
      mutate(percent_female = percent_female*100) %>% 
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent Female: <b>", percent_female, "%", "</b>")
      )
    
    median_class <- round(median(studies_clean$percent_female, na.rm = TRUE), 1)
    
    # Dense grid for markers, but exclude x positions near study bubbles
    x_dense <- seq(0.8, 1.4, length.out = 40)
    exclude_window <- 0.05 # <-- adjust for bubble size!
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_class, length(x_dense_no_bubbles))
    
    pct_fem_plot <- ggplot() +
      geom_point(
        data = studies_clean,
        aes(x = 1, y = percent_female, text = hover_text),
        size = 4,
        alpha = 0.3,
        color = "#007030"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        panel.grid = element_blank(),
        panel.border = element_blank()
      ) +
      scale_y_continuous(
          limits = c(0, NA),                # <-- Ensure 0 is visible
          breaks = c(0, 50, 100),
          labels = c("0", "50", "100"),
          expand = c(0, 2)
      ) +
      labs(y = "Percent", x = "") +
      coord_cartesian(xlim = c(0.8, 1.4))
    
    p <- ggplotly(pct_fem_plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black", size = 12)
        ),
        showlegend = FALSE,
        xaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = "black",
          linewidth = 1,
          showgrid = FALSE,
          range = c(0, max(studies_clean$percent_female, na.rm = TRUE) + 1), # <-- Start at 0
          tickvals = c(0, 50, 100),
          ticktext = c("0%", "50%", "100%")
        ),
        annotations = list(
          list(
            x = 1.3,
            y = median_class + 5,
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            xref = "x",
            yref = "y",
            hovertext = paste0("Median = ", median_class),
            captureevents = TRUE
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add solid line (median)
    p <- p %>%
      plotly::add_trace(
        x = c(0.8, 1.4),
        y = c(median_class, median_class),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        text = paste0("Median = ", median_class),
        hoverinfo = "none", # Only markers will trigger hover
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers along the line for hover, excluding region near bubbles
    p <- p %>%
      plotly::add_trace(
        x = x_dense_no_bubbles,
        y = y_dense_no_bubbles,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "rgba(0,0,0,0)", # invisible
          size = 12                # larger size for bigger hover hitbox
        ),
        text = paste0("Median = ", median_class, "%"),
        hoverinfo = "text",
        showlegend = FALSE,
        inherit = FALSE
      )
    p <- p %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
    p
  })
  ##################################################################################
  ### Forest plot
  
  output$forest_plot <- renderPlot({
    se <- sqrt(df$vi)
    df$lower <- df$yi - 1.96 * se
    df$upper <- df$yi + 1.96 * se
    
    # 1. Filter out rows with missing values if needed
    df <- df[!is.na(df$yi) & !is.na(df$study) & !is.na(df$intervention) & !is.na(df$comparison) & !is.na(df$outcome_measure) & !is.na(df$weeks), ]
    
    # 2. Create a "table" label for the y-axis (matching your screenshot)
    df$label <- sprintf(
      "%-18s %-25s %-15s %-14s %-6s %8s",
      df$study,
      df$intervention,
      df$comparison,
      df$outcome_measure,
      format(df$weeks, justify = "right"),
      format(round(df$yi, 3), nsmall = 3, justify = "right")
    )
    
    # 3. Plot: Table as y-axis, bubbles for SMD
    ggplot(df, aes(x = yi, y = reorder(label, yi))) +
      geom_point(aes(size = abs(yi), color = yi < 0), alpha = 0.8) +
      scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "yellow")) +
      scale_size_continuous(range = c(3, 12)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      labs(x = "SMD", y = NULL) +
      theme_minimal(base_size = 15) +
      theme(axis.text.y = element_text(family = "mono", size = 11, hjust = 1))
  })  
}


# Run the application 
shinyApp(ui = ui, server = server)
