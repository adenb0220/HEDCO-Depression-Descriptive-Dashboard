#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi)

# Import data
df <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which= "Depression Symptoms")
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level")

# Helper function to standardize
standardize <- function(x) {
  x %>%
    tolower() %>%
    stri_trans_general("Latin-ASCII") %>%
    trimws()
}

# Aligning study years
studies$study_author_year[studies$study_author_year == "McLaughlin 2010"] <- "McLaughlin 2011"


# Standardize keys
df <- df %>% mutate(study_std = standardize(study))
studies <- studies %>% mutate(study_author_year_std = standardize(study_author_year))

# Merge: left join to keep all df rows
merged <- df %>%
  left_join(
    studies,
    by = c("study_std" = "study_author_year_std"),
    suffix = c(".df", ".studies")
  )

dup_bases <- intersect(
  gsub("\\.df$", "", names(merged)[endsWith(names(merged), ".df")]),
  gsub("\\.studies$", "", names(merged)[endsWith(names(merged), ".studies")])
)

# 2. For each duplicated, keep the .y version, rename it to the base, and drop the .x version
for (col in dup_bases) {
  merged[[col]] <- merged[[paste0(col, ".studies")]]
}

# 3. Remove all .x and .y columns (now redundant), except the renamed ones
merged <- merged %>% select(-matches("\\.df$"), -matches("\\.studies$"))

# Find studies in df with no match in studies file
no_match_df <- merged %>%
  filter(is.na(primary_study_id)) %>%
  distinct(study)

# Find studies in studies file not matched in df
no_match_studies <- studies %>%
  filter(!study_author_year_std %in% df$study_std) %>%
  distinct(study_author_year)

# Print unmatched studies
print("Studies in df not matched in studies:")
print(no_match_df)

print("Studies in studies file not matched in df:")
print(no_match_studies)

#############################################################################
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
        .reactable-table th, .reactable-table td { box-sizing: border-box !important;
        }
        .reactable-table td svg {
        width: 100% !important;
        height: auto !important;
        aspect-ratio: 12 / 1 !important; /* adjust to your viewBox ratio */
        max-width: 100% !important;
        display: block;
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
  ## Row 3
  fluidRow(
    column(12,
           div(
             style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
             "Standardized Mean Difference in Depression Symptoms"
           ),
           div(
             style = "margin-left: 10px; margin-top: 6px; font-size: 12px",
             "A negative SMD indicates an intervention benefit"
           ),
           reactableOutput("forest_tbl", width = "100%")
    )
  )
)
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# Define server logic required 
server <- function(input, output, session) {
  
  # Studies panel: Number of unique studies
  output$studies_panel <- renderUI({
    n_studies <- length(unique(merged$study))
    div(class = "studies-panel",
        h4("No. of Studies"),
        span(class = "studies-count", n_studies),
        span(class = "tooltiptext", HTML(sprintf("Distinct count of Study Author Year: <b>%d</b>", n_studies)))
    )
  })
  
  # Count studies per country
  country_counts <- merged %>%
    distinct(study, country) %>%       # One row per unique study/country
    count(country)                     # Count unique studies per country
  
  country_names <- country_counts$country
  country_counts_vec <- country_counts$n
  country_iso3 <- countrycode(country_names, origin = "country.name", destination = "iso3c")
  country_name_lookup <- setNames(country_names, country_iso3)
  
  all_iso3 <- na.omit(countrycode::codelist$iso3c)
  all_iso3 <- setdiff(all_iso3, "ATA") # Remove Antarctica
  
  country_vals <- setNames(rep(0, length(all_iso3)), all_iso3)
  country_vals[country_iso3] <- country_counts_vec
  
  label_data <- data.frame(
    iso3c = country_iso3,
    count = as.numeric(country_counts$n)
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
  
  # World map
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
        textfont = list(size = 14, color = "white"),
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
          projection = list(type = "equirectangular"),
          lonaxis = list(range = c(-150, 150)),  
          lataxis = list(range = c(-50, 70))            
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
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
  
  grade_categories <- sapply(merged$grade_level, classify_grade_level)
  
  merged$grade_category <- grade_categories
  
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735")
  
  
  # ## School Level Graph
  output$school_level <- renderPlotly({
    # Prepare data for plot
    school_level_plot_data <- merged %>%
      distinct(study, grade_category) %>%
      mutate(
        grade_category = factor(
          grade_category,
          levels = c("Unclear", "High", "Middle+High", "Middle", "Elementary+Middle", "Elementary")
        )
      ) %>%
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
    studies_clean <- merged %>%
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
            y = median_schools + 3,
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
    studies_clean <- merged %>%
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
    studies_clean <- merged %>%
      mutate(number_participants = ifelse(number_participants == -999, NA, number_participants)) %>%
      filter(!is.na(number_participants) & number_participants > 0) %>%
      distinct(study_author_year, number_participants, .keep_all = TRUE) %>%
      mutate(
        label = paste0(study_author_year, "\nn=", number_participants),
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Students: <b>", number_participants, "</b>")
      )
    
    plot_ly(
      data = studies_clean,
      type = "treemap",
      labels = ~label,
      values = ~number_participants,
      parents = NA,  # No root node, each study is top-level
      textinfo = "label",
      marker = list(
        line = list(width = 2, color = "white"),
        colors = ~number_participants,
        colorscale = list(
          c(0, "#8ABB40"),
          c(1, "#104735")
        ),
        reversescale = FALSE
      ),
      hoverinfo = "text",
      text = ~hover_text,
      tiling = list(packing = "squarify")
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
    studies_clean <- merged %>%
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
    studies_clean <- merged %>%
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
  # ---- Forest Plot Table: Grouped/Merged cells with hierarchical borders ----
  
  # ----------- GROUPING / BLANKING / BORDER LOGIC (all functions together) -----------
  group_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Measure")
  
  # Hierarchical "pivot-style" blanking for reactable/gt tables
  # Hierarchical "pivot-style" blanking for reactable/gt tables
  # Hierarchical "pivot-style" blanking for reactable/gt tables
  hierarchical_blanker <- function(df, group_cols) {
    n <- nrow(df)
    if (n < 2) return(df)
    # For each row after the first...
    for (i in 2:n) {
      for (col_idx in seq_along(group_cols)) {
        col <- group_cols[col_idx]
        # If any previous grouping column differs, stop blanking this row for higher columns
        if (any(sapply(group_cols[1:(col_idx-1)], function(pc) !identical(df[[pc]][i], df[[pc]][i-1])))) {
          break
        }
        # If this value is the same as in the previous row, blank it
        if (identical(df[[col]][i], df[[col]][i-1])) {
          df[[col]][i] <- ""
        }
      }
    }
    df
  }
  
  # Add a borderType column for hierarchical borders in reactable
  add_border_type <- function(merged, group_cols) {
    borderType <- rep("none", nrow(merged))
    for (i in seq_len(nrow(merged))) {
      if (i == 1) {
        borderType[i] <- "thick"
      } else {
        is_diff <- any(sapply(group_cols, function(col) {
          as.character(merged[[col]][i]) != as.character(merged[[col]][i-1])
        }))
        if (is_diff) borderType[i] <- "thick"
      }
    }
    merged$borderType <- borderType
    merged
  }
  
  # ----------- DATA PREP -----------
  
  # 1. Standard errors and CIs on merged
  se <- sqrt(merged$vi)
  merged$lower <- merged$yi - 1.96 * se
  merged$upper <- merged$yi + 1.96 * se
  
  # 2. Join number_participants as n
  merged_forest <- merged %>%
    transmute(
      `Study Author Year` = study,
      `Intervention` = intervention,
      `Comparison` = comparison,
      `Outcome Measure` = outcome_measure,
      `Weeks` = outcome_timepoint,
      `n` = number_participants,
      `SMD` = round(yi, 3),
      lower = lower,
      upper = upper
    )
  
  # 4. Sort & apply blanking and border logic
  merged_forest <- merged_forest[do.call(order, merged_forest[group_cols]), ]
  merged_forest <- hierarchical_blanker(merged_forest, group_cols)
  merged_forest <- add_border_type(merged_forest, group_cols)
  
  # ----------- FOREST SVG COLUMN -----------
  
  max_n <- max(merged_forest$n, na.rm = TRUE)
  bubble_radius <- function(n, min_r = 4, max_r = 16) {
    if (is.na(n) || n <= 0) return(min_r)
    prop <- sqrt(n / max_n)
    r <- min_r + (max_r - min_r) * prop
    return(r)
  }
  
  make_forest_svg <- function(yi, lower, upper, n, show_axis = FALSE) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 1200
    svg_height <- if (show_axis) 120 else 48
    center_y <- svg_height / 2
    
    max_n <-  max(merged_forest$n, na.rm = TRUE)
    bubble_radius <- function(n, min_r = 6, max_r = 18) {
      if (is.na(n) || n <= 0) return(min_r)
      prop <- sqrt(n / max_n)
      r <- min_r + (max_r - min_r) * prop
      return(r)
    }
    r <- bubble_radius(n)
    
    scale <- function(x) ref_width * (x - min_x) / (max_x - min_x)
    
    axis_svg <- if (show_axis) {
      axis_y <- 55
      tick_label_y <- 80
      axis_label_y <- 110
      
      ticks <- seq(-3, 3, by =1)
      tick_x <- scale(ticks)
      axis_g <- htmltools::tagList(
        htmltools::tags$line(
          x1 = scale(min_x), x2 = scale(max_x), y1 = axis_y, y2 = axis_y,
          stroke = "#444", "stroke-width" = 1
        ),
        lapply(seq_along(tick_x), function(i) {
          htmltools::tags$g(
            htmltools::tags$line(
              x1 = tick_x[i], x2 = tick_x[i], y1 = axis_y, y2 = axis_y + 12,
              stroke = "#444", "stroke-width" = 1
            ),
            htmltools::tags$text(
              x = tick_x[i], y = tick_label_y,
              text.anchor = "middle", font.size = 13, font.family = "Arial", ticks[i]
            )
          )
        }),
        htmltools::tags$text(
          x = ref_width / 2, y = axis_label_y,
          "Standardized mean difference",
          font.size = 20, font.family = "Arial", text.anchor = "middle"
        )
      )
      htmltools::tags$g(axis_g)
    } else {NULL}
    
    forest_geom <- if (!is.na(yi) && !is.na(lower) && !is.na(upper) && !is.na(n)) list(
      htmltools::tags$line(
        x1 = scale(lower), x2 = scale(upper), y1 = center_y, y2 = center_y,
        stroke = "#333", "stroke-width" = 2
      ),
      htmltools::tags$circle(
        cx = scale(yi), cy = center_y, r = r,
        fill = ifelse(yi < -0.03, "#235223",        # green if less than -0.03
                      ifelse(yi > 0.03, "#E0C311",  # yellow if greater than 0.03
                             "#B0B0B0")),           # grey if between
        stroke = "#222", "stroke-width" = 1
      )
    ) else NULL
    
    as.character(
      htmltools::tags$svg(
        width = "100%",
        height = svg_height,
        viewBox = sprintf("0 0 %d %d", ref_width, svg_height),
        preserveAspectRatio = "xMidYMid meet",
        htmltools::tags$rect(x=0, y=0, width=ref_width, height=svg_height, fill="white"),
        forest_geom,
        htmltools::tags$line(x1 = scale(0), x2 = scale(0), y1 = 0, y2 = svg_height, stroke = "#888", "stroke-dasharray" = "2,2", "stroke-width" = 1),
        axis_svg
      )
    )
  }
  
  # ----------- FINAL ASSEMBLY & REACTABLE OUTPUT -----------
  
  # Add footer axis row
  nrow_merged <- nrow(merged_forest)
  forest_axis_footer <- merged_forest[1, ]
  forest_axis_footer[,] <- ""
  forest_axis_footer$borderType <- ""
  forest_axis_footer$` ` <- make_forest_svg(yi = NA, lower = NA, upper = NA, n = NA, show_axis = TRUE)
  merged_forest$` ` <- mapply(make_forest_svg, merged_forest$SMD, merged_forest$lower, merged_forest$upper, merged_forest$n, MoreArgs = list(show_axis = FALSE), SIMPLIFY = FALSE)
  merged_forest <- rbind(merged_forest, forest_axis_footer)
  
  merged_forest <- merged_forest[, c(
    "Study Author Year", "Intervention", "Comparison", "Outcome Measure", "Weeks", "SMD", " ", "borderType"
  )]
  
  rownames(merged_forest) <- NULL
  is_group_start <- function(value, index, column, data) {
    if (index == 1) return(TRUE)
    !identical(data[[column]][index], data[[column]][index - 1])
  }
  
  output$forest_tbl <- renderReactable({
    reactable(
      merged_forest,
      columns = list(
        borderType = colDef(show = FALSE),
        `Study Author Year` = colDef(
          name = "Study Author Year",
          minWidth = 120,
          maxWidth = 200,
          style = list(whiteSpace = "pre-line", wordBreak = "break-word", fontWeight = "bold")
        ),
        `Intervention` = colDef(
          name = "Intervention",
          minWidth = 150,
          maxWidth = 250,
          style = list(whiteSpace = "pre-line", wordBreak = "break-word")
        ),
        `Comparison` = colDef(
          name = "Comparison",
          minWidth = 120,
          maxWidth = 200,
          style = list(whiteSpace = "pre-line", wordBreak = "break-word")
        ),
        `Outcome Measure` = colDef(
          name = "Outcome Measure",
          minWidth = 170,
          maxWidth = 260,
          style = list(whiteSpace = "pre-line", wordBreak = "break-word")
        ),
        Weeks = colDef(
          name = "Weeks",
          minWidth = 50,
          maxWidth = 70,
          align = "right",
          style = list(borderTop = "2px solid #222")
        ),
        SMD = colDef(
          name = "SMD",
          minWidth = 70,
          maxWidth = 90,
          align = "right",
          format = colFormat(digits = 3),
          style = list(borderTop = "2px solid #222")
        ),
        ` ` = colDef(
          name = "",
          html = TRUE,
          minWidth = 700,
          resizable = TRUE,
          style = list(verticalAlign = "middle", textAlign = "center", padding = "0", borderTop = "2px solid #222"),
          sortable = FALSE,
          filterable = FALSE,
        )
      ),
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      style = list(fontFamily = "Arial, sans-serif"),
      fullWidth = TRUE,
      defaultPageSize = nrow(merged_forest)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

