#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, rnaturalearth,rnaturalearthdata, sf)

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
          color: darkgreen;
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
          color: darkgreen;
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
        margin-bottom: 30px; /* Adjust this value as needed */
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
         plotlyOutput("num_schools_plot"))
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
    c(0,   "#C7E1B8"),
    c(0.25, "#B4E6B4"),
    c(0.5, "#64C864"),
    c(0.75, "#329632"),
    c(1,   "#648C65")
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
    "#C7E1B8",  # lightest
    "#B4E6B4",  # rgb(180,230,180)
    "#64C864",  # rgb(100,200,100)
    "#329632",  # rgb(50,150,50)
    "#648C65")   # darkest
    
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
          font = list(color = "black"))) %>% 
      config(displayModeBar = FALSE)
  })
  
  # Number of Schools Graph
  output$num_schools_plot <- renderPlotly({
    # Clean the data first - replace -999 with NA
    studies_clean <- studies %>%
      mutate(number_schools = ifelse(number_schools == -999, NA, number_schools)) %>%
      filter(!is.na(number_schools)) %>%
      mutate(
        hover_text = paste0("Study: ", study_author_year, "<br>No. of Schools: ", number_schools)
      )
    
    # Calculate median (excluding NAs)
    median_schools <- median(studies_clean$number_schools, na.rm = TRUE)
    
    # Make the base ggplot
    num_schools_plot <- ggplot() +
      # Add the study points (KEEP hover for study bubbles)
      geom_point(
        data = studies_clean,
        aes(x = 1, y = number_schools, text = hover_text),
        size = 4,
        alpha = 0.6,
        color = "#329632"
      ) +
      # Add median line 
      geom_hline(
        yintercept = median_schools,
        linetype = "dotted",
        color = "gray60",
        size = 1
      ) +
      # Add median text 
      annotate(
        "text",
        x = 0.89,                           
        y = median_schools,                 
        label = "Median",
        color = "#329632",                  
        hjust = 0,                          
        vjust = 0.5                         
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
      labs(
        y = "Schools",
        x = ""
      ) +
      coord_cartesian(xlim = c(0.8, 1.4))
    
    # Convert to plotly
    ggplotly(num_schools_plot, tooltip = "text") %>%
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
          tickvals = c(0, 20, 40),
          ticktext = c("0", "20", "40")
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
