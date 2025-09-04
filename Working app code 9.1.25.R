#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi, shinyWidgets)

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

GLOBAL_MAX_N <- max(merged$number_participants, na.rm = TRUE)


###########################
# Grade level creation
classify_grade_level <- function(x) {
  x <- gsub(" ", "", x)
  if (tolower(x) %in% c("cannot tell", "unclear", "")) return("Unclear")
  grades_split <- unlist(strsplit(x, ","))
  if (any(is.na(suppressWarnings(as.numeric(grades_split))))) return("Unclear")
  grades_num <- as.numeric(grades_split)
  min_g <- min(grades_num)
  max_g <- max(grades_num)
  if (min_g >= 1 && max_g <= 5) return("Elementary")
  if (min_g >= 6 && max_g <= 8) return("Middle")
  if (min_g >= 9 && max_g <= 12) return("High")
  if (min_g <= 5 && max_g >= 6 && max_g <= 8) return("Elementary+Middle")
  if (min_g <= 8 && max_g >= 9 && max_g <= 12) return("Middle+High")
  if (min_g <= 5 && max_g >= 9) return("Unclear") # spans all ranges
  return("Unclear")
}

# School level creation
school_level_choices <- merged %>%
  mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
  distinct(grade_category) %>%
  pull(grade_category) %>%
  na.omit() %>%
  unique() %>%
  as.character()

# Optionally order them:
school_level_choices <- intersect(
  c("Unclear", "High", "Middle+High", "Middle", "Elementary+Middle", "Elementary"),
  school_level_choices
)

###########################
# Urbanicity creation
clean_urbanicity <- function(x) {
  if (is.na(x) || trimws(x) == "") return("Unclear")
  if (tolower(trimws(x)) %in% c("cannot tell", "unclear")) return("Unclear")
  x <- tolower(x)
  x <- gsub("[0-9]+\\.", "", x)
  x <- gsub("[\r\n]+", ",", x)
  parts <- unlist(strsplit(x, "[,;]+"))
  parts <- trimws(parts)
  possible <- c("rural", "suburban", "urban")
  found <- unique(parts[parts %in% possible])
  if (length(found) == 0) return("Unclear")
  # Capitalize each part, then collapse
  label <- paste(sort(found), collapse = "+")
  stringr::str_to_title(label)
}

merged <- merged %>%
  mutate(urbanicity_clean = sapply(urbanicity, clean_urbanicity))

urbanicity_choices <- merged %>%
  distinct(urbanicity_clean) %>%
  pull(urbanicity_clean) %>%
  as.character() %>%
  sort()

# If you want "Unclear" at the bottom:
urbanicity_choices <- c(setdiff(urbanicity_choices, "Unclear"), "Unclear")

###########################
# Grade level processing - Individual grades 1-12 plus Unclear
process_grade_levels <- function(grade_string) {
  if (is.na(grade_string) || tolower(trimws(grade_string)) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  
  # Clean and split the grade string
  grades_clean <- gsub(" ", "", grade_string)
  grades_split <- unlist(strsplit(grades_clean, ","))
  
  # Convert to numeric, keep only valid grades 1-12
  grades_numeric <- suppressWarnings(as.numeric(grades_split))
  valid_grades <- grades_numeric[!is.na(grades_numeric) & grades_numeric >= 1 & grades_numeric <= 12]
  
  if (length(valid_grades) == 0) {
    return("Unclear")
  }
  
  return(paste(sort(valid_grades), collapse = ","))
}

# Apply grade processing to merged data
merged <- merged %>%
  mutate(processed_grades = sapply(grade_level, process_grade_levels))

# Create individual grade choices (1-12 plus Unclear)
individual_grade_choices <- c(as.character(1:12), "Unclear")

# Helper function to check if a study's grades match selected filters
grades_match_filter <- function(processed_grades, selected_grades) {
  if (processed_grades == "Unclear") {
    return("Unclear" %in% selected_grades)
  }
  
  study_grades <- as.character(as.numeric(unlist(strsplit(processed_grades, ","))))
  return(any(study_grades %in% selected_grades))
}

#Filters
filtered_data <- reactive({
  # If ANY filter is empty, return empty data frame
  if (
    is.null(input$country_filter) || length(input$country_filter) == 0 ||
    is.null(input$school_level_filter) || length(input$school_level_filter) == 0 ||
    is.null(input$urbanicity_filter) || length(input$urbanicity_filter) == 0 ||
    is.null(input$individual_grade_filter) || length(input$individual_grade_filter) == 0 ||
    is.null(input$school_type_filter) || length(input$school_type_filter) == 0 ||
    is.null(input$outcome_filter) || length(input$outcome_filter) == 0
  ) {
    return(merged[0,]) # empty df with correct columns
  }
  
  data <- merged %>%
    mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
    filter(country %in% input$country_filter) %>%
    filter(grade_category %in% input$school_level_filter) %>%
    filter(urbanicity_clean %in% input$urbanicity_filter) %>%
    filter(sapply(processed_grades, function(x) grades_match_filter(x, input$individual_grade_filter))) %>%
    filter(school_type %in% input$school_type_filter) %>%
    filter(outcome_measure %in% input$outcome_filter)
  
  data
})

###########################
# School type processing - Individual types plus Unclear
process_school_types <- function(school_type_string) {
  if (is.na(school_type_string) || tolower(trimws(school_type_string)) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  
  # Clean and split the school type string
  # Handle various separators: comma, newline characters, etc.
  school_types_clean <- gsub("[\r\n]+", ",", school_type_string)  # Replace newlines with commas
  school_types_split <- unlist(strsplit(school_types_clean, "[,;]+"))
  school_types_split <- trimws(school_types_split)  # Remove whitespace
  
  # Valid school types
  valid_types <- c("Public", "Private", "Charter")
  found_types <- unique(school_types_split[school_types_split %in% valid_types])
  
  if (length(found_types) == 0) {
    return("Unclear")
  }
  
  return(paste(sort(found_types), collapse = ","))
}

# Apply school type processing to merged data
merged <- merged %>%
  mutate(processed_school_types = sapply(school_type, process_school_types))

# Create individual school type choices
individual_school_type_choices <- c("Public", "Private", "Charter", "Unclear")

# Helper function to check if a study's school types match selected filters
school_types_match_filter <- function(processed_school_types, selected_school_types) {
  if (processed_school_types == "Unclear") {
    return("Unclear" %in% selected_school_types)
  }
  
  study_school_types <- unlist(strsplit(processed_school_types, ","))
  return(any(study_school_types %in% selected_school_types))
}

###########################
# Outcome measure choices - Root-based grouping
# Root-based outcome measure grouping with improved suffix handling
process_outcome_measures_roots <- function(all_outcome_measures) {
  # Remove NA values and empty strings for processing
  clean_measures <- all_outcome_measures[!is.na(all_outcome_measures) & trimws(all_outcome_measures) != ""]
  
  if (length(clean_measures) == 0) return(rep("Other/Unclear", length(all_outcome_measures)))
  
  # Function to extract meaningful word combinations
  extract_meaningful_parts <- function(measure) {
    # Remove parentheses content
    clean_measure <- gsub("\\([^)]*\\)", "", measure)
    
    # Remove common suffixes and modifiers (updated with colon variants)
    clean_measure <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", clean_measure)  # Handle colon suffixes
    clean_measure <- gsub("\\s*Short\\s+Version.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Revised.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-2.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-Youth.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-II.*$", "", clean_measure)
    clean_measure <- gsub("\\s*for\\s+Children.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Depression\\s+Sub-Scale.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Depression\\s+Subscale.*$", "", clean_measure)
    clean_measure <- gsub("-9$", "", clean_measure)  # Handle PHQ-9 -> PHQ
    
    # Clean up extra spaces
    clean_measure <- gsub("\\s+", " ", trimws(clean_measure))
    
    return(clean_measure)
  }
  
  # Extract roots for all measures
  roots <- sapply(clean_measures, extract_meaningful_parts, USE.NAMES = FALSE)
  
  # Group identical roots
  root_groups <- split(seq_along(roots), roots)
  
  # Create result mapping
  result <- character(length(all_outcome_measures))
  clean_idx <- 1
  
  for (i in seq_along(all_outcome_measures)) {
    if (is.na(all_outcome_measures[i]) || trimws(all_outcome_measures[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      root <- roots[clean_idx]
      # If only one measure has this root, keep the original name (truncated)
      if (length(root_groups[[root]]) == 1) {
        # For single measures, use a cleaned version of the original
        original <- clean_measures[clean_idx]
        # Remove parentheses and long suffixes for cleaner display
        cleaned_original <- gsub("\\([^)]*\\).*$", "", original)
        cleaned_original <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", cleaned_original)
        cleaned_original <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", cleaned_original)
        cleaned_original <- gsub("-9$", "", cleaned_original)  # Handle PHQ-9
        cleaned_original <- trimws(cleaned_original)
        result[i] <- cleaned_original
      } else {
        # Multiple measures share this root, use the root as group name
        result[i] <- root
      }
      clean_idx <- clean_idx + 1
    }
  }
  
  return(result)
}

# Apply root-based processing to merged data
processed_outcomes_roots <- process_outcome_measures_roots(merged$outcome_measure)
merged <- merged %>%
  mutate(processed_outcome_measure_roots = processed_outcomes_roots)

# Create outcome measure family choices
outcome_measure_family_choices <- merged %>%
  distinct(processed_outcome_measure_roots) %>%
  pull(processed_outcome_measure_roots) %>%
  sort()

# Helper function to check if a study's outcome measures match selected filters
outcome_measures_match_filter_roots <- function(processed_outcome, selected_outcomes) {
  return(processed_outcome %in% selected_outcomes)
}
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
        .reactable-table th, .reactable-table td { 
          box-sizing: border-box !important;
        }
        .reactable-table td svg {
          width: 100% !important;
          height: auto !important;
          aspect-ratio: 12 / 1 !important;
          max-width: 100% !important;
          display: block;
        }
        .reactable thead th {
          position: sticky;
          top: 0;
          background-color: #fff;
          z-index: 2;
        }
        
        /* Make reactable container allow overflow for tooltips */
        .reactable {
          overflow: visible !important;
        }
        
        .reactable-table {
          overflow: visible !important;
        }
        
        /* Custom tooltip that floats on top of everything */
        .forest-tooltip {
          position: relative;
        }
        
        .forest-tooltip .tooltip-content {
          visibility: hidden;
          position: fixed;
          background: white;
          color: black;
          padding: 12px 16px;
          border-radius: 8px;
          border: 2px solid #333;
          font-size: 13px;
          line-height: 1.5;
          font-family: "Source Sans", sans-serif;
          z-index: 999999;
          box-shadow: 0 8px 24px rgba(0,0,0,0.5);
          max-width: 600px;
          min-width: 450px;
          width: auto;
          word-wrap: break-word;
          white-space: normal;
          opacity: 0;
          transition: opacity 0.2s ease-in-out;
          pointer-events: none;
        }
        
        .forest-tooltip:hover .tooltip-content {
          visibility: visible;
          opacity: 1;
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
  
  HTML('
<script>
document.addEventListener("DOMContentLoaded", function() {
  let currentTooltip = null;
  
  document.addEventListener("mouseover", function(e) {
    if (e.target.closest(".forest-tooltip")) {
      const tooltip = e.target.closest(".forest-tooltip").querySelector(".tooltip-content");
      if (tooltip) {
        currentTooltip = tooltip;
        updateTooltipPosition(e, tooltip);
      }
    }
  });
  
  document.addEventListener("mousemove", function(e) {
    if (currentTooltip) {
      updateTooltipPosition(e, currentTooltip);
    }
  });
  
  document.addEventListener("mouseout", function(e) {
    if (!e.target.closest(".forest-tooltip")) {
      currentTooltip = null;
    }
  });
  
  function updateTooltipPosition(e, tooltip) {
    const rect = tooltip.getBoundingClientRect();
    let x = e.clientX + 15;
    let y = e.clientY + 10;
    
    // Adjust if tooltip would go off screen
    if (x + rect.width > window.innerWidth) {
      x = e.clientX - rect.width - 15;
    }
    if (y + rect.height > window.innerHeight) {
      y = e.clientY - rect.height - 10;
    }
    
    tooltip.style.left = x + "px";
    tooltip.style.top = y + "px";
  }
});
</script>
'),
  
  # Application title
  fluidRow(
    column(12,
           div(class = "title-panel", "School-based Interventions to Reduce Depression")
    )
  ),
  
  ### Filter dropdowns - restructured layout
  fluidRow(
    # Count of studies on the left
    column(2,
           uiOutput("studies_panel")
    ),
    
    # Filter area on the right (10 columns)
    column(10,
           # First row of filters
           fluidRow(
             column(1),  # Empty column to push filters left
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "country_filter",
                      label = "Country",
                      choices = sort(unique(merged$country)),
                      selected = sort(unique(merged$country)),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(unique(merged$country)) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "school_level_filter",
                      label = "School Level",
                      choices = school_level_choices,
                      selected = school_level_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(school_level_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "urbanicity_filter",
                      label = "Urbanicity",
                      choices = urbanicity_choices,
                      selected = urbanicity_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(urbanicity_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(2)  # Empty column on the right
           ),
           
           # Second row of filters
           fluidRow(
             column(1),  # Empty column to push filters left
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "individual_grade_filter",
                      label = "Individual Grades",
                      choices = individual_grade_choices,
                      selected = individual_grade_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(individual_grade_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "individual_school_type_filter",
                      label = "School Type",
                      choices = individual_school_type_choices,
                      selected = individual_school_type_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(individual_school_type_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "outcome_family_filter",
                      label = "Outcome Measure Family",
                      choices = outcome_measure_family_choices,
                      selected = outcome_measure_family_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(outcome_measure_family_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(2)  # Empty column on the right
           )
    )
  ),
  
  # Tabs
  tabsetPanel(
    tabPanel("Visualizations",
             ### Row 1
             fluidRow(
               # World Map
               column(6,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Country"
                      ),
                      plotlyOutput("world_map")
               ),
               
               # School Level Bar Chart
               column(3,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "School Level"
                      ),
                      plotlyOutput("school_level")
               ),
               column(3, 
                      div(style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                          "Urbanicity"
                      ),
                      plotlyOutput("urbanicity")
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
                      plotlyOutput("pct_fem", height = "350px"))
             ),
             
             ### Row 3
             fluidRow(
               column(4,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Race/Ethnicity Percentages Across Selected Studies"
                      ),
                      plotlyOutput("race_ethnicity_graph", height = "350px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "FRPL Percentages"
                      ),
                      plotlyOutput("frpl_graph", height = "350px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "ELL Percentages"
                      ),
                      plotlyOutput("ell_graph", height = "350px")),
               column(4,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Outcome Measures Used"
                      ),
                      plotlyOutput("outcomes_graph", height = "350px")),
             )  
    ),  
    
    tabPanel("Forest Plot",
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
  )
)
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# Define server logic required 
server <- function(input, output, session) {
  
  #Filters
  filtered_data <- reactive({
    # If ANY filter is empty, return empty data frame
    if (
      is.null(input$country_filter) || length(input$country_filter) == 0 ||
      is.null(input$school_level_filter) || length(input$school_level_filter) == 0 ||
      is.null(input$urbanicity_filter) || length(input$urbanicity_filter) == 0 ||
      is.null(input$individual_grade_filter) || length(input$individual_grade_filter) == 0 ||
      is.null(input$individual_school_type_filter) || length(input$individual_school_type_filter) == 0 ||
      is.null(input$outcome_family_filter) || length(input$outcome_family_filter) == 0
    ) {
      return(merged[0,]) # empty df with correct columns
    }
    
    data <- merged %>%
      mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
      filter(country %in% input$country_filter) %>%
      filter(grade_category %in% input$school_level_filter) %>%
      filter(urbanicity_clean %in% input$urbanicity_filter) %>%
      filter(sapply(processed_grades, function(x) grades_match_filter(x, input$individual_grade_filter))) %>%
      filter(sapply(processed_school_types, function(x) school_types_match_filter(x, input$individual_school_type_filter))) %>%
      filter(sapply(processed_outcome_measure_roots, function(x) outcome_measures_match_filter_roots(x, input$outcome_family_filter)))  # CHANGED: from processed_outcome_measure_simple to processed_outcome_measure_roots
    
    data
  })
  
  
  # Studies panel: Number of unique studies
  output$studies_panel <- renderUI({
    n_studies <- length(unique(filtered_data()$study))
    div(class = "studies-panel",
        style = "border: 2px solid black; padding: 8px 12px 25px 12px; border-radius: 5px; margin-top: 22px; width: 180px;",
        h4("No. of Studies", style = "margin: 0 0 5px 0;"),
        span(class = "studies-count", n_studies),
        span(class = "tooltiptext", HTML(sprintf("Distinct count of Study Author Year: <b>%d</b>", n_studies)))
    )
  })
  
  # Count studies per country
  # All code below goes inside your server function
  
  # 1. Reactive: Count studies per country using the filtered data
  country_counts <- reactive({
    filtered_data() %>%
      distinct(study, country) %>%
      count(country)
  })
  
  # 2. All country-level mapping logic as a reactive (so it's reusable)
  country_map_data <- reactive({
    cc <- country_counts()
    country_names <- cc$country
    country_counts_vec <- cc$n
    country_iso3 <- countrycode(country_names, origin = "country.name", destination = "iso3c")
    country_name_lookup <- setNames(country_names, country_iso3)
    
    all_iso3 <- na.omit(countrycode::codelist$iso3c)
    all_iso3 <- setdiff(all_iso3, "ATA") # Remove Antarctica
    
    country_vals <- setNames(rep(0, length(all_iso3)), all_iso3)
    country_vals[country_iso3] <- country_counts_vec
    
    studied_iso3 <- names(country_vals)[country_vals > 0]
    studied_counts <- country_vals[studied_iso3]
    studied_names <- country_names[match(studied_iso3, country_iso3)]
    
    list(
      country_vals = country_vals,
      country_name_lookup = country_name_lookup,
      studied_iso3 = studied_iso3,
      studied_counts = studied_counts,
      studied_names = studied_names
    )
  })
  
  green_scale <- list(
    c(0, "#8ABB40"),
    c(0.33, "#489D46"),
    c(0.66, "#007030"),
    c(1,   "#104735")
  )
  
  # 3. Map output (filtered)
  # 3. Map output (filtered) - Remove toolbar and fill vertical space better
  output$world_map <- renderPlotly({
    d <- country_map_data()
    
    plot_geo() %>%
      add_trace(
        z = ifelse(d$country_vals > 0, d$country_vals, NA),
        locations = names(d$country_vals),
        type = "choropleth",
        locationmode = "ISO-3",
        colorscale = green_scale,
        marker = list(line = list(color = 'black', width = 2)),
        zmin = 0,
        zmax = max(d$country_vals, na.rm = TRUE),
        showscale = FALSE,
        text = d$country_name_lookup[names(d$country_vals)],
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
        locations = d$studied_iso3,
        locationmode = "ISO-3",
        text = d$studied_counts,
        hovertext = paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>", d$studied_names, "</b><br>",
          "No. of Studies: <b>", d$studied_counts, "</b></span>"
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
        margin = list(l = 20, r = 0, t = 0, b = 0)
      ) %>%
      config(
        displayModeBar = FALSE,  # Remove the toolbar
        scrollZoom = FALSE,      # Disable scroll zoom
        doubleClick = FALSE      # Disable double-click zoom
      )
  })
  
  ########################################################
  # Grade levels
  classify_grade_level <- function(x) {
    x <- gsub(" ", "", x)
    if (tolower(x) %in% c("cannot tell", "unclear", "")) return("Unclear")
    grades_split <- unlist(strsplit(x, ","))
    if (any(is.na(suppressWarnings(as.numeric(grades_split))))) return("Unclear")
    grades_num <- as.numeric(grades_split)
    min_g <- min(grades_num)
    max_g <- max(grades_num)
    if (min_g >= 1 && max_g <= 5) return("Elementary")
    if (min_g >= 6 && max_g <= 8) return("Middle")
    if (min_g >= 9 && max_g <= 12) return("High")
    if (min_g <= 5 && max_g >= 6 && max_g <= 8) return("Elementary+Middle")
    if (min_g <= 8 && max_g >= 9 && max_g <= 12) return("Middle+High")
    if (min_g <= 5 && max_g >= 9) return("Unclear") # spans all ranges
    return("Unclear")
  }
  
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735"
  )
  
  output$school_level <- renderPlotly({
    school_level_plot_data <- filtered_data() %>%
      mutate(
        grade_category = sapply(grade_level, classify_grade_level)
      ) %>%
      distinct(study, grade_category) %>%
      mutate(
        grade_category = factor(
          grade_category,
          levels = c("Unclear", "High", "Middle+High", "Middle", "Elementary+Middle", "Elementary")
        )
      ) %>%
      group_by(grade_category) %>%
      summarise(grade_n = n(), .groups = "drop") %>%
      mutate(
        hover = sprintf(
          "School Level: <b>%s</b><br>Number of Studies: <b>%d</b>",
          grade_category, grade_n
        )
      )
    
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
  
  ################################################
  # Urbanicity graph
  
  output$urbanicity <- renderPlotly({
    # Check if filtered data is empty
    filtered_df <- filtered_data()
    
    if (nrow(filtered_df) == 0) {
      # Return empty plot when no data is filtered
      empty_plot <- plot_ly() %>%
        layout(
          title = "",
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          margin = list(l = 90, r = 5, t = 5, b = 5)
        ) %>%
        config(displayModeBar = FALSE)
      
      return(empty_plot)
    }
    
    urbanicity_plot_data <- filtered_df %>%
      # Handle NA values properly
      mutate(
        urbanicity_clean = ifelse(is.na(urbanicity_clean), "Unclear", urbanicity_clean)
      ) %>%
      distinct(study, urbanicity_clean) %>%
      # Create factor with proper ordering - single categories first, then combinations, then unclear
      mutate(
        urbanicity_clean = factor(
          urbanicity_clean,
          levels = c("Rural", "Suburban", "Urban", 
                     "Rural+Suburban", "Rural+Urban", "Suburban+Urban", 
                     "Rural+Suburban+Urban", "Unclear")
        )
      ) %>%
      group_by(urbanicity_clean) %>%
      summarise(urbanicity_n = n(), .groups = "drop") %>%
      # Remove any empty groups
      filter(urbanicity_n > 0)
    
    # Check if processed data is empty after filtering
    if (nrow(urbanicity_plot_data) == 0) {
      # Return empty plot when no valid data remains
      empty_plot <- plot_ly() %>%
        layout(
          title = "",
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          margin = list(l = 90, r = 5, t = 5, b = 5)
        ) %>%
        config(displayModeBar = FALSE)
      
      return(empty_plot)
    }
    
    # Add hover text after confirming data exists
    urbanicity_plot_data <- urbanicity_plot_data %>%
      mutate(
        hover = sprintf(
          "Urbanicity: <b>%s</b><br>Number of Studies: <b>%d</b>",
          urbanicity_clean, urbanicity_n
        )
      )
    
    urbanicity_plot <- ggplot(urbanicity_plot_data, aes(
      x = urbanicity_clean,
      y = urbanicity_n,
      fill = urbanicity_n,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, fill = NULL) +
      # Reverse the y-axis so "Rural" appears at top and "Unclear" at bottom
      scale_x_discrete(limits = rev)
    
    ggplotly(urbanicity_plot, tooltip = "text") %>%
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
  ################################################################################################
  # Number of Schools Graph
  output$num_schools_plot <- renderPlotly({
    studies_clean <- filtered_data() %>%
      mutate(number_schools = ifelse(number_schools == -999, NA, number_schools)) %>%
      filter(!is.na(number_schools)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Schools: <b>", number_schools, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering
    studies_with_positions <- studies_clean %>%
      arrange(number_schools) %>%
      group_by(number_schools) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters safely
    median_schools <- median(studies_with_positions$number_schools, na.rm = TRUE)
    max_y <- max(studies_with_positions$number_schools, na.rm = TRUE)
    min_y <- min(studies_with_positions$number_schools, na.rm = TRUE)
    y_buffer <- max(2, 0.07 * (max_y - min_y))
    y_range <- c(max(0, min_y - y_buffer), max_y + y_buffer)
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_schools, length(x_dense_no_bubbles))
    
    # Create ggplot
    num_schools_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = number_schools, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = y_range,
        breaks = pretty(y_range, n = 5),
        expand = c(0, 0)
      ) +
      labs(y = "Schools", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
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
          range = y_range
        ),
        annotations = list(
          list(
            x = 1.35,  # Moved from 1.3 to 1.35 (further right)
            y = median_schools,
            yref = "y",
            xref = "x",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_schools, median_schools),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", median_schools),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  ##########################################################################################
  # Number of classrooms graph
  output$num_class_plot <- renderPlotly({
    studies_clean <- filtered_data() %>%
      mutate(number_classrooms = ifelse(number_classrooms == -999, NA, number_classrooms)) %>%
      filter(!is.na(number_classrooms)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Classrooms: <b>", number_classrooms, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering - same as schools plot
    studies_with_positions <- studies_clean %>%
      arrange(number_classrooms) %>%
      group_by(number_classrooms) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters safely
    median_class <- median(studies_with_positions$number_classrooms, na.rm = TRUE)
    max_y <- max(studies_with_positions$number_classrooms, na.rm = TRUE)
    min_y <- min(studies_with_positions$number_classrooms, na.rm = TRUE)
    y_buffer <- max(2, 0.07 * (max_y - min_y))
    y_range <- c(max(0, min_y - y_buffer), max_y + y_buffer)
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_class, length(x_dense_no_bubbles))
    
    # Create ggplot
    num_class_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = number_classrooms, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = y_range,
        breaks = pretty(y_range, n = 5),
        expand = c(0, 0)
      ) +
      labs(y = "Classrooms", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
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
          range = y_range
        ),
        annotations = list(
          list(
            x = 1.35,  # Moved right like in schools plot
            y = median_class,
            yref = "y",
            xref = "x",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_class, median_class),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", median_class),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  ############################################################################
  # Number of students plot
  output$num_students_tile <- renderPlotly({
    studies_clean <- filtered_data() %>%
      mutate(number_participants = ifelse(number_participants == -999, NA, number_participants)) %>%
      filter(!is.na(number_participants) & number_participants > 0) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%    mutate(
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
    studies_clean <- filtered_data() %>%
      mutate(average_age = ifelse(average_age == -999, NA, average_age)) %>%
      filter(!is.na(average_age)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>% 
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Average Age: <b>", average_age, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering - same as schools and classrooms plots
    studies_with_positions <- studies_clean %>%
      arrange(average_age) %>%
      group_by(average_age) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters safely
    median_age <- round(median(studies_with_positions$average_age, na.rm = TRUE), 1)
    max_y <- max(studies_with_positions$average_age, na.rm = TRUE)
    min_y <- min(studies_with_positions$average_age, na.rm = TRUE)
    y_buffer <- max(1, 0.07 * (max_y - min_y))
    y_range <- c(floor(min_y - y_buffer), ceiling(max_y + y_buffer))
    
    # Dynamic ticks (up to 9 ticks, step 1 if range is reasonable)
    pretty_ticks <- pretty(y_range, n = 9)
    pretty_ticks <- pretty_ticks[pretty_ticks >= y_range[1] & pretty_ticks <= y_range[2]]
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_age, length(x_dense_no_bubbles))
    
    # Create ggplot
    num_age_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = average_age, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = y_range,
        breaks = pretty_ticks,
        labels = as.character(pretty_ticks),
        expand = c(0, 0)
      ) +
      labs(y = "Age, years", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
    p <- ggplotly(num_age_plot, tooltip = "text") %>%
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
          range = y_range,
          tickvals = pretty_ticks,
          ticktext = as.character(pretty_ticks)
        ),
        annotations = list(
          list(
            x = 1.35,  # Moved right like in other plots
            y = median_age,
            yref = "y",
            xref = "x",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_age, median_age),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", median_age),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  ##################################################################################
  # Female graph
  output$pct_fem <- renderPlotly({
    studies_clean <- filtered_data() %>%
      mutate(percent_female = ifelse(percent_female == -999, NA, percent_female)) %>%
      filter(!is.na(percent_female)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(percent_female = percent_female * 100) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent Female: <b>", percent_female, "%", "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering - same as other plots
    studies_with_positions <- studies_clean %>%
      arrange(percent_female) %>%
      group_by(percent_female) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters
    median_pct <- round(median(studies_with_positions$percent_female, na.rm = TRUE), 1)
    pretty_ticks <- c(0, 25, 50, 75, 100)
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_pct, length(x_dense_no_bubbles))
    
    # Create ggplot
    pct_fem_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = percent_female, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = c(0, 100),
        breaks = pretty_ticks,
        labels = paste0(pretty_ticks, "%"),
        expand = c(0, 0)
      ) +
      labs(y = "Percent", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
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
          range = c(0, 100),
          tickvals = pretty_ticks,
          ticktext = paste0(pretty_ticks, "%")
        ),
        annotations = list(
          list(
            x = 1.35,  # Moved right like in other plots
            y = median_pct,
            xref = "x",
            yref = "y",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_pct, median_pct),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", median_pct, "%"),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  
  ###############################################
  # Race/ethnicity graph
  
  # Green color scale
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735"
  )
  
  output$race_ethnicity_graph <- renderPlotly({
    race_plot_data <- filtered_data() %>%
      # First, get one row per study with the racial composition
      group_by(study) %>%
      slice(1) %>%  # Take first row for each study to avoid double counting
      ungroup() %>%
      # Convert character values to numeric and replace -999 with NA
      mutate(
        percent_white = as.numeric(ifelse(percent_white == -999, NA, percent_white)),
        percent_black = as.numeric(ifelse(percent_black == -999, NA, percent_black)),
        percent_aian = as.numeric(ifelse(percent_aian == -999, NA, percent_aian)),
        percent_nhpi = as.numeric(ifelse(percent_nhpi == -999, NA, percent_nhpi)),
        percent_asian = as.numeric(ifelse(percent_asian == -999, NA, percent_asian)),
        percent_latinx = as.numeric(ifelse(percent_latinx == -999, NA, percent_latinx)),
        percent_other = as.numeric(ifelse(percent_other == -999, NA, percent_other))
      ) %>%
      # Calculate mean proportions across all studies (excluding NA values)
      summarise(
        White = mean(percent_white, na.rm = TRUE),
        Black = mean(percent_black, na.rm = TRUE),
        `American Indian/\nAlaska Native` = mean(percent_aian, na.rm = TRUE),
        `Native Hawaiian/\nPacific Islander` = mean(percent_nhpi, na.rm = TRUE),
        Asian = mean(percent_asian, na.rm = TRUE),
        Latinx = mean(percent_latinx, na.rm = TRUE),
        Other = mean(percent_other, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Transform to long format for plotting
      pivot_longer(
        cols = everything(),
        names_to = "race_ethnicity",
        values_to = "proportion"
      ) %>%
      # Remove any rows with NA proportions
      filter(!is.na(proportion)) %>%
      # Order by proportion (ASCENDING for coord_flip to show largest at top)
      arrange(proportion) %>%
      mutate(
        race_ethnicity = factor(race_ethnicity, levels = race_ethnicity),
        hover = sprintf(
          "Race/Ethnicity: <b>%s</b><br>Average Proportion: <b>%.1f%%</b>",
          gsub("\n", " ", race_ethnicity), proportion * 100
        )
      )
    
    race_plot <- ggplot(race_plot_data, aes(
      x = race_ethnicity,
      y = proportion,
      fill = proportion,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, fill = NULL)
    
    ggplotly(race_plot, tooltip = "text") %>%
      layout(
        margin = list(l = 150, r = 5, t = 5, b = 5),
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      ) %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  ###################################################################################
  # FRPL Graph 
  output$frpl_graph <- renderPlotly({
    studies_clean <- filtered_data() %>% 
      mutate(percent_FRPL = ifelse(percent_FRPL == -999, NA, percent_FRPL)) %>%
      filter(!is.na(percent_FRPL)) %>%
      mutate(percent_FRPL = percent_FRPL * 100) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent FRPL: <b>", round(percent_FRPL, 1), "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering - same as other plots
    studies_with_positions <- studies_clean %>%
      arrange(percent_FRPL) %>%
      group_by(percent_FRPL) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters
    median_frpl <- median(studies_with_positions$percent_FRPL, na.rm = TRUE)
    max_y <- max(studies_with_positions$percent_FRPL, na.rm = TRUE)
    min_y <- min(studies_with_positions$percent_FRPL, na.rm = TRUE)
    y_buffer <- max(2, 0.07 * (max_y - min_y))
    y_range <- c(min(0, min_y - y_buffer), max_y + y_buffer)
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_frpl, length(x_dense_no_bubbles))
    
    pretty_ticks <- pretty(y_range, n = 5)
    pretty_ticks <- pretty_ticks[pretty_ticks >= y_range[1] & pretty_ticks <= y_range[2]]
    
    # Create ggplot
    frpl_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = percent_FRPL, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = y_range,
        breaks = pretty_ticks,
        labels = as.character(pretty_ticks),
        expand = c(0, 0)
      ) +
      labs(y = "Percent FRPL (%)", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
    p <- ggplotly(frpl_plot, tooltip = "text") %>%
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
          range = y_range,
          tickvals = pretty_ticks,
          ticktext = as.character(pretty_ticks)
        ),
        annotations = list(
          list(
            x = 1.35,  # Moved right like in other plots
            y = median_frpl,
            yref = "y",
            xref = "x",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_frpl, median_frpl),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", round(median_frpl, 1), "%"),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  
  ###################################################################################
  # ELL Graph 
  output$ell_graph <- renderPlotly({
    studies_clean <- filtered_data() %>% 
      mutate(percent_ELL = ifelse(percent_ELL == -999, NA, percent_ELL)) %>%
      filter(!is.na(percent_ELL)) %>%
      # Additional safety checks
      filter(is.finite(percent_ELL)) %>%
      mutate(percent_ELL = percent_ELL * 100) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        # Ensure hover text components are properly formatted
        percent_ELL_rounded = round(percent_ELL, 1),
        hover_text = paste0("Study: <b>", as.character(study_author_year), "</b><br>Percent ELL: <b>", percent_ELL_rounded, "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers"))
    }
    
    # Vectorized jittering - same as other plots
    studies_with_positions <- studies_clean %>%
      arrange(percent_ELL) %>%
      group_by(percent_ELL) %>%
      mutate(
        group_size = n(),
        position_in_group = row_number() - 1,
        x_pos = ifelse(
          group_size == 1,
          1.0,
          1.0 + (position_in_group - (group_size - 1) / 2) * (0.5 / group_size)
        )
      ) %>%
      ungroup()
    
    # Calculate plot parameters with safety checks
    median_ell <- round(median(studies_with_positions$percent_ELL, na.rm = TRUE), 1)
    max_y <- max(studies_with_positions$percent_ELL, na.rm = TRUE)
    min_y <- min(studies_with_positions$percent_ELL, na.rm = TRUE)
    
    # Ensure we have valid values
    if (!is.finite(median_ell)) median_ell <- 0
    if (!is.finite(max_y)) max_y <- 100
    if (!is.finite(min_y)) min_y <- 0
    
    y_buffer <- max(2, 0.07 * (max_y - min_y))
    y_range <- c(max(0, min_y - y_buffer), max_y + y_buffer)
    
    # Create invisible markers for median hover (avoiding jittered bubble area)
    x_dense <- seq(0.6, 1.4, length.out = 30)
    exclude_window <- 0.35  # Exclude area around bubbles
    x_dense_no_bubbles <- x_dense[abs(x_dense - 1) > exclude_window]
    y_dense_no_bubbles <- rep(median_ell, length(x_dense_no_bubbles))
    
    pretty_ticks <- pretty(y_range, n = 5)
    pretty_ticks <- pretty_ticks[pretty_ticks >= y_range[1] & pretty_ticks <= y_range[2]]
    
    # Create ggplot
    ell_plot <- ggplot() +
      geom_point(
        data = studies_with_positions,
        aes(x = x_pos, y = percent_ELL, text = hover_text),
        size = 4,
        alpha = 0.7,
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
        limits = y_range,
        breaks = pretty_ticks,
        labels = as.character(pretty_ticks),
        expand = c(0, 0)
      ) +
      labs(y = "Percent ELL (%)", x = "") +
      coord_cartesian(xlim = c(0.6, 1.4))
    
    # Convert to plotly
    p <- ggplotly(ell_plot, tooltip = "text") %>%
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
          range = y_range,
          tickvals = pretty_ticks,
          ticktext = as.character(pretty_ticks)
        ),
        annotations = list(
          list(
            x = 1.35,
            y = median_ell,
            yref = "y",
            xref = "x",
            text = "Median",
            showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center",
            yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add median line
    p <- p %>%
      plotly::add_trace(
        x = c(0.6, 1.4),
        y = c(median_ell, median_ell),
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#8ABB40",
          dash = "dot",
          width = 4
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    # Add invisible markers for median hover
    if (length(x_dense_no_bubbles) > 0 && length(y_dense_no_bubbles) > 0) {
      p <- p %>%
        plotly::add_trace(
          x = x_dense_no_bubbles,
          y = y_dense_no_bubbles,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(0,0,0,0)",
            size = 12
          ),
          text = paste0("Median = ", median_ell, "%"),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }
    
    p <- p %>% layout(dragmode = FALSE) %>% config(displayModeBar = FALSE)
    p
  })
  ###################################################################################
  # Outcomes graph
  # Outcomes graph
  output$outcomes_graph <- renderPlotly({
    # Check if filtered data is empty
    filtered_df <- filtered_data()
    
    if (nrow(filtered_df) == 0) {
      # Return empty plot when no data is filtered
      empty_plot <- plot_ly() %>%
        layout(
          title = "",
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          margin = list(l = 120, r = 5, t = 5, b = 5)  # Increased left margin for outcome names
        ) %>%
        config(displayModeBar = FALSE)
      
      return(empty_plot)
    }
    
    outcomes_plot_data <- filtered_df %>%
      # Handle NA values properly
      mutate(
        processed_outcome_measure_roots = ifelse(
          is.na(processed_outcome_measure_roots) | trimws(processed_outcome_measure_roots) == "", 
          "Other/Unclear", 
          processed_outcome_measure_roots
        )
      ) %>%
      distinct(study, processed_outcome_measure_roots) %>%
      group_by(processed_outcome_measure_roots) %>%
      summarise(outcome_n = n(), .groups = "drop") %>%
      # Remove any empty groups
      filter(outcome_n > 0) %>%
      # Order by count (descending) for better visualization
      arrange(desc(outcome_n)) %>%
      # Create factor to maintain order in plot
      mutate(
        processed_outcome_measure_roots = factor(
          processed_outcome_measure_roots,
          levels = processed_outcome_measure_roots
        )
      )
    
    # Check if processed data is empty after filtering
    if (nrow(outcomes_plot_data) == 0) {
      # Return empty plot when no valid data remains
      empty_plot <- plot_ly() %>%
        layout(
          title = "",
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          margin = list(l = 120, r = 5, t = 5, b = 5)
        ) %>%
        config(displayModeBar = FALSE)
      
      return(empty_plot)
    }
    
    # Add hover text after confirming data exists
    outcomes_plot_data <- outcomes_plot_data %>%
      mutate(
        hover = sprintf(
          "Outcome Measure: <b>%s</b><br>Number of Studies: <b>%d</b>",
          processed_outcome_measure_roots, outcome_n
        )
      )
    
    outcomes_plot <- ggplot(outcomes_plot_data, aes(
      x = processed_outcome_measure_roots,
      y = outcome_n,
      fill = outcome_n,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, fill = NULL) +
      # Keep the order as arranged (highest count at top)
      scale_x_discrete(limits = rev)
    
    ggplotly(outcomes_plot, tooltip = "text") %>%
      layout(
        margin = list(l = 120, r = 5, t = 5, b = 5),  # Increased left margin for outcome names
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      ) %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  ###################################################################################
  ### Forest plot
  # NEW: Forest plot data filtered
  filtered_merged_forest <- reactive({
    # Get filtered merged
    filtered <- filtered_data()
    if (nrow(filtered) == 0) {
      # Return empty forest table with same columns as merged
      tibble(
        `Study Author Year` = character(),
        `Intervention` = character(),
        `Comparison` = character(),
        `Outcome Measure` = character(),
        `Weeks` = character(),
        `n` = numeric(),
        `SMD` = numeric(),
        lower = numeric(),
        upper = numeric(),
        processed_grades = list(),
        processed_school_types = list(),
        outcome_measure = character()
      )
    } else {
      se <- sqrt(filtered$vi)
      filtered$lower <- filtered$yi - 1.96 * se
      filtered$upper <- filtered$yi + 1.96 * se
      filtered_forest <- filtered %>%
        transmute(
          `Study Author Year` = study,
          `Intervention` = intervention,
          `Comparison` = comparison,
          `Outcome Measure` = processed_outcome_measure_roots,  # Use grouped version for display
          `Weeks` = as.character(outcome_timepoint),
          `n` = number_participants,
          `SMD` = round(yi, 3),
          lower = lower,
          upper = upper,
          processed_grades = processed_grades,  # Keep for tooltip
          processed_school_types = processed_school_types,  # Keep for tooltip
          outcome_measure = outcome_measure  # Keep specific measure for tooltip
        )
      filtered_forest
    }
  })
  
  # ----------- HIERARCHICAL BLANKING FUNCTION -----------
  hierarchical_blanker <- function(merged, group_cols) {
    n <- nrow(merged)
    if (n < 2) return(merged)
    original <- merged # keep original for comparison!
    
    # Add border indicator
    merged$border_top <- FALSE
    
    for (i in 2:n) {
      first_diff_col <- NA
      for (col_idx in seq_along(group_cols)) {
        col <- group_cols[col_idx]
        if (!identical(original[[col]][i], original[[col]][i-1])) {
          first_diff_col <- col_idx
          break
        }
      }
      
      # Check if Study Author Year changed (first column)
      if (!is.na(first_diff_col) && first_diff_col == 1) {
        merged$border_top[i] <- TRUE
      }
      
      if (is.na(first_diff_col)) {
        for (col_idx in seq_along(group_cols)) {
          col <- group_cols[col_idx]
          merged[[col]][i] <- ""
        }
      } else {
        if (first_diff_col > 1) {
          for (col_idx in 1:(first_diff_col-1)) {
            col <- group_cols[col_idx]
            merged[[col]][i] <- ""
          }
        }
        # Changed column and those to the right: keep as is
      }
    }
    merged
  }
  
  # ----------- FUNCTION TO ESTIMATE ROW HEIGHT BASED ON TEXT CONTENT -----------
  estimate_row_height <- function(merged_forest) {
    if (nrow(merged_forest) == 0) return(70)
    
    # Function to estimate text height based on character count and column width
    estimate_text_height <- function(text, col_width_chars, base_font_size = 13) {
      if (is.na(text) || text == "") return(1)
      
      char_count <- nchar(as.character(text))
      # Estimate characters per line based on column width
      chars_per_line <- max(1, floor(col_width_chars * 0.8))  # Conservative estimate
      lines_needed <- ceiling(char_count / chars_per_line)
      
      # Add extra lines for natural word wrapping
      if (grepl(" ", text) && char_count > chars_per_line) {
        lines_needed <- lines_needed + 1
      }
      
      return(lines_needed)
    }
    
    # Column width estimates (in characters, roughly)
    col_widths <- list(
      "Study Author Year" = 25,
      "Intervention" = 30,
      "Comparison" = 25,
      "Outcome Measure" = 35,
      "Weeks" = 8,
      "SMD" = 8
    )
    
    max_lines_per_row <- numeric(nrow(merged_forest))
    
    for (i in 1:nrow(merged_forest)) {
      row_max_lines <- 1
      
      for (col_name in names(col_widths)) {
        if (col_name %in% names(merged_forest)) {
          text_lines <- estimate_text_height(
            merged_forest[[col_name]][i], 
            col_widths[[col_name]]
          )
          row_max_lines <- max(row_max_lines, text_lines)
        }
      }
      
      max_lines_per_row[i] <- row_max_lines
    }
    
    # Calculate height: base height + (extra lines * line height)
    base_height <- 8
    line_height <- 11
    max_lines <- max(max_lines_per_row)
    
    return(base_height + ((max_lines - 1) * line_height))
  }
  
  # ----------- FOREST SVG FUNCTION WITH STYLED HOVER WRAPPER -----------
  # ----------- FOREST SVG FUNCTION WITH FLOATING TOOLTIP -----------
  make_forest_svg <- function(yi, lower, upper, n, max_n = NULL, row_height = 70, show_axis = FALSE, tooltip_html = NULL) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 500
    svg_height <- if (show_axis) 160 else row_height
    center_y <- svg_height / 2
    
    bubble_radius <- function(n, min_r = 2, max_r = 8) {
      if (is.na(n) || n <= 0 || is.na(max_n) || max_n == 0) return(min_r)
      prop <- sqrt(n / max_n)
      r <- min_r + (max_r - min_r) * prop
      return(r)
    }
    r <- bubble_radius(n)
    
    scale <- function(x) ref_width * (x - min_x) / (max_x - min_x)
    
    axis_svg <- if (show_axis) {
      axis_y <- 60
      tick_label_y <- 90
      axis_label_y <- 135
      
      ticks <- seq(-3, 3, by = 1)
      tick_x <- scale(ticks)
      axis_g <- htmltools::tagList(
        htmltools::tags$line(
          x1 = scale(min_x), x2 = scale(max_x), y1 = axis_y, y2 = axis_y,
          stroke = "#444", "stroke-width" = 2
        ),
        lapply(seq_along(tick_x), function(i) {
          htmltools::tags$g(
            htmltools::tags$line(
              x1 = tick_x[i], x2 = tick_x[i], y1 = axis_y, y2 = axis_y + 15,
              stroke = "#444", "stroke-width" = 2
            ),
            htmltools::tags$text(
              x = tick_x[i], y = tick_label_y,
              text.anchor = "middle", font.size = 18, font.family = "Arial",
              fill = "#333", font.weight = "bold", ticks[i]
            )
          )
        }),
        htmltools::tags$text(
          x = ref_width / 2, y = axis_label_y,
          "Standardized Mean Difference",
          font.size = 20, font.family = "Arial", text.anchor = "middle",
          fill = "#333", font.weight = "bold"
        )
      )
      htmltools::tags$g(axis_g)
    } else {
      NULL
    }
    
    # Initialize forest_geom as NULL first, then conditionally populate
    forest_geom <- NULL
    if (!is.na(yi) && !is.na(lower) && !is.na(upper) && !is.na(n)) {
      forest_geom <- list(
        # Confidence interval line
        htmltools::tags$line(
          x1 = scale(lower), x2 = scale(upper), y1 = center_y, y2 = center_y,
          stroke = "#333", "stroke-width" = 2
        ),
        # Point estimate circle
        htmltools::tags$circle(
          cx = scale(yi), cy = center_y, r = r,
          fill = ifelse(yi < -0.03, "#235223",        # green if less than -0.03
                        ifelse(yi > 0.03, "#E0C311",  # yellow if greater than 0.03
                               "#B0B0B0")),           # grey if between
          stroke = "#222", "stroke-width" = 1
        )
      )
    }
    
    # Create the SVG content
    svg_content <- as.character(
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
    
    # Wrap with floating tooltip if provided
    if (!is.null(tooltip_html) && tooltip_html != "") {
      return(sprintf(
        '<div class="forest-tooltip" style="cursor: pointer; height: 100%%; position: relative;">
        %s
        <div class="tooltip-content" id="tooltip-%s">%s</div>
      </div>', 
        svg_content, 
        sample(1:999999, 1), # Random ID to avoid conflicts
        tooltip_html
      ))
    } else {
      return(svg_content)
    }
  }
  
  # ----------- FINAL ASSEMBLY & REACTABLE OUTPUT FOR FILTERED FOREST -----------
  output$forest_tbl <- renderReactable({
    merged_forest <- filtered_merged_forest()
    max_n <- GLOBAL_MAX_N 
    
    if (nrow(merged_forest) == 0) {
      # Empty case - create placeholder
      merged_forest <- tibble(
        `Study Author Year` = "",
        `Intervention` = "",
        `Comparison` = "",
        `Outcome Measure` = "",
        `Weeks` = "",
        `SMD` = "",
        ` ` = list(""),
        border_top = FALSE
      )
      row_height <- 30
    } else {
      # SINGLE ROW PROTECTION: Add border_top column first, before any operations
      merged_forest$border_top <- FALSE
      
      # Sort only if more than 1 row
      group_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Measure")
      if (nrow(merged_forest) > 1) {
        merged_forest <- merged_forest[do.call(order, merged_forest[group_cols]), ]
      }
      
      # CRITICAL: Store original data BEFORE hierarchical blanking
      original_forest_pre_blank <- merged_forest
      
      # Apply hierarchical blanking (safe for single rows)
      merged_forest <- hierarchical_blanker(merged_forest, group_cols)
      
      # Calculate row height
      row_height <- estimate_row_height(merged_forest)
      
      # Create HTML tooltip text using ORIGINAL data (before blanking)
      tooltip_html_texts <- sapply(1:nrow(merged_forest), function(i) {
        # Use original data before blanking for tooltip content
        orig_data <- original_forest_pre_blank[i, ]
        
        if (is.na(orig_data$SMD) || orig_data$SMD == "" || is.na(orig_data$n)) {
          return("")
        }
        
        # Calculate effect size
        smd_val <- as.numeric(orig_data$SMD)
        effect_size <- if (abs(smd_val) >= 2) {
          "Very Large"
        } else if (abs(smd_val) >= 1) {
          "Large"
        } else if (abs(smd_val) >= 0.5) {
          "Medium"
        } else if (abs(smd_val) >= 0.2) {
          "Small"
        } else {
          "Very Small/Null"
        }
        
        # Format grades for display
        grades_list <- orig_data$processed_grades[[1]]
        grades_display <- if (is.null(grades_list) || length(grades_list) == 0) {
          "Not specified"
        } else {
          paste(grades_list, collapse = ", ")
        }
        
        # Format school types for display
        school_types_list <- orig_data$processed_school_types[[1]]
        school_types_display <- if (is.null(school_types_list) || length(school_types_list) == 0) {
          "Not specified"
        } else {
          paste(school_types_list, collapse = ", ")
        }
        
        # Get specific outcome measure (not the grouped version)
        specific_outcome <- if (is.na(orig_data$outcome_measure) || orig_data$outcome_measure == "") {
          "Not specified"
        } else {
          orig_data$outcome_measure
        }
        
        # Create HTML tooltip with bolded values - using original (non-blanked) data
        paste(
          paste0("Study: <b>", orig_data$`Study Author Year`, "</b>"),
          paste0("Intervention: <b>", orig_data$Intervention, "</b>"),
          paste0("Comparison (group): <b>", orig_data$Comparison, "</b>"),
          paste0("Outcome Measure: <b>", specific_outcome, "</b>"),
          paste0("Individual Grades: <b>", grades_display, "</b>"),
          paste0("School Type: <b>", school_types_display, "</b>"),
          paste0("Weeks: <b>", orig_data$Weeks, "</b>"),
          paste0("SMD: <b>", orig_data$SMD, "</b>"),
          paste0("n: <b>", orig_data$n, "</b>"),
          paste0("Effect size: <b>", effect_size, "</b>"),
          sep = "<br>"
        )
      })
      
      # CRITICAL: Always create SVG as list-column, protect against simplification
      svg_list <- mapply(
        make_forest_svg,
        merged_forest$SMD,
        merged_forest$lower,
        merged_forest$upper,
        merged_forest$n,
        max_n,
        row_height,
        FALSE,
        tooltip_html_texts,  # Pass HTML tooltip text based on original data
        SIMPLIFY = FALSE  # NEVER simplify to avoid vector conversion
      )
      
      # Force to list if it isn't already (extra protection)
      merged_forest$` ` <- as.list(svg_list)
      
      # Convert SMD to character
      merged_forest$SMD <- as.character(merged_forest$SMD)
      
      # SAFE SELECT: Ensure we keep all required columns for display
      display_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Measure", "Weeks", "SMD", " ", "border_top")
      merged_forest <- merged_forest[, display_cols, drop = FALSE]
      
      # Ensure it's still a tibble
      merged_forest <- tibble::as_tibble(merged_forest)
    }
    
    # Create axis footer with matching column structure
    forest_axis_footer <- tibble(
      `Study Author Year` = "",
      `Intervention` = "",
      `Comparison` = "",
      `Outcome Measure` = "",
      `Weeks` = "",
      `SMD` = "",
      ` ` = list(make_forest_svg(NA, NA, NA, NA, max_n, 160, show_axis = TRUE, tooltip_html = NULL)),
      border_top = TRUE
    )
    
    # Combine rows - both should have identical column structure now
    merged_forest <- dplyr::bind_rows(merged_forest, forest_axis_footer)
    
    # Final safety check
    if (nrow(merged_forest) == 0) {
      merged_forest <- tibble(
        `Study Author Year` = "",
        `Intervention` = "",
        `Comparison` = "",
        `Outcome Measure` = "",
        `Weeks` = "",
        `SMD` = "",
        ` ` = list(""),
        border_top = FALSE
      )
    }
    
    reactable(
      merged_forest,
      columns = list(
        `Study Author Year` = colDef(
          name = "Study Author Year",
          minWidth = 180,
          sortable = FALSE,  # Disable sorting
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        `Intervention` = colDef(
          name = "Intervention",
          minWidth = 200,
          sortable = FALSE,  # Disable sorting
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        `Comparison` = colDef(
          name = "Comparison",
          minWidth = 150,
          sortable = FALSE,  # Disable sorting
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        `Outcome Measure` = colDef(
          name = "Outcome Measure",
          minWidth = 220,
          sortable = FALSE,  # Disable sorting
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        `Weeks` = colDef(
          name = "Weeks",
          minWidth = 80,
          sortable = FALSE,  
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        `SMD` = colDef(
          name = "SMD",
          minWidth = 80,
          sortable = FALSE,  
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        ` ` = colDef(
          name = " ",
          html = TRUE,
          minWidth = 500,
          sortable = FALSE,  # Disable sorting on forest plot column
          style = function(value, index) {
            style_list <- list()
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333"
          )
        ),
        border_top = colDef(show = FALSE)
      ),
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      style = list(
        fontFamily = "Arial, sans-serif",
        fontSize = "13px",
        height = "75vh", 
        overflowY = "auto"
      ),
      rowStyle = function(index) {
        is_axis_row <- index == nrow(merged_forest)
        if (is_axis_row) {
          list(height = "160px")
        } else {
          list(height = paste0(row_height, "px"))
        }
      },
      fullWidth = TRUE,
      defaultPageSize = nrow(merged_forest)
    )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

