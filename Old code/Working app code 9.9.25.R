# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi, shinyWidgets, shinyjs)

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

# For each duplicated, keep the .studies version, rename it to the base, and drop the .df version
for (col in dup_bases) {
  merged[[col]] <- merged[[paste0(col, ".studies")]]
}

# Remove all .df and .studies columns (now redundant), except the renamed ones
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
# OUTCOME TYPE FILTER (based on Excel sheet tabs)
tryCatch({
  available_outcome_types <- openxlsx::getSheetNames(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"))
  outcome_type_choices <- available_outcome_types[!grepl("^(Sheet|Feuil)", available_outcome_types)]
  cat("Successfully created outcome_type_choices:", paste(outcome_type_choices, collapse = ", "), "\n")
}, error = function(e) {
  cat("Error creating outcome_type_choices:", e$message, "\n")
  outcome_type_choices <- c("Depression Symptoms")  # Fallback
})

###########################
# Urbanicity creation (MOVED BEFORE intervention processing)
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
# Grade level processing
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

###########################
# School type processing
process_school_types <- function(school_type_string) {
  if (is.na(school_type_string) || tolower(trimws(school_type_string)) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  
  # Clean and split the school type string
  school_types_clean <- gsub("[\r\n]+", ",", school_type_string)
  school_types_split <- unlist(strsplit(school_types_clean, "[,;]+"))
  school_types_split <- trimws(school_types_split)
  
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
process_outcome_measures_roots <- function(all_outcome_measures) {
  # Remove NA values and empty strings for processing
  clean_measures <- all_outcome_measures[!is.na(all_outcome_measures) & trimws(all_outcome_measures) != ""]
  
  if (length(clean_measures) == 0) return(rep("Other/Unclear", length(all_outcome_measures)))
  
  # Function to extract meaningful word combinations
  extract_meaningful_parts <- function(measure) {
    # Remove parentheses content
    clean_measure <- gsub("\\([^)]*\\)", "", measure)
    
    # Remove common suffixes and modifiers
    clean_measure <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*Short\\s+Version.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Revised.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-2.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-Youth.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-II.*$", "", clean_measure)
    clean_measure <- gsub("\\s*for\\s+Children.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Depression\\s+Sub-Scale.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Depression\\s+Subscale.*$", "", clean_measure)
    clean_measure <- gsub("-9$", "", clean_measure)
    
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
      if (length(root_groups[[root]]) == 1) {
        original <- clean_measures[clean_idx]
        cleaned_original <- gsub("\\([^)]*\\).*$", "", original)
        cleaned_original <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", cleaned_original)
        cleaned_original <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", cleaned_original)
        cleaned_original <- gsub("-9$", "", cleaned_original)
        cleaned_original <- trimws(cleaned_original)
        result[i] <- cleaned_original
      } else {
        result[i] <- root
      }
      clean_idx <- clean_idx + 1
    }
  }
  
  return(result)
}

# Apply outcome processing to merged data
processed_outcomes_roots <- process_outcome_measures_roots(merged$outcome_measure)
merged <- merged %>%
  mutate(processed_outcome_measure_roots = processed_outcomes_roots)

# Create outcome measure family choices
outcome_measure_family_choices <- merged %>%
  distinct(processed_outcome_measure_roots) %>%
  pull(processed_outcome_measure_roots) %>%
  sort()

# Helper function for outcome measures
outcome_measures_match_filter_roots <- function(processed_outcome, selected_outcomes) {
  return(processed_outcome %in% selected_outcomes)
}

###########################
# INTERVENTION TYPE PROCESSING - FIXED ORDER
process_intervention_families <- function(all_interventions) {
  # Remove NA values and empty strings for processing
  clean_interventions <- all_interventions[!is.na(all_interventions) & trimws(all_interventions) != ""]
  
  if (length(clean_interventions) == 0) return(rep("Other/Unclear", length(all_interventions)))
  
  # Function to extract meaningful intervention families
  extract_intervention_family <- function(intervention) {
    # Convert to lowercase for processing
    clean_intervention <- tolower(trimws(intervention))
    
    # Define intervention family mappings
    if (grepl("cbt|cognitive.*behav|rational.*emotive", clean_intervention)) {
      return("CBT/Cognitive-Behavioral")
    } else if (grepl("mindfulness|meditation|mbsr|mindful", clean_intervention)) {
      return("Mindfulness-Based")
    } else if (grepl("interpersonal|ipt|social.*skill", clean_intervention)) {
      return("Interpersonal/Social Skills")
    } else if (grepl("prevention|universal|selective", clean_intervention)) {
      return("Prevention Programs")
    } else if (grepl("group|peer|support.*group", clean_intervention)) {
      return("Group-Based")
    } else if (grepl("family|parent|caregiver", clean_intervention)) {
      return("Family-Based")
    } else if (grepl("physical.*activity|exercise|sport", clean_intervention)) {
      return("Physical Activity")
    } else if (grepl("art|music|creative|expressive", clean_intervention)) {
      return("Creative/Expressive")
    } else if (grepl("psychoeducation|education|psycho.*education", clean_intervention)) {
      return("Psychoeducational")
    } else if (grepl("resilience|strength|positive.*psychology", clean_intervention)) {
      return("Resilience/Strengths-Based")
    } else {
      # For interventions that don't fit categories, use cleaned original name
      cleaned_original <- gsub("\\s*-\\s*(program|intervention|therapy|treatment).*$", "", intervention, ignore.case = TRUE)
      cleaned_original <- gsub("\\([^)]*\\)", "", cleaned_original)
      cleaned_original <- trimws(cleaned_original)
      return(cleaned_original)
    }
  }
  
  # Extract families for all interventions
  families <- sapply(clean_interventions, extract_intervention_family, USE.NAMES = FALSE)
  
  # Create result mapping
  result <- character(length(all_interventions))
  clean_idx <- 1
  
  for (i in seq_along(all_interventions)) {
    if (is.na(all_interventions[i]) || trimws(all_interventions[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      result[i] <- families[clean_idx]
      clean_idx <- clean_idx + 1
    }
  }
  
  return(result)
}

# Apply intervention family processing to merged data
processed_intervention_families <- process_intervention_families(merged$intervention)
merged <- merged %>%
  mutate(processed_intervention_families = processed_intervention_families)

# Create intervention family choices
intervention_family_choices <- merged %>%
  distinct(processed_intervention_families) %>%
  pull(processed_intervention_families) %>%
  sort()

# Debug intervention families
cat("=== INTERVENTION FAMILIES DEBUG ===\n")
cat("Number of intervention families:", length(intervention_family_choices), "\n")
cat("Intervention families:\n")
for(i in seq_along(intervention_family_choices)) {
  cat(i, ":", intervention_family_choices[i], "\n")
}
cat("=== END DEBUG ===\n")
# Helper function to check if a study's intervention families match selected filters
intervention_families_match_filter <- function(processed_intervention, selected_interventions) {
  return(processed_intervention %in% selected_interventions)
}

# Debug output
cat("=== DEBUGGING FILTER VARIABLES ===\n")
cat("outcome_type_choices:", paste(outcome_type_choices, collapse = ", "), "\n")
cat("intervention_family_choices:", paste(intervention_family_choices, collapse = ", "), "\n")
cat("=== END DEBUG ===\n")

###########################
# Grade level creation - Updated scheme
classify_grade_level <- function(x) {
  x <- gsub(" ", "", x)
  if (tolower(x) %in% c("cannot tell", "unclear", "")) return("Unclear")
  
  # Handle kindergarten - convert K to 0
  x <- gsub("K", "0", x, ignore.case = TRUE)
  
  grades_split <- unlist(strsplit(x, ","))
  if (any(is.na(suppressWarnings(as.numeric(grades_split))))) return("Unclear")
  
  grades_num <- as.numeric(grades_split)
  min_g <- min(grades_num)
  max_g <- max(grades_num)
  
  # New classification scheme
  if (min_g >= 0 && max_g <= 8) return("K-8")
  if (min_g >= 9 && max_g <= 12) return("9-12")
  if (min_g <= 8 && max_g >= 9) return("K-12")
  
  return("Unclear")
}

# School level creation - Updated choices
school_level_choices <- merged %>%
  mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
  distinct(grade_category) %>%
  pull(grade_category) %>%
  na.omit() %>%
  unique() %>%
  as.character()

# Order them logically:
school_level_choices <- intersect(
  c("K-8", "9-12", "K-12", "Unclear"),
  school_level_choices
)

###########################
# REACTIVE DATA FUNCTION FOR OUTCOME TYPE SWITCHING
load_outcome_data <- function(outcome_type) {
  if (outcome_type == "Depression Symptoms") {
    return(merged)
  } else {
    tryCatch({
      df_new <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which = outcome_type)
      df_new <- df_new %>% mutate(study_std = standardize(study))
      
      merged_new <- df_new %>%
        left_join(
          studies,
          by = c("study_std" = "study_author_year_std"),
          suffix = c(".df", ".studies")
        )
      
      dup_bases <- intersect(
        gsub("\\.df$", "", names(merged_new)[endsWith(names(merged_new), ".df")]),
        gsub("\\.studies$", "", names(merged_new)[endsWith(names(merged_new), ".studies")])
      )
      
      for (col in dup_bases) {
        merged_new[[col]] <- merged_new[[paste0(col, ".studies")]]
      }
      
      merged_new <- merged_new %>% 
        select(-matches("\\.df$"), -matches("\\.studies$")) %>%
        mutate(
          urbanicity_clean = sapply(urbanicity, clean_urbanicity),
          processed_grades = sapply(grade_level, process_grade_levels),
          processed_school_types = sapply(school_type, process_school_types),
          processed_intervention_families = process_intervention_families(intervention),
          processed_outcome_measure_roots = process_outcome_measures_roots(outcome_measure)
        )
      
      return(merged_new)
    }, error = function(e) {
      return(merged)
    })
  }
}

#############################################################################
# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  
  # Application title
  fluidRow(
    column(12,
           div(class = "title-panel", "School-based Interventions to Reduce Depression")
    )
  ),
  
  ### Filter dropdowns - REORGANIZED INTO 2 ROWS
  ### Filter dropdowns - 2 rows, 4 filters each
  fluidRow(
    # Count of studies on the left
    column(2,
           uiOutput("studies_panel")
    ),
    
    # Filter area in the middle (8 columns)
    column(8,
           # First row of filters
           # First row of filters
           fluidRow(
             column(3,
                    pickerInput(
                      inputId = "country_filter",
                      label = "Country",
                      choices = sort(unique(merged$country)),
                      selected = sort(unique(merged$country)),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "school_level_filter",
                      label = "School Level",
                      choices = school_level_choices,
                      selected = school_level_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "urbanicity_filter",
                      label = "Urbanicity",
                      choices = urbanicity_choices,
                      selected = urbanicity_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "intervention_family_filter",
                      label = "Intervention Family",
                      choices = character(0),
                      selected = character(0),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             )
           ),
           
           # Second row of filters
           # Second row of filters  
           fluidRow(
             column(3,
                    pickerInput(
                      inputId = "individual_grade_filter",
                      label = "Individual Grades",
                      choices = individual_grade_choices,
                      selected = individual_grade_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "individual_school_type_filter",
                      label = "School Type",
                      choices = individual_school_type_choices,
                      selected = individual_school_type_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "outcome_family_filter",
                      label = "Outcome Measure Family",
                      choices = if (length(outcome_measure_family_choices) > 0) outcome_measure_family_choices else "Other/Unclear",
                      selected = if (length(outcome_measure_family_choices) > 0) outcome_measure_family_choices else "Other/Unclear",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    pickerInput(
                      inputId = "outcome_type_filter",
                      label = "Outcome Type",
                      choices = if (length(outcome_type_choices) > 0) outcome_type_choices else "Depression Symptoms",
                      selected = "Depression Symptoms",
                      multiple = FALSE,
                      options = list(`live-search` = TRUE),
                      width = "100%"
                    )
             )
           )
    ),
    
    # Reset button on the right (2 columns)
    column(2,
           div(style = "margin-top: 22px;",
               actionButton("reset_filters", "Clear All Filters", 
                            class = "btn-warning btn-sm", 
                            icon = icon("times-circle"),
                            style = "background-color: #8ABB40 ; border-color: #8ABB40 ; color: white; font-size: 18px; padding: 15px 8px; white-space: nowrap; width: 90%; margin-left: -20px; height: 60px; display: flex; align-items: center; justify-content: center;")
           ),
           div(style = "margin-top: 10px;",
               actionButton("toggle_hover", "Hover Information: ON", 
                            class = "btn-info btn-sm", 
                            icon = icon("info-circle"),
                            style = "background-color: #007030 ; border-color: #007030 ; color: white; font-size: 14px; padding: 10px 5px; white-space: nowrap; width: 90%; margin-left: -20px; height: 45px; display: flex; align-items: center; justify-content: center;")
           ),
           # Add CSS for hover control
           tags$head(tags$style(HTML("
       #reset_filters i { margin-right: 8px !important; }
       #toggle_hover i { margin-right: 8px !important; }
       
       /* Global hover control */
       .hover-disabled .forest-tooltip,
       .hover-disabled [data-toggle='tooltip'],
       .hover-disabled .plotly,
       .hover-disabled svg {
         pointer-events: none !important;
       }
       
       .hover-disabled .plotly .hovertext {
         display: none !important;
       }
     ")))
    )
  ),
  
  # Rest of your UI (tabs) stays the same...
  tabsetPanel(
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
    ),
    
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
                      plotlyOutput("outcomes_graph", height = "350px"))
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
  
  # Add reactive value for hover state (add this in your server)
  hover_enabled <- reactiveVal(TRUE)
  
  # Enhanced hover toggle observer that affects all visualizations
  observeEvent(input$toggle_hover, {
    current_state <- hover_enabled()
    hover_enabled(!current_state)
    if (hover_enabled()) {
      updateActionButton(session, "toggle_hover", label = "Hover Information: ON", icon = icon("info-circle"))
      shinyjs::runjs("$('body').removeClass('hover-disabled');")
    } else {
      updateActionButton(session, "toggle_hover", label = "Hover Information: OFF", icon = icon("info-circle"))
      shinyjs::runjs("$('body').addClass('hover-disabled');")
    }
  })
  # Initialize intervention filter on app start
observe({
  current_data <- load_outcome_data("Depression Symptoms")
  
  initial_intervention_choices <- current_data %>%
    distinct(processed_intervention_families) %>%
    pull(processed_intervention_families) %>%
    sort()
  
  updatePickerInput(
    session, 
    "intervention_family_filter",
    choices = initial_intervention_choices,
    selected = initial_intervention_choices
  )
})

# Update intervention filter when outcome type changes
observeEvent(input$outcome_type_filter, {
  current_data <- load_outcome_data(input$outcome_type_filter)
  
  new_intervention_choices <- current_data %>%
    distinct(processed_intervention_families) %>%
    pull(processed_intervention_families) %>%
    sort()
  
  updatePickerInput(
    session, 
    "intervention_family_filter",
    choices = new_intervention_choices,
    selected = new_intervention_choices
  )
})
# Initialize intervention filter on app start
observe({
  current_data <- load_outcome_data("Depression Symptoms")
  
  initial_intervention_choices <- current_data %>%
    distinct(processed_intervention_families) %>%
    pull(processed_intervention_families) %>%
    sort()
  
  updatePickerInput(
    session, 
    "intervention_family_filter",
    choices = initial_intervention_choices,
    selected = initial_intervention_choices
  )
})

# Update intervention filter when outcome type changes
observeEvent(input$outcome_type_filter, {
  current_data <- load_outcome_data(input$outcome_type_filter)
  
  new_intervention_choices <- current_data %>%
    distinct(processed_intervention_families) %>%
    pull(processed_intervention_families) %>%
    sort()
  
  updatePickerInput(
    session, 
    "intervention_family_filter",
    choices = new_intervention_choices,
    selected = new_intervention_choices
  )
})
  # Updated reset button observer (make sure this includes ALL filters)
observeEvent(input$reset_filters, {
  current_data <- load_outcome_data(input$outcome_type_filter)
  
  # Get current intervention choices for the selected outcome type
  current_intervention_choices <- current_data %>%
    distinct(processed_intervention_families) %>%
    pull(processed_intervention_families) %>%
    sort()
  
  # Reset all filters (keeping your existing code but updating intervention part)
  updatePickerInput(session, "country_filter", 
                    selected = sort(unique(merged$country)))
  updatePickerInput(session, "school_level_filter", 
                    selected = school_level_choices)
  updatePickerInput(session, "urbanicity_filter", 
                    selected = urbanicity_choices)
  updatePickerInput(session, "individual_grade_filter", 
                    selected = individual_grade_choices)
  updatePickerInput(session, "individual_school_type_filter", 
                    selected = individual_school_type_choices)
  updatePickerInput(session, "outcome_family_filter", 
                    selected = outcome_measure_family_choices)
  updatePickerInput(session, "outcome_type_filter", 
                    selected = "Depression Symptoms")
  updatePickerInput(session, "intervention_family_filter", 
                    selected = current_intervention_choices)  # Updated this line
})
  
  
  #Filters
  # Standard filtered data reactive
  filtered_data <- reactive({
    # Load correct dataset depending on outcome type selection
    data <- load_outcome_data(input$outcome_type_filter)
    
    # If ANY filter is empty, return empty df
    if (
      is.null(input$country_filter) || length(input$country_filter) == 0 ||
      is.null(input$school_level_filter) || length(input$school_level_filter) == 0 ||
      is.null(input$urbanicity_filter) || length(input$urbanicity_filter) == 0 ||
      is.null(input$individual_grade_filter) || length(input$individual_grade_filter) == 0 ||
      is.null(input$individual_school_type_filter) || length(input$individual_school_type_filter) == 0 ||
      is.null(input$outcome_family_filter) || length(input$outcome_family_filter) == 0 ||
      is.null(input$intervention_family_filter) || length(input$intervention_family_filter) == 0
    ) {
      return(data[0,]) # empty df with correct columns
    }
    
    data <- data %>%
      mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
      filter(country %in% input$country_filter) %>%
      filter(grade_category %in% input$school_level_filter) %>%
      filter(urbanicity_clean %in% input$urbanicity_filter) %>%
      filter(sapply(processed_grades, function(x) grades_match_filter(x, input$individual_grade_filter))) %>%
      filter(sapply(processed_school_types, function(x) school_types_match_filter(x, input$individual_school_type_filter))) %>%
      filter(sapply(processed_outcome_measure_roots, function(x) outcome_measures_match_filter_roots(x, input$outcome_family_filter))) %>%
      filter(sapply(processed_intervention_families, function(x) intervention_families_match_filter(x, input$intervention_family_filter)))
    
    return(data)
  })
  
  
  # Improved no data plot function with better text wrapping and sizing
  create_compact_no_data_plot <- function() {
    plot_ly() %>%
      add_annotations(
        text = "No studies meet<br>selected filters",  # Shorter message
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          size = 11,
          color = "#666666",
          family = "Arial, sans-serif"
        ),
        align = "center",
        xanchor = "center",
        yanchor = "middle"
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          range = c(0, 1),
          fixedrange = TRUE
        ),
        yaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          range = c(0, 1),
          fixedrange = TRUE
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 5, r = 5, t = 5, b = 5),
        autosize = TRUE,
        showlegend = FALSE
      ) %>%
      config(
        displayModeBar = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        staticPlot = TRUE,
        responsive = TRUE
      )
  }
  
  ###########################################################
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
  
  ###########################################################################  
  # Count studies per country
  country_counts <- reactive({
    filtered_data() %>%
      distinct(study, country) %>%
      count(country)
  })
  
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
        displayModeBar = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE
      )
  })
  
  ########################################################
  output$school_level <- renderPlotly({
    data <- filtered_data()
    if (!"grade_category" %in% names(data) || nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    else {
      school_level_plot_data <- data %>%
        distinct(study, grade_category) %>%
        mutate(
          grade_category = factor(
            grade_category,
            levels = rev(c("K-8", "9-12", "K-12", "Unclear"))
          )
        ) %>%
        group_by(grade_category) %>%
        summarise(grade_n = n(), .groups = "drop") %>%
        mutate(
          selected = as.character(grade_category) %in% input$school_level_filter,
          fill_color = case_when(
            selected & grade_n <= 5 ~ "#8ABB40",
            selected & grade_n <= 10 ~ "#489D46",
            selected & grade_n <= 15 ~ "#007030",
            selected ~ "#104735",
            !selected ~ "#CCCCCC60"
          ),
          hover_text = paste0(
            "Grade Level: <b>", grade_category, "</b><br>",
            "Number of Studies: <b>", grade_n, "</b>"
          )
        )
      
      p <- ggplot(school_level_plot_data, aes(
        x = grade_category,
        y = grade_n,
        fill = I(fill_color),
        text = hover_text
      )) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        labs(x = NULL, y = NULL)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 5, r = 5, t = 5, b = 90),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    }
  })
  
  ################################################
  # Urbanicity graph
  output$urbanicity <- renderPlotly({
    data <- filtered_data()
    if (!"urbanicity_clean" %in% names(data) || nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    else {
      urbanicity_plot_data <- data %>%
        mutate(urbanicity_clean = ifelse(is.na(urbanicity_clean), "Unclear", urbanicity_clean)) %>%
        distinct(study, urbanicity_clean) %>%
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
        filter(urbanicity_n > 0) %>%
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
        scale_fill_gradientn(colors = c("#8ABB40", "#489D46", "#007030", "#104735")) +
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_x_discrete(limits = rev)
      
      ggplotly(urbanicity_plot, tooltip = "text") %>%
        layout(
          margin = list(l = 90, r = 5, t = 5, b = 5),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    }
  })
  
  ################################################################################################
  # Number of Schools Graph - UPDATED
  output$num_schools_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(number_schools = ifelse(number_schools == -999, NA, number_schools)) %>%
      filter(!is.na(number_schools)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Schools: <b>", number_schools, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(123)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(number_schools) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(number_schools, number_schools, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        number_schools_adj = number_schools + y_offset
      )
    
    mean_schools <- round(mean(studies_clean$number_schools), 1)
    y_range <- range(studies_clean$number_schools_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_schools, mean_schools),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_schools),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~number_schools_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Schools"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_schools, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ##########################################################################################
  # Number of classrooms graph - UPDATED
  output$num_class_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(number_classrooms = ifelse(number_classrooms == -999, NA, number_classrooms)) %>%
      filter(!is.na(number_classrooms)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Classrooms: <b>", number_classrooms, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(456)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(number_classrooms) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(number_classrooms, number_classrooms, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        number_classrooms_adj = number_classrooms + y_offset
      )
    
    mean_classrooms <- round(mean(studies_clean$number_classrooms), 1)
    y_range <- range(studies_clean$number_classrooms_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_classrooms, mean_classrooms),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_classrooms),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~number_classrooms_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Classrooms"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_classrooms, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ############################################################################
  # Number of students plot - UPDATED
  output$num_students_tile <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(number_participants = ifelse(number_participants == -999, NA, number_participants)) %>%
      filter(!is.na(number_participants) & number_participants > 0) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        label = paste0(study_author_year, "\nn=", number_participants),
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Students: <b>", number_participants, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    plot_ly(
      data = studies_clean,
      type = "treemap",
      labels = ~label,
      values = ~number_participants,
      parents = NA,
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
        pathbar = list(visible = FALSE),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var plotDiv = el;
      plotDiv.on('plotly_treemapclick', function(eventData) {
        return false;
      });
    }
  ")
  })
  
  ##################################################################################
  # Average age graph - UPDATED
  output$avg_age <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(average_age = ifelse(average_age == -999, NA, average_age)) %>%
      filter(!is.na(average_age)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Average Age: <b>", average_age, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(789)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(average_age) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(average_age, average_age, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        average_age_adj = average_age + y_offset
      )
    
    mean_age <- round(mean(studies_clean$average_age), 1)
    y_range <- range(studies_clean$average_age_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_age, mean_age),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_age),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~average_age_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Age, years"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_age, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ##################################################################################
  # Female graph - UPDATED
  output$pct_fem <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(percent_female = ifelse(percent_female == -999, NA, percent_female)) %>%
      filter(!is.na(percent_female)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(percent_female = percent_female * 100) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent Female: <b>", percent_female, "%", "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(101)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(percent_female) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(percent_female, percent_female, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        percent_female_adj = percent_female + y_offset
      )
    
    mean_female <- round(mean(studies_clean$percent_female), 1)
    y_limits <- c(0, 100)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_female, mean_female),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_female, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_female_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%")
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_female, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ###############################################
  # Race/ethnicity graph - UPDATED
  output$race_ethnicity_graph <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    race_plot_data <- data %>%
      group_by(study) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        percent_white = as.numeric(ifelse(percent_white == -999, NA, percent_white)),
        percent_black = as.numeric(ifelse(percent_black == -999, NA, percent_black)),
        percent_aian = as.numeric(ifelse(percent_aian == -999, NA, percent_aian)),
        percent_nhpi = as.numeric(ifelse(percent_nhpi == -999, NA, percent_nhpi)),
        percent_asian = as.numeric(ifelse(percent_asian == -999, NA, percent_asian)),
        percent_latinx = as.numeric(ifelse(percent_latinx == -999, NA, percent_latinx)),
        percent_other = as.numeric(ifelse(percent_other == -999, NA, percent_other))
      ) %>%
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
      pivot_longer(
        cols = everything(),
        names_to = "race_ethnicity",
        values_to = "proportion"
      ) %>%
      filter(!is.na(proportion) & is.finite(proportion)) %>%
      arrange(proportion) %>%
      mutate(
        race_ethnicity = factor(race_ethnicity, levels = race_ethnicity),
        hover = sprintf(
          "Race/Ethnicity: <b>%s</b><br>Average Proportion: <b>%.1f%%</b>",
          gsub("\n", " ", race_ethnicity), proportion * 100
        )
      )
    
    if (nrow(race_plot_data) == 0) {
      return(create_compact_no_data_plot())
    }
    
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
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  ###################################################################################
  # FRPL Graph - UPDATED
  output$frpl_graph <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>% 
      mutate(percent_FRPL = ifelse(percent_FRPL == -999, NA, percent_FRPL)) %>%
      filter(!is.na(percent_FRPL)) %>%
      mutate(percent_FRPL = percent_FRPL * 100) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent FRPL: <b>", round(percent_FRPL, 1), "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(202)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(percent_FRPL) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(percent_FRPL, percent_FRPL, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        percent_FRPL_adj = percent_FRPL + y_offset
      )
    
    mean_frpl <- round(mean(studies_clean$percent_FRPL), 1)
    y_range <- range(studies_clean$percent_FRPL_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_frpl, mean_frpl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_frpl, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_FRPL_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent FRPL (%)"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_frpl, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ###################################################################################
  # ELL Graph - UPDATED
  output$ell_graph <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>% 
      mutate(percent_ELL = ifelse(percent_ELL == -999, NA, percent_ELL)) %>%
      filter(!is.na(percent_ELL)) %>%
      filter(is.finite(percent_ELL)) %>%
      mutate(percent_ELL = percent_ELL * 100) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        percent_ELL_rounded = round(percent_ELL, 1),
        hover_text = paste0("Study: <b>", as.character(study_author_year), "</b><br>Percent ELL: <b>", percent_ELL_rounded, "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(303)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(percent_ELL) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(percent_ELL, percent_ELL, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        percent_ELL_adj = percent_ELL + y_offset
      )
    
    mean_ell <- round(mean(studies_clean$percent_ELL, na.rm = TRUE), 1)
    if (!is.finite(mean_ell)) mean_ell <- 0
    
    y_range <- range(studies_clean$percent_ELL_adj, na.rm = TRUE)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_ell, mean_ell),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = "text",
        text = paste0("Mean = ", mean_ell, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_ELL_adj,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent ELL (%)"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_ell, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  ###################################################################################
  # Outcomes graph - UPDATED
  output$outcomes_graph <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    outcomes_plot_data <- data %>%
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
      filter(outcome_n > 0) %>%
      arrange(desc(outcome_n)) %>%
      mutate(
        processed_outcome_measure_roots = factor(
          processed_outcome_measure_roots,
          levels = processed_outcome_measure_roots
        )
      )
    
    if (nrow(outcomes_plot_data) == 0) {
      return(create_compact_no_data_plot())
    }
    
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
      scale_x_discrete(limits = rev)
    
    ggplotly(outcomes_plot, tooltip = "text") %>%
      layout(
        margin = list(l = 120, r = 5, t = 5, b = 5),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  ###################################################################################
  ### Complete Forest Plot Code with All Functions Defined
  ###################################################################################
  
  # 1. HELPER FUNCTIONS (define these first)
  safe_format_list <- function(list_value, default_text = "Not specified") {
    tryCatch({
      if (is.null(list_value) || length(list_value) == 0) {
        return(default_text)
      }
      if (is.list(list_value) && length(list_value) == 1) {
        list_value <- list_value[[1]]
      }
      if (length(list_value) == 0 || all(is.na(list_value))) {
        return(default_text)
      }
      clean_values <- as.character(list_value[!is.na(list_value)])
      if (length(clean_values) == 0) {
        return(default_text)
      }
      return(paste(clean_values, collapse = ", "))
    }, error = function(e) {
      return(default_text)
    })
  }
  
  format_viz_value <- function(value, is_percentage = FALSE, default_text = "Not specified") {
    tryCatch({
      if (is.null(value) || length(value) == 0) {
        return(default_text)
      }
      if (is.na(value) || value == -999 || value == "" || value == "NA") {
        return(default_text)
      }
      if (is_percentage && is.numeric(value)) {
        return(paste0(round(value * 100, 1), "%"))
      }
      if (is.numeric(value)) {
        return(as.character(round(value, 1)))
      }
      return(as.character(value))
    }, error = function(e) {
      return(default_text)
    })
  }
  
  format_race_ethnicity_breakdown <- function(white, black, aian, nhpi, asian, latinx, other) {
    tryCatch({
      race_components <- c()
      if (!is.na(white) && white != -999 && white > 0) {
        race_components <- c(race_components, paste0("White: ", round(white * 100, 1), "%"))
      }
      if (!is.na(black) && black != -999 && black > 0) {
        race_components <- c(race_components, paste0("Black: ", round(black * 100, 1), "%"))
      }
      if (!is.na(aian) && aian != -999 && aian > 0) {
        race_components <- c(race_components, paste0("AIAN: ", round(aian * 100, 1), "%"))
      }
      if (!is.na(nhpi) && nhpi != -999 && nhpi > 0) {
        race_components <- c(race_components, paste0("NHPI: ", round(nhpi * 100, 1), "%"))
      }
      if (!is.na(asian) && asian != -999 && asian > 0) {
        race_components <- c(race_components, paste0("Asian: ", round(asian * 100, 1), "%"))
      }
      if (!is.na(latinx) && latinx != -999 && latinx > 0) {
        race_components <- c(race_components, paste0("Latinx: ", round(latinx * 100, 1), "%"))
      }
      if (!is.na(other) && other != -999 && other > 0) {
        race_components <- c(race_components, paste0("Other: ", round(other * 100, 1), "%"))
      }
      if (length(race_components) == 0) {
        return("Not specified")
      }
      return(paste(race_components, collapse = ", "))
    }, error = function(e) {
      return("Not specified")
    })
  }
  
  format_processed_grades <- function(processed_grades) {
    if (is.null(processed_grades) || is.na(processed_grades) || processed_grades == "" || processed_grades == "Unclear") {
      return("Not specified")
    }
    grades <- unlist(strsplit(as.character(processed_grades), ","))
    return(paste(grades, collapse = ", "))
  }
  # 
  # 2. HIERARCHICAL BLANKING FUNCTION
  hierarchical_blanker <- function(merged, group_cols) {
    n <- nrow(merged)
    if (n < 2) return(merged)
    original <- merged
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
      }
    }
    merged
  }
  
  # 3. ROW HEIGHT CALCULATION
  calculate_dynamic_row_height <- function(merged_forest) {
    if (nrow(merged_forest) == 0) return(80)
    
    text_complexity <- sapply(1:nrow(merged_forest), function(i) {
      text_lengths <- c(
        nchar(as.character(merged_forest$`Study Author Year`[i])),
        nchar(as.character(merged_forest$Intervention[i])),
        nchar(as.character(merged_forest$Comparison[i])),
        nchar(as.character(merged_forest$`Outcome Measure`[i]))
      )
      weighted_lengths <- text_lengths / c(20, 25, 20, 30)
      max(weighted_lengths, na.rm = TRUE)
    })
    
    base_height <- 80
    content_factor <- pmax(1, text_complexity)
    heights <- base_height + (content_factor - 1) * 25
    heights <- pmax(80, pmin(heights, 200))
    
    return(round(mean(heights)))
  }
  
  # 4. FOREST SVG FUNCTION (this must be defined before the output)
  make_forest_svg <- function(yi, lower, upper, n, max_n = NULL, row_height = 70, show_axis = FALSE, tooltip_html = NULL, hover_enabled = TRUE) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 500
    svg_height <- if (show_axis) 180 else row_height
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
    
    forest_geom <- NULL
    if (!is.na(yi) && !is.na(lower) && !is.na(upper) && !is.na(n)) {
      forest_geom <- list(
        htmltools::tags$line(
          x1 = scale(lower), x2 = scale(upper), y1 = center_y, y2 = center_y,
          stroke = "#333", "stroke-width" = 2
        ),
        htmltools::tags$circle(
          cx = scale(yi), cy = center_y, r = r,
          fill = ifelse(yi < -0.03, "#235223",
                        ifelse(yi > 0.03, "#E0C311", "#B0B0B0")),
          stroke = "#222", "stroke-width" = 1
        )
      )
    }
    
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
    
    if (!is.null(tooltip_html) && tooltip_html != "" && hover_enabled) {
      return(sprintf(
        '<div class="forest-tooltip" style="cursor: pointer; height: 100%%; position: relative;">
        %s
        <div class="tooltip-content" id="tooltip-%s">%s</div>
      </div>', 
        svg_content, 
        sample(1:999999, 1),
        tooltip_html
      ))
    } else {
      return(svg_content)
    }
  }
  
  # 5. FILTERED DATA REACTIVE
  filtered_merged_forest <- reactive({
    filtered <- filtered_data()
    if (nrow(filtered) == 0) {
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
          `Outcome Measure` = outcome_measure,
          `Weeks` = as.character(outcome_timepoint),
          `n` = number_participants,
          `SMD` = round(yi, 3),
          lower = lower,
          upper = upper,
          processed_grades = processed_grades,
          processed_school_types = processed_school_types,
          outcome_measure_specific = outcome_measure,
          country = if("country" %in% names(filtered)) country else NA_character_,
          school_level_computed = if("grade_level" %in% names(filtered)) sapply(grade_level, classify_grade_level) else NA_character_,
          urbanicity_computed = if("urbanicity" %in% names(filtered)) sapply(urbanicity, clean_urbanicity) else NA_character_,
          number_schools = if("number_schools" %in% names(filtered)) number_schools else NA_real_,
          number_classrooms = if("number_classrooms" %in% names(filtered)) number_classrooms else NA_real_,
          number_participants = number_participants,
          average_age = if("average_age" %in% names(filtered)) average_age else NA_real_,
          percent_female = if("percent_female" %in% names(filtered)) percent_female else NA_real_,
          percent_FRPL = if("percent_FRPL" %in% names(filtered)) percent_FRPL else NA_real_,
          percent_ELL = if("percent_ELL" %in% names(filtered)) percent_ELL else NA_real_,
          percent_white = if("percent_white" %in% names(filtered)) percent_white else NA_real_,
          percent_black = if("percent_black" %in% names(filtered)) percent_black else NA_real_,
          percent_aian = if("percent_aian" %in% names(filtered)) percent_aian else NA_real_,
          percent_nhpi = if("percent_nhpi" %in% names(filtered)) percent_nhpi else NA_real_,
          percent_asian = if("percent_asian" %in% names(filtered)) percent_asian else NA_real_,
          percent_latinx = if("percent_latinx" %in% names(filtered)) percent_latinx else NA_real_,
          percent_other = if("percent_other" %in% names(filtered)) percent_other else NA_real_
        )
      filtered_forest
    }
  })
  
  # 6. MAIN FOREST PLOT OUTPUT
  output$forest_tbl <- renderReactable({
    merged_forest <- filtered_merged_forest()
    max_n <- GLOBAL_MAX_N 
    hover_state <- hover_enabled()
    
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
      row_height <- 80
    } else {
      merged_forest$border_top <- FALSE
      
      group_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Measure")
      if (nrow(merged_forest) > 1) {
        merged_forest <- merged_forest[do.call(order, merged_forest[group_cols]), ]
      }
      
      original_forest_pre_blank <- merged_forest
      merged_forest <- hierarchical_blanker(merged_forest, group_cols)
      row_height <- calculate_dynamic_row_height(merged_forest)
      
      # Create tooltips
      tooltip_html_texts <- sapply(1:nrow(merged_forest), function(i) {
        tryCatch({
          orig_data <- original_forest_pre_blank[i, ]
          
          if (is.na(orig_data$SMD) || orig_data$SMD == "" || is.na(orig_data$n)) {
            return("")
          }
          
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
          
          ci_text <- paste0("[", round(orig_data$lower, 3), ", ", round(orig_data$upper, 3), "]")
          grades_display <- format_processed_grades(orig_data$processed_grades)
          school_types_display <- safe_format_list(orig_data$processed_school_types)
          
          race_ethnicity_display <- format_race_ethnicity_breakdown(
            orig_data$percent_white, orig_data$percent_black, orig_data$percent_aian,
            orig_data$percent_nhpi, orig_data$percent_asian, orig_data$percent_latinx,
            orig_data$percent_other
          )
          
          tooltip_parts <- c(
            paste0("SMD: <b>", orig_data$SMD, "</b>"),
            paste0("95% CI: <b>", ci_text, "</b>"),
            paste0("Effect Size: <b>", effect_size, "</b>"),
            "",
            paste0("Country: <b>", format_viz_value(orig_data$country), "</b>"),
            paste0("School Level: <b>", format_viz_value(orig_data$school_level_computed), "</b>"),
            paste0("Urbanicity: <b>", format_viz_value(orig_data$urbanicity_computed), "</b>"),
            paste0("Individual Grades: <b>", grades_display, "</b>"),
            paste0("School Type: <b>", school_types_display, "</b>"),
            paste0("No. of Schools: <b>", format_viz_value(orig_data$number_schools), "</b>"),
            paste0("No. of Classrooms: <b>", format_viz_value(orig_data$number_classrooms), "</b>"),
            paste0("No. of Students: <b>", format_viz_value(orig_data$number_participants), "</b>"),
            paste0("Average Age: <b>", format_viz_value(orig_data$average_age), "</b>"),
            paste0("% Female: <b>", format_viz_value(orig_data$percent_female, TRUE), "</b>"),
            paste0("% FRPL: <b>", format_viz_value(orig_data$percent_FRPL, TRUE), "</b>"),
            paste0("% ELL: <b>", format_viz_value(orig_data$percent_ELL, TRUE), "</b>"),
            paste0("Race/Ethnicity: <b>", race_ethnicity_display, "</b>")
          )
          
          return(paste(tooltip_parts, collapse = "<br>"))
          
        }, error = function(e) {
          return(paste0("Study: <b>", as.character(orig_data$`Study Author Year`), "</b><br>Error loading details"))
        })
      }, USE.NAMES = FALSE)
      
      svg_list <- mapply(
        make_forest_svg,
        merged_forest$SMD, merged_forest$lower, merged_forest$upper, merged_forest$n,
        max_n, row_height, FALSE, tooltip_html_texts, hover_state,
        SIMPLIFY = FALSE
      )
      
      merged_forest$` ` <- as.list(svg_list)
      merged_forest$SMD <- as.character(merged_forest$SMD)
      
      display_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Measure", "Weeks", "SMD", " ", "border_top")
      merged_forest <- merged_forest[, display_cols, drop = FALSE]
      merged_forest <- tibble::as_tibble(merged_forest)
    }
    
    forest_axis_footer <- tibble(
      `Study Author Year` = "", `Intervention` = "", `Comparison` = "", 
      `Outcome Measure` = "", `Weeks` = "", `SMD` = "",
      ` ` = list(make_forest_svg(NA, NA, NA, NA, max_n, 180, show_axis = TRUE, tooltip_html = NULL, hover_enabled = FALSE)),
      border_top = TRUE
    )
    
    merged_forest <- dplyr::bind_rows(merged_forest, forest_axis_footer)
    
    # Reactable with all styling
    # Updated reactable with responsive column widths
    # Updated reactable with proper numeric widths
    reactable(
      merged_forest,
      columns = list(
        `Study Author Year` = colDef(
          name = "Study Author Year",
          minWidth = 150,
          # Remove width to allow flex sizing
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word", 
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        `Intervention` = colDef(
          name = "Intervention",
          minWidth = 220,
          # Remove width to allow flex sizing
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        `Comparison` = colDef(
          name = "Comparison",
          minWidth = 120,
          # Remove width to allow flex sizing
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        `Outcome Measure` = colDef(
          name = "Outcome Measure",
          minWidth = 200,
          # Remove width to allow flex sizing
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        `Weeks` = colDef(
          name = "Weeks",
          width = 70,  # Keep small fixed width for this column
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              textAlign = "center",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        `SMD` = colDef(
          name = "SMD",
          width = 80,  # Keep small fixed width for this column
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              textAlign = "center",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        ` ` = colDef(
          name = " ",
          html = TRUE,
          # Use a flex value to take up remaining space (approximately 25% of total)
          minWidth = 400,
          maxWidth = 800,
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              padding = "8px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
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
        overflowY = "auto",
        width = "100%"
      ),
      rowStyle = function(index) {
        is_axis_row <- index == nrow(merged_forest)
        if (is_axis_row) {
          list(height = "180px")
        } else {
          list(
            height = paste0(row_height, "px"),
            minHeight = "80px"
          )
        }
      },
      fullWidth = TRUE,
      defaultPageSize = nrow(merged_forest)
    )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

