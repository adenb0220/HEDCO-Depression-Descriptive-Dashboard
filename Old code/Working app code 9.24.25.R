#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi, shinyWidgets, shinyjs)

# Import data - Get all outcome domains
outcome_domains_sheets <- readxl::excel_sheets(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"))
# Remove "metadata" from outcome domains
outcome_domains_sheets <- outcome_domains_sheets[outcome_domains_sheets != "metadata"]
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level")

# Load all domain sheets and combine
load_all_domains <- function(domains) {
  all_data <- list()
  
  for (domain in domains) {
    domain_data <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which = domain)
    
    # Use outcome_domain column from the data if it exists, otherwise fall back to sheet name
    if (!"outcome_domain" %in% names(domain_data)) {
      domain_data$outcome_domain <- domain  # Add domain identifier using sheet name as fallback
    }
    
    # Fix the specific icc column type issue if it exists
    if ("icc" %in% names(domain_data)) {
      domain_data$icc <- as.numeric(as.character(domain_data$icc))
    }
    
    all_data[[domain]] <- domain_data
  }
  
  # Combine all sheets
  combined_df <- bind_rows(all_data)
  return(combined_df)
}

# Load all domain data
all_domains_df <- load_all_domains(outcome_domains_sheets)

# Get unique outcome domains from the actual data (not sheet names)
outcome_domains <- sort(unique(all_domains_df$outcome_domain))

# Load studies data (same as before)
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which = "study_level")

# Helper function to standardize (same as before)
standardize <- function(x) {
  x %>%
    tolower() %>%
    stri_trans_general("Latin-ASCII") %>%
    trimws()
}

# Aligning study years (same as before)
studies$study_author_year[studies$study_author_year == "McLaughlin 2010"] <- "McLaughlin 2011"

# Standardize keys
all_domains_df <- all_domains_df %>% mutate(study_std = standardize(study))
studies <- studies %>% mutate(study_author_year_std = standardize(study_author_year))

# Create the full merged dataset with all domains
merged_all_domains <- all_domains_df %>%
  left_join(
    studies,
    by = c("study_std" = "study_author_year_std"),
    suffix = c(".df", ".studies")
  )

# Handle duplicate columns
dup_bases <- intersect(
  gsub("\\.df$", "", names(merged_all_domains)[endsWith(names(merged_all_domains), ".df")]),
  gsub("\\.studies$", "", names(merged_all_domains)[endsWith(names(merged_all_domains), ".studies")])
)

for (col in dup_bases) {
  merged_all_domains[[col]] <- merged_all_domains[[paste0(col, ".studies")]]
}

merged_all_domains <- merged_all_domains %>% select(-matches("\\.df$"), -matches("\\.studies$"))

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
  if (min_g >= 0 && max_g <= 8) return("K-8")      # Kindergarten through 8th grade
  if (min_g >= 9 && max_g <= 12) return("9-12")    # 9th through 12th grade
  if (min_g <= 8 && max_g >= 9) return("K-12")     # Spans from K-8 into 9-12
  
  return("Unclear")
}

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

# Outcome measure choices - Root-based grouping
# Root-based outcome measure grouping with improved suffix handling
# Improved outcome measure grouping function
process_outcome_measures_roots <- function(all_outcome_measures) {
  # Remove NA values and empty strings for processing
  clean_measures <- all_outcome_measures[!is.na(all_outcome_measures) & trimws(all_outcome_measures) != ""]
  
  if (length(clean_measures) == 0) return(rep("Other/Unclear", length(all_outcome_measures)))
  
  # Function to normalize and extract base instrument names
  normalize_instrument_name <- function(measure) {
    # Convert to lowercase for comparison
    lower_measure <- tolower(measure)
    
    # Define instrument families with their variations
    instrument_patterns <- list(
      "Center for Epidemiologic Studies Depression Scale" = c(
        "center for epidemiologic studies depression scale",
        "ces-d", "cesd"
      ),
      "Children's Depression Inventory" = c(
        "children's depression inventory",
        "childrens depression inventory",
        "child depression inventory",
        "cdi"
      ),
      "Revised Children's Anxiety and Depression Scale" = c(
        "revised children's anxiety and depression scale",
        "revised child anxiety and depression scale",
        "rcads"
      ),
      "Beck Depression Inventory" = c(
        "beck depression inventory",
        "bdi"
      ),
      "Patient Health Questionnaire" = c(
        "patient health questionnaire",
        "phq"
      ),
      "Depression Anxiety Stress Scale" = c(
        "depression anxiety stress scale",
        "dass"
      ),
      "Reynolds Adolescent Depression Scale" = c(
        "reynolds adolescent depression scale",
        "rads"
      ),
      "Reynolds Child Depression Scale" = c(
        "reynolds child depression scale",
        "rcds"
      ),
      "Revised Children's Manifest Anxiety Scale" = c(
        "revised children's manifest anxiety scale",
        "revised child manifest anxiety scale",
        "rcmas"
      ),
      "Spence Children's Anxiety Scale" = c(
        "spence children's anxiety scale",
        "spence child anxiety scale",
        "scas"
      ),
      "Mood and Feelings Questionnaire" = c(
        "mood and feelings questionnaire",
        "mfq"
      ),
      "Generalised Anxiety Disorder Scale" = c(
        "generalised anxiety disorder",
        "generalized anxiety disorder",
        "gad"
      ),
      "Multidimensional Anxiety Scale" = c(
        "multidimensional anxiety scale",
        "masc"
      ),
      "State-Trait Anxiety Inventory" = c(
        "state-trait anxiety inventory",
        "speilberger state-trait anxiety inventory",
        "spielberger state-trait anxiety inventory",
        "stai"
      ),
      "Major Depression Inventory" = c(
        "major depression inventory",
        "mdi"
      ),
      "Warwick-Edinburgh Mental Wellbeing Scale" = c(
        "warwick-edinburgh mental wellbeing scale",
        "wemwbs"
      ),
      "Kessler Psychological Distress Scale" = c(
        "kessler psychological distress scale",
        "six-item short form of the kessler psychological distress scale",
        "k6", "k10"
      )
    )
    
    # Check each instrument family
    for (instrument_name in names(instrument_patterns)) {
      patterns <- instrument_patterns[[instrument_name]]
      for (pattern in patterns) {
        if (grepl(pattern, lower_measure, fixed = TRUE)) {
          return(instrument_name)
        }
      }
    }
    
    # If no match found, try to extract a clean base name
    # Remove common suffixes and modifiers
    clean_measure <- measure
    clean_measure <- gsub("\\s*>\\s*\\d+.*$", "", clean_measure)  # Remove > 24, > 15 etc
    clean_measure <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*(General\\s+Anxiety\\s+Subscale|Panic\\s+Subscale|Separation\\s+Anxiety\\s+Subscale|Social\\s+Phobia\\s+Subscale|Depression\\s+Subscale|Anxiety\\s+Subscale).*$", "", clean_measure)
    clean_measure <- gsub("\\s*Short\\s+Version.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Revised.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-2.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-Youth.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-II.*$", "", clean_measure)
    clean_measure <- gsub("\\s*for\\s+Children.*$", "", clean_measure)
    clean_measure <- gsub("-9$", "", clean_measure)  # Handle PHQ-9 -> PHQ
    clean_measure <- gsub("\\s*\\(.*\\).*$", "", clean_measure)  # Remove anything in parentheses
    clean_measure <- gsub("\\s+", " ", trimws(clean_measure))
    
    return(clean_measure)
  }
  
  # Normalize all measures
  normalized <- sapply(clean_measures, normalize_instrument_name, USE.NAMES = FALSE)
  
  # Create result mapping
  result <- character(length(all_outcome_measures))
  clean_idx <- 1
  
  for (i in seq_along(all_outcome_measures)) {
    if (is.na(all_outcome_measures[i]) || trimws(all_outcome_measures[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      result[i] <- normalized[clean_idx]
      clean_idx <- clean_idx + 1
    }
  }
  
  return(result)
}

# Test the function with your data
outcome_measure_family_choices <- c(
  "Alcohol Misuse",
  "Anxiety Scale",
  "Australian Tertiary Admission Rank",
  "Beck Depression Inventory",
  "Cannabis Misuse",
  "Center for Epidemiologic Studies Depression Scale",
  "Center for Epidemiologic Studies Depression Scale > 24",
  "Center for Epidemiologic Studies Depression Scale >15",
  "Children's Depression Inventory",
  "Children's Automatic Thoughts Scale",
  "Children's Depression Inventory",
  "Children's Depression Rating Scale",
  "Childrens Depression Inventory",
  "Depression Anxiety Stress Scale",
  "Depression Questionnaire",
  "Diagnostic Interview",
  "Generalised Anxiety Disorder seven item scale",
  "Major Depression Inventory",
  "Mood and Feelings Questionnaire",
  "Multidimensional Anxiety Scale",
  "Patient Health Questionnaire-9",
  "Peer Nomination Inventory for Depression",
  "Revised Children's Anxiety and Depression Scale",
  "Revised Children's Anxiety and Depression Scale - General Anxiety Subscale",
  "Revised Children's Anxiety and Depression Scale - Panic Subscale",
  "Revised Children's Anxiety and Depression Scale - Separation Anxiety Subscale",
  "Revised Children's Anxiety and Depression Scale - Social Phobia Subscale",
  "Revised Children's Attributional Style Questionnaire",
  "Revised Children's Manifest Anxiety Scale",
  "Reynolds Adolescent Depression Scale",
  "Reynolds Child Depression Scale",
  "Scale for Assessing Academic Stress",
  "School Grades",
  "Selbstbeurteilungsbogen-Depressive Stoerungen",
  "Self-Harming Behavior",
  "Self-Harming Thoughts",
  "Six-item short form of the Kessler psychological distress scale",
  "Speilberger State-Trait Anxiety Inventory",
  "Spence Child Anxiety Scale - Generalized Anxiety Disorder",
  "Spence Child Anxiety Scale - Social Anxiety",
  "Spence Children's Anxiety Scale",
  "State Anxiety Inventory for Children",
  "Street Drug Misuse",
  "Structured Clinical Interview for DSM-IV Disorders",
  "Structured Clinical Interviews",
  "Warwick-Edinburgh Mental Wellbeing Scale",
  "Youth Risk Behavior Survey"
)

# Test the grouping
grouped_outcomes <- process_outcome_measures_roots(outcome_measure_family_choices)

# Show the results
results_df <- data.frame(
  Original = outcome_measure_family_choices,
  Grouped = grouped_outcomes,
  stringsAsFactors = FALSE
)

print(results_df)

########################################
# Intervention processing
process_intervention_roots <- function(all_interventions) {
  # Helper to normalize hyphens/dashes and spaces
  normalize_text <- function(x) {
    # Replace all unicode dashes with ASCII hyphen
    x <- gsub("[\u2012\u2013\u2014\u2015\u2212]", "-", x) # en dash, em dash, figure dash, minus, etc.
    x <- gsub("[–—−]", "-", x) # Other common dash forms
    # Remove duplicate spaces and leading/trailing
    trimws(gsub("\\s+", " ", x))
  }
  
  # Remove NA and empty
  clean_interventions <- all_interventions[!is.na(all_interventions) & trimws(all_interventions) != ""]
  if (length(clean_interventions) == 0) return(rep("Other/Unclear", length(all_interventions)))
  
  # Extract root (basic cleaning)
  extract_root <- function(intervention) {
    root <- normalize_text(intervention)
    root <- gsub("\\([^)]*\\)", "", root) # Remove parentheses
    root <- normalize_text(root)
    root
  }
  
  # Grouping by pattern
  custom_group_root <- function(root) {
    # .b Interventions
    if (grepl("^\\.b", root, ignore.case = TRUE)) {
      return(".b Interventions")
    }
    # Adolescent Coping with Depression Interventions
    if (grepl("^Adolescents? Coping with Depression", root, ignore.case = TRUE) |
        grepl("^Adolescent Coping with Depression", root, ignore.case = TRUE)) {
      return("Adolescent Coping with Depression Interventions")
    }
    # Resourceful Adolescent Program Interventions
    if (grepl("^Resourceful Adolescent Program", root, ignore.case = TRUE)) {
      return("Resourceful Adolescent Program Interventions")
    }
    # Penn Program Interventions
    if (grepl("^Penn Prevention Program", root, ignore.case = TRUE) |
        grepl("^Penn Resiliency Program", root, ignore.case = TRUE) |
        grepl("^Reversed Penn Group", root, ignore.case = TRUE) |
        grepl("^Normal Penn group", root, ignore.case = TRUE)) {
      return("Penn Program Interventions")
    }
    # Social Support Intervention Interventions
    if (grepl("^Social Support Intervention", root, ignore.case = TRUE)) {
      return("Social Support Intervention Interventions")
    }
    # Aussie Optimism Program Interventions
    if (grepl("^Aussie Optimism Program", root, ignore.case = TRUE)) {
      return("Aussie Optimism Program Interventions")
    }
    # Cognitive-Behavioral Program Interventions
    if (grepl("^Cognitive-Behavior", root, ignore.case = TRUE)) {
      return("Cognitive-Behavioral Program Interventions")
    }
    # EMOTION Interventions
    if (grepl("^EMOTION", root, ignore.case = TRUE) |
        grepl("Coping Kids Managing Anxiety and Depression", root, ignore.case = TRUE)) {
      return("EMOTION / Coping Kids Interventions")
    }
    # MoodGYM Interventions
    if (grepl("^MoodGYM", root, ignore.case = TRUE)) {
      return("MoodGYM Interventions")
    }
    # Universal Intervention Interventions
    if (grepl("^Universal Intervention", root, ignore.case = TRUE) |
        grepl("^Universal plus Indicated Intervention", root, ignore.case = TRUE)) {
      return("Universal Intervention Interventions")
    }
    # Interpersonal Psychotherapy Interventions
    if (grepl("^Interpersonal Psychotherapy", root, ignore.case = TRUE)) {
      return("Interpersonal Psychotherapy Interventions")
    }
    # Smart, Positive, Active, Realistic, X-factor thoughts Interventions
    if (grepl("^Smart, Positive, Active, Realistic, X-factor thoughts", root, ignore.case = TRUE)) {
      return("Smart, Positive, Active, Realistic, X-factor thoughts Interventions")
    }
    # Add new patterns for other program interventions here...
    
    # Default: return cleaned root (for singletons)
    root
  }
  
  # Step 1: extract roots + normalize
  roots <- sapply(clean_interventions, extract_root, USE.NAMES = FALSE)
  # Step 2: apply grouping by interventions
  grouped_roots <- sapply(roots, custom_group_root, USE.NAMES = FALSE)
  
  # Build result for all_interventions
  result <- character(length(all_interventions))
  clean_idx <- 1
  for (i in seq_along(all_interventions)) {
    if (is.na(all_interventions[i]) || trimws(all_interventions[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      root <- grouped_roots[clean_idx]
      result[i] <- root
      clean_idx <- clean_idx + 1
    }
  }
  return(result)
}

# Apply ALL data processing to merged_all_domains
merged_all_domains <- merged_all_domains %>%
  mutate(urbanicity_clean = sapply(urbanicity, clean_urbanicity)) %>%
  mutate(processed_grades = sapply(grade_level, process_grade_levels)) %>%
  mutate(processed_school_types = sapply(school_type, process_school_types))

# Apply outcome measure processing
processed_outcomes_roots <- process_outcome_measures_roots(merged_all_domains$outcome_measure)
merged_all_domains <- merged_all_domains %>%
  mutate(processed_outcome_measure_roots = processed_outcomes_roots)

# Apply intervention processing and create choices
merged_all_domains$processed_intervention_roots <- process_intervention_roots(merged_all_domains$intervention)

# Set default to show ALL domains instead of filtering
merged <- merged_all_domains  # Use all data by default

GLOBAL_MAX_N <- max(merged$number_participants, na.rm = TRUE)

# School level creation - Updated choices
school_level_choices <- merged_all_domains %>%
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

urbanicity_choices <- merged_all_domains %>%
  distinct(urbanicity_clean) %>%
  pull(urbanicity_clean) %>%
  as.character() %>%
  sort()

# If you want "Unclear" at the bottom:
urbanicity_choices <- c(setdiff(urbanicity_choices, "Unclear"), "Unclear")

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

# Create outcome measure family choices
outcome_measure_family_choices <- merged %>%
  distinct(processed_outcome_measure_roots) %>%
  pull(processed_outcome_measure_roots) %>%
  sort()

# Helper function to check if a study's outcome measures match selected filters
outcome_measures_match_filter_roots <- function(processed_outcome, selected_outcomes) {
  return(processed_outcome %in% selected_outcomes)
}

intervention_group_choices <- sort(unique(merged$processed_intervention_roots))

# Store country choices for use in UI
country_choices <- sort(unique(merged$country))

#############################################################################
# Define UI for application
ui <- fluidPage(
  useShinyjs(),
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
          color: #007030;
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
        .reactable-table th, .reactable-table td { 
          box-sizing: border-box !important;
        }
        .reactable thead th {
          position: sticky;
          top: 0;
          background-color: #fff;
          z-index: 2;
        }
        
          .text-tooltip, .svg-tooltip {
          display: none;
          position: fixed;
          z-index: 10000;
          background: white;
          color: black;
          padding: 15px;
          border-radius: 8px;
          font-size: 13px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.3);
          border: 2px solid #333;
          pointer-events: none;
        }

      body.hover-disabled .text-tooltip,
      body.hover-disabled .svg-tooltip,
      body.hover-disabled .text-cell-hover:hover .text-tooltip,
      body.hover-disabled .svg-container:hover .svg-tooltip {
        display: none !important;
      }
    }
    body, * {
  cursor: default !important;
    }

      /* Pointer cursor ONLY for interactive/filter elements */
      button, .btn, a, label[for], .selectpicker, select, input[type="checkbox"], input[type="radio"],
      .dropdown-menu, .bootstrap-select, .nav-tabs, #toggle_hover, #reset_filters, .picker, summary, details,
      .reactable .rt-th, .reactable .rt-td, .reactable .rt-tr, .reactable .rt-tbody {
        cursor: pointer !important;
      }
      
      /* Text cursor for text inputs */
      input[type="text"], textarea, input[type="search"], input[type="password"], [contenteditable] {
        cursor: text !important;
      }
      
      /* Force arrow cursor on all plotly containers and SVGs */
      .plotly, .plotly > div, .svg-container, .main-svg, .plotly-graph-div, .plot-container, g, rect, .hoverlayer, .subplot, .cartesianlayer, .layer-above, .layer-below {
        cursor: default !important;
      }
      
      /* For reactable table body and headers, default cursor unless .rt-th/.rt-td are interactive */
      .reactable, .reactable-table, .rt-table, .rt-thead, .rt-tbody, .rt-tr-group, .rt-tr {
        cursor: default !important;
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
          min-width: 300px;
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
      x = e.clientX - rect.width - 0;
    }
    if (y + rect.height > window.innerHeight) {
      y = e.clientY - rect.height - 0;
    }
    
    tooltip.style.left = x + "px";
    tooltip.style.top = y + "px";
  }
});
</script>
'),
  uiOutput("outcomes_tab_triggers"),
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
    
    # Filter area in the middle (8 columns instead of 9)
    column(8,
           # First row of filters
           fluidRow(
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "country_filter",
                      label = "Location",
                      choices = country_choices,
                      selected = country_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(country_choices) - 1),
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
                      inputId = "intervention_group_filter",
                      label = "Intervention Group",
                      choices = intervention_group_choices,
                      selected = intervention_group_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(intervention_group_choices) - 1),
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
           ),
           
           # Second row of filters
           fluidRow(
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
                      label = "Outcome Measure Group",
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
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "outcome_domains",
                      label = "Outcome Domain",
                      choices = outcome_domains,
                      selected = outcome_domains,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(outcome_domains) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
           )
    ),
    
    # Reset button on the right (2 columns instead of 1)
    column(2,
           div(style = "margin-top: 35px;",
               actionButton(
                 "reset_filters", 
                 tagList(icon("times-circle"), HTML("&nbsp;Clear All Filters")), 
                 class = "btn-warning btn-sm", 
                 style = "background-color: #8ABB40 ; border-color: #8ABB40 ; color: white; font-size: clamp(10px, 1.8vw, 14px); padding: 15px 8px; white-space: nowrap; width: 90%; margin-left: -20px; height: 60px; display: flex; align-items: center; justify-content: center;"
               )
           ),
           div(style = "margin-top: 30px;",
               actionButton(
                 "toggle_hover", 
                 tagList(
                   icon("info-circle", style = "margin-right:8px;"), 
                   "Hover Information: ON"
                 ), 
                 class = "btn-info btn-sm", 
                 style = "background-color: #007030 ; border-color: #007030 ; color: white; font-size: clamp(10px, 1.8vw, 14px); padding: 15px 8px; white-space: nowrap; width: 90%; margin-left: -20px; height: 60px; display: flex; align-items: center; justify-content: center;"
               )
           )
    )
  ),
  
  # Tabs - Forest Plot is now FIRST, Visualizations is SECOND
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
                        "Location"
                      ),
                      plotlyOutput("world_map"),
                      uiOutput("country_note") 
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
               column(3,
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
               column(5,
                      uiOutput("outcomes_navigation"),
                      plotlyOutput("outcomes_graph", height = "400px")
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
  
  # Hover state reactive
  hover_enabled <- reactiveVal(TRUE)
  
  # Robust hover toggle observer
  observeEvent(input$toggle_hover, {
    current_state <- hover_enabled()
    hover_enabled(!current_state)
    if (hover_enabled()) {
      shinyjs::runjs("
      document.body.classList.remove('hover-disabled');
      $('#toggle_hover').html('<i class=\"fa fa-info-circle\" style=\"margin-right:8px;\"></i>Hover Information: ON');
    ")
    } else {
      shinyjs::runjs("
      document.body.classList.add('hover-disabled');
      $('#toggle_hover').html('<i class=\"fa fa-info-circle\" style=\"margin-right:8px;\"></i>Hover Information: OFF');
    ")
    }
  })
  
  # Filtered data reactive
  filtered_data <- reactive({
    # If ANY filter is empty, return empty data frame
    if (
      is.null(input$country_filter) || length(input$country_filter) == 0 ||
      is.null(input$school_level_filter) || length(input$school_level_filter) == 0 ||
      is.null(input$urbanicity_filter) || length(input$urbanicity_filter) == 0 ||
      is.null(input$individual_grade_filter) || length(input$individual_grade_filter) == 0 ||
      is.null(input$individual_school_type_filter) || length(input$individual_school_type_filter) == 0 ||
      is.null(input$outcome_family_filter) || length(input$outcome_family_filter) == 0 ||
      is.null(input$intervention_group_filter) || length(input$intervention_group_filter) == 0 ||
      is.null(input$outcome_domains) || length(input$outcome_domains) == 0
    ) {
      return(merged[0,])
    }
    
    data <- merged %>%
      mutate(grade_category = sapply(grade_level, classify_grade_level)) %>%
      filter(outcome_domain %in% input$outcome_domains) %>%
      filter(country %in% input$country_filter) %>%
      filter(grade_category %in% input$school_level_filter) %>%
      filter(urbanicity_clean %in% input$urbanicity_filter) %>%
      filter(sapply(processed_grades, function(x) grades_match_filter(x, input$individual_grade_filter))) %>%
      filter(sapply(processed_school_types, function(x) school_types_match_filter(x, input$individual_school_type_filter))) %>%
      filter(processed_outcome_measure_roots %in% input$outcome_family_filter) %>%
      filter(processed_intervention_roots %in% input$intervention_group_filter)
    
    data
  })
  
  # Reset button observer
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "country_filter", selected = country_choices)
    updatePickerInput(session, "school_level_filter", selected = school_level_choices)
    updatePickerInput(session, "urbanicity_filter", selected = urbanicity_choices)
    updatePickerInput(session, "individual_grade_filter", selected = individual_grade_choices)
    updatePickerInput(session, "individual_school_type_filter", selected = individual_school_type_choices)
    updatePickerInput(session, "outcome_family_filter", selected = outcome_measure_family_choices)
    updatePickerInput(session, "intervention_group_filter", selected = intervention_group_choices)
    updatePickerInput(session, "outcome_domains", selected = outcome_domains)
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
    hover_is_on <- hover_enabled()
    div(class = "studies-panel",
        style = "padding: 8px 12px 25px 12px; border-radius: 5px; margin-top: 22px; width: 180px;",
        h4("No. of Studies", style = "margin: 0 0 5px 0;"),
        span(class = "studies-count", n_studies),
        if (hover_is_on) span(class = "tooltiptext", HTML(sprintf("Distinct count of Study Author Year: <b>%d</b>", n_studies)))
        # Tooltip is only rendered in the UI when hover is enabled
    )
  })
  ###########################################################################  
  # Count studies per country
  country_counts <- reactive({
    filtered_data() %>%
      distinct(study, country) %>%
      count(country)
  })
  
  # Reactive to count studies in Hong Kong and Mauritius
  hk_mauritius_counts <- reactive({
    fd <- filtered_data()
    tibble(
      country = c("Hong Kong", "Mauritius"),
      count = sapply(c("Hong Kong", "Mauritius"), function(cntry) {
        n_distinct(fd$study[fd$country == cntry])
      })
    )
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
  
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735"
  )
  
  output$world_map <- renderPlotly({
    d <- country_map_data()
    hover_is_on <- hover_enabled()
    
    plot <- plot_geo() %>%
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
        hoverinfo = if (hover_is_on) "text+z" else "none",  # toggle hover
        hovertemplate = if (hover_is_on) paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>%{text}</b><br>",
          "No. of Studies: <b>%{z}</b></span><extra></extra>"
        ) else NULL,
        autocolorscale = FALSE
      ) %>%
      add_trace(
        type = "scattergeo",
        mode = "text",
        locations = d$studied_iso3,
        locationmode = "ISO-3",
        text = d$studied_counts,
        hovertext = if (hover_is_on) paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>", d$studied_names, "</b><br>",
          "No. of Studies: <b>", d$studied_counts, "</b></span>"
        ) else "",
        hoverinfo = if (hover_is_on) "text" else "none",
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
    
    plot
  })
  
  output$country_note <- renderUI({
    counts <- hk_mauritius_counts()
    hk_count <- counts$count[counts$country == "Hong Kong"]
    maur_count <- counts$count[counts$country == "Mauritius"]
    
    pluralize <- function(n, name) {
      verb <- if (n == 1) "was" else "were"
      word <- if (n == 1) "study" else "studies"
      sprintf("<b>%d</b> %s %s set in <b>%s</b>", n, word, verb, name)
    }
    
    main_text <- sprintf("%s and %s.", 
                         pluralize(hk_count, "Hong Kong"),
                         pluralize(maur_count, "Mauritius"))
    
    HTML(sprintf(
      "<span style='font-size:16px; color:#444; margin-left:40px; display: block;'><b>Note:</b> Countries under 1,000 sq miles are not visible on this map.<br>
     With the current filters: %s", main_text))
  })
  
  ########################################################
  output$school_level <- renderPlotly({
    hover_is_on <- hover_enabled()
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
      
      ggplotly(p, tooltip = if (hover_is_on) "text" else NULL) %>%
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
    hover_is_on <- hover_enabled()
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
      
      ggplotly(urbanicity_plot, tooltip = if (hover_is_on) "text" else NULL ) %>%
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
    hover_is_on <- hover_enabled()
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_schools),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~number_schools_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_classrooms),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~number_classrooms_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
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
      hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_age),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~average_age_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_female, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_female_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
    
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
    
    ggplotly(race_plot, tooltip = if (hover_is_on) "text" else NULL ) %>%
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
    hover_is_on <- hover_enabled()
    
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_frpl, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_FRPL_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
    hover_is_on <- hover_enabled()
    
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
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_ell, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~percent_ELL_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
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
  # Outcome families 
  # Add reactive value to track which plot is shown
  # Reactive value to track which plot is shown
  outcomes_page_selected <- reactiveVal(1)  # 1 for ">2 studies", 2 for "<2 studies"
  
  output$outcomes_navigation <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    # Prepare outcome families
    outcomes_base_data <- data %>%
      mutate(
        outcome_family_clean = ifelse(
          is.na(processed_outcome_measure_roots) | trimws(processed_outcome_measure_roots) == "",
          "Other/Unclear",
          trimws(processed_outcome_measure_roots)
        )
      ) %>%
      distinct(study, outcome_family_clean)
    
    family_counts <- outcomes_base_data %>%
      group_by(outcome_family_clean) %>%
      summarise(outcome_n = n_distinct(study), .groups = "drop")
    
    # Grouping counts for navigation
    gt2_count <- sum(family_counts$outcome_n > 2)
    lt2_count <- sum(family_counts$outcome_n <= 2)
    current_page <- outcomes_page_selected()
    
    # Navigation bar
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px",
      div(style = "width: 80px;"), # Empty div for layout spacing
      div(
        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
        if (current_page == 1) {
          paste0("Outcome Measure Groups Used in >2 Studies")
        } else {
          paste0("Outcome Measure Groups Used in <2 Studies")
        }
      ),
      div(
        if (current_page == 1 && lt2_count > 0) {
          actionButton("outcomes_next", HTML("Next &#8250;"), class = "btn btn-outline-secondary btn-sm", style = "font-size: 12px;")
        } else if (current_page == 2) {
          actionButton("outcomes_back_to_first", HTML("Previous &#8249;"), class = "btn btn-outline-secondary btn-sm", style = "font-size: 12px;")
        } else {
          span()
        }
      )
    )
  })
  
  # Navigation button observers
  observeEvent(input$outcomes_prev, {
    outcomes_page_selected(max(1, outcomes_page_selected() - 1))
  })
  observeEvent(input$outcomes_next, {
    outcomes_page_selected(2)
  })
  observeEvent(input$outcomes_back_to_first, {
    outcomes_page_selected(1)
  })
  
  output$outcomes_graph <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    outcomes_base_data <- data %>%
      mutate(
        outcome_family_clean = ifelse(
          is.na(processed_outcome_measure_roots) | trimws(processed_outcome_measure_roots) == "",
          "Other/Unclear",
          trimws(processed_outcome_measure_roots)
        )
      ) %>%
      distinct(study, outcome_family_clean)
    
    family_counts <- outcomes_base_data %>%
      group_by(outcome_family_clean) %>%
      summarise(outcome_n = n_distinct(study), .groups = "drop")
    
    current_page <- outcomes_page_selected()
    
    if (current_page == 1) {
      # Show outcome families used in >2 studies
      plot_data <- family_counts %>%
        filter(outcome_n > 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in >2 studies"
      y_label <- "Number of Studies"
    } else {
      # Show outcome families used in <2 studies
      plot_data <- family_counts %>%
        filter(outcome_n <= 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in <2 studies"
      y_label <- "Number of Studies"
    }
    
    if (nrow(plot_data) == 0) {
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = if(current_page == 1) "No outcome families used in >2 studies" else "No outcome families used in <2 studies",
                 size = 5, color = "gray60") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      return(ggplotly(empty_plot, tooltip = "none") %>%
               layout(showlegend = FALSE, 
                      margin = list(l = 20, r = 20, t = 20, b = 20)) %>%
               config(displayModeBar = FALSE, responsive = TRUE))
    }
    
    if (current_page == 1) {
      # Show outcome families used in >2 studies
      plot_data <- family_counts %>%
        filter(outcome_n > 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in >2 studies"
      y_label <- "Number of Studies"
      y_breaks <- seq(0, max(plot_data$outcome_n, na.rm = TRUE), by = 5)
    } else {
      # Show outcome families used in <2 studies
      plot_data <- family_counts %>%
        filter(outcome_n <= 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in <2 studies"
      y_label <- "Number of Studies"
      y_breaks <- seq(1, 2, by = 1)
    }
    
    plot_data <- plot_data %>%
      mutate(
        display_name = factor(outcome_family_clean, levels = outcome_family_clean),
        hover = sprintf(
          "Outcome Family: <b>%s</b><br>Studies: <b>%g</b>",
          outcome_family_clean, 
          as.numeric(outcome_n)
        )
      )
    
    outcomes_plot <- ggplot(plot_data, aes(
      x = display_name,
      y = outcome_n,
      fill = outcome_n,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
      ) +
      labs(x = NULL, y = y_label) +
      scale_x_discrete(limits = rev)+
      scale_y_continuous(breaks = y_breaks)
    
    
    p <- ggplotly(outcomes_plot, tooltip = if (hover_is_on) "text" else NULL) %>%
      layout(
        margin = list(l = 150, r = 5, t = 20, b = 30),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    return(p)
  })
  
  
  
  ##################################################################################
  ### Forest Plot Code with Hover Tooltips for Blank Cells
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
  
  # NEW: Function to create cell with hover tooltip for original content
  create_cell_with_hover <- function(display_text, original_text, column_name, row_index) {
    cell_id <- paste0("cell_", column_name, "_", row_index)
    
    # If display text is empty/blank but original text exists, show tooltip
    if ((is.null(display_text) || display_text == "" || is.na(display_text)) && 
        !is.null(original_text) && original_text != "" && !is.na(original_text)) {
      
      return(sprintf(
        '<div class="hover-cell" id="%s" data-original="%s" style="width: 100%%; height: 100%%; cursor: pointer; position: relative;">
        <span class="display-text">%s</span>
        <div class="cell-tooltip" style="display: none; position: fixed; z-index: 10000; background: white; color: black; padding: 10px; border-radius: 6px; font-size: 13px; max-width: 400px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); white-space: normal; line-height: 1.4; border: 1px solid #ccc; pointer-events: none;">%s</div>
      </div>',
        cell_id,
        htmltools::htmlEscape(as.character(original_text)),
        htmltools::htmlEscape(as.character(display_text)),
        htmltools::htmlEscape(as.character(original_text))
      ))
    } else {
      # Regular cell without hover
      return(htmltools::htmlEscape(as.character(display_text)))
    }
  }
  
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
  
  # 4. FOREST SVG FUNCTION 
  make_forest_svg <- function(yi, lower, upper, n, max_n = NULL, row_height = 70, show_axis = FALSE, tooltip_html = NULL, hover_enabled = TRUE) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 500
    svg_height <- if (show_axis) 48 else row_height
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
      axis_y <- 25
      tick_label_y <- 45
      axis_label_y <- 10
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
              x1 = tick_x[i], x2 = tick_x[i], y1 = axis_y, y2 = axis_y + 8,
              stroke = "#444", "stroke-width" = 2
            ),
            htmltools::tags$text(
              x = tick_x[i], y = tick_label_y,
              text.anchor = "middle", font.size = 13, font.family = "Arial",
              fill = "#333", font.weight = "bold", ticks[i]
            )
          )
        }),
        htmltools::tags$text(
          x = ref_width / 2, y = axis_label_y,
          "Standardized Mean Difference",
          font.size = 14, font.family = "Arial", text.anchor = "middle",
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
          `Outcome Domain` = outcome_domain,        
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
  ##################################################################################
  ### Forest Plot Code with Hover Tooltips for Blank Cells
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
  
  # NEW: Function to create cell with hover tooltip for original content
  create_cell_with_hover <- function(display_text, original_text, column_name, row_index) {
    cell_id <- paste0("cell_", column_name, "_", row_index)
    
    # If display text is empty/blank but original text exists, show tooltip
    if ((is.null(display_text) || display_text == "" || is.na(display_text)) && 
        !is.null(original_text) && original_text != "" && !is.na(original_text)) {
      
      return(sprintf(
        '<div class="hover-cell" id="%s" data-original="%s" style="width: 100%%; height: 100%%; cursor: pointer; position: relative;">
        <span class="display-text">%s</span>
        <div class="cell-tooltip" style="display: none; position: fixed; z-index: 10000; background: white; color: black; padding: 10px; border-radius: 6px; font-size: 13px; max-width: 400px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); white-space: normal; line-height: 1.4; border: 1px solid #ccc; pointer-events: none;">%s</div>
      </div>',
        cell_id,
        htmltools::htmlEscape(as.character(original_text)),
        htmltools::htmlEscape(as.character(display_text)),
        htmltools::htmlEscape(as.character(original_text))
      ))
    } else {
      # Regular cell without hover
      return(htmltools::htmlEscape(as.character(display_text)))
    }
  }
  
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
  
  # 4. FOREST SVG FUNCTION 
  make_forest_svg <- function(yi, lower, upper, n, max_n = NULL, row_height = 70, show_axis = FALSE, tooltip_html = NULL, hover_enabled = TRUE) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 500
    svg_height <- if (show_axis) 48 else row_height
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
      axis_y <- 25
      tick_label_y <- 45
      axis_label_y <- 10
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
              x1 = tick_x[i], x2 = tick_x[i], y1 = axis_y, y2 = axis_y + 8,
              stroke = "#444", "stroke-width" = 2
            ),
            htmltools::tags$text(
              x = tick_x[i], y = tick_label_y,
              text.anchor = "middle", font.size = 13, font.family = "Arial",
              fill = "#333", font.weight = "bold", ticks[i]
            )
          )
        }),
        htmltools::tags$text(
          x = ref_width / 2, y = axis_label_y,
          "Standardized Mean Difference",
          font.size = 14, font.family = "Arial", text.anchor = "middle",
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
          `Outcome Domain` = outcome_domain,        
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
    
    # If no data, return empty reactable with message
    if (nrow(merged_forest) == 0) {
      empty_data <- tibble(
        Message = "No studies meet selected filters."
      )
      return(
        reactable(
          empty_data,
          columns = list(
            Message = colDef(
              name = "",
              style = list(
                textAlign = "center",
                fontSize = "1.5em",
                color = "#333",
                padding = "60px"
              ),
              headerStyle = list(display = "none")
            )
          ),
          showPagination = FALSE,
          style = list(
            border = "none"
          ),
          bordered = FALSE,
          highlight = FALSE
        )
      )
    }
    else {
      merged_forest$border_top <- FALSE
      
      group_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Domain", "Outcome Measure")
      if (nrow(merged_forest) > 1) {
        merged_forest <- merged_forest[do.call(order, merged_forest[group_cols]), ]
      }
      
      # Store original data BEFORE blanking for tooltips
      original_forest_pre_blank <- merged_forest
      merged_forest <- hierarchical_blanker(merged_forest, group_cols)
      row_height <- calculate_dynamic_row_height(merged_forest)
      
      # Create tooltips for SVG
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
      
      display_cols <- c(
        "Study Author Year", "Intervention", "Comparison", 
        "Outcome Domain", "Outcome Measure", "Weeks", "SMD", " ", "border_top"
      )
      merged_forest <- merged_forest[, display_cols, drop = FALSE]
      merged_forest <- tibble::as_tibble(merged_forest)
    }
    
    # Reactable with hover-enabled cells
    rt <- reactable(
      merged_forest,
      columns = list(
        `Study Author Year` = colDef(
          name = "Study Author Year",
          minWidth = 150,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$`Study Author Year`[index]
            create_cell_with_hover(value, original_value, "study", index)
          },
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
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$Intervention[index]
            create_cell_with_hover(value, original_value, "intervention", index)
          },
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
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$Comparison[index]
            create_cell_with_hover(value, original_value, "comparison", index)
          },
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
        `Outcome Domain` = colDef(
          name = "Outcome Domain",
          minWidth = 160,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$`Outcome Domain`[index]
            create_cell_with_hover(value, original_value, "domain", index)
          },
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
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$`Outcome Measure`[index]
            create_cell_with_hover(value, original_value, "measure", index)
          },
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
          width = 70,
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
          width = 80,
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
          html = TRUE,
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
          header = htmltools::HTML(
            make_forest_svg(
              NA, NA, NA, NA, max_n, 400, show_axis = TRUE, tooltip_html = NULL, hover_enabled = FALSE
            )
          ),
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333",
            padding = "0px",
            verticalAlign = "top",
            background = "#fff"
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
        height = "calc(100vh - 400px)",
        overflowY = "auto",
        width = "100%"
      ),
      rowStyle = function(index) {
        list(
          height = paste0(row_height, "px"),
          minHeight = "80px"
        )
      },
      fullWidth = TRUE,
      defaultPageSize = nrow(merged_forest)
    )
    
    # Add JavaScript for hover functionality that integrates with your toggle
    shinyjs::runjs('
    // Add CSS styles for hover tooltips
    if (!document.getElementById("forest-hover-styles")) {
      const style = document.createElement("style");
      style.id = "forest-hover-styles";
      style.textContent = `
        .hover-cell {
          width: 100% !important;
          height: 100% !important;
          position: relative;
          padding: 8px;
          box-sizing: border-box;
        }
        
        .cell-tooltip {
          display: none !important;
          position: fixed !important;
          z-index: 10000 !important;
          background: white !important;
          color: black !important;
          padding: 15px !important;
          border-radius: 8px !important;
          font-size: 13px !important;
          min-width: 200px !important;
          max-width: 500px !important;
          box-shadow: 0 4px 12px rgba(0,0,0,0.3) !important;
          white-space: normal !important;
          line-height: 1.5 !important;
          border: 2px solid #333 !important;
          pointer-events: none !important;
          word-wrap: break-word !important;
        }
        
        /* Show tooltips only when hover is enabled */
        body:not(.hover-disabled) .hover-cell:hover {
          cursor: pointer !important;
        }
        
        body:not(.hover-disabled) .hover-cell:hover .cell-tooltip {
          display: block !important;
        }
        
        /* Hide tooltips when hover is disabled */
        body.hover-disabled .hover-cell {
          cursor: default !important;
        }
        
        body.hover-disabled .cell-tooltip {
          display: none !important;
        }
      `;
      document.head.appendChild(style);
    }
    
    // Position tooltip function
    function positionTooltip(tooltip, event) {
      const viewportWidth = window.innerWidth;
      const viewportHeight = window.innerHeight;
      
      // Make tooltip visible to measure dimensions
      tooltip.style.visibility = "hidden";
      tooltip.style.display = "block";
      tooltip.style.left = "-9999px";
      tooltip.style.top = "-9999px";
      
      const rect = tooltip.getBoundingClientRect();
      const tooltipWidth = rect.width;
      const tooltipHeight = rect.height;
      
      // Calculate position (to the right and above cursor)
      let x = event.clientX + 10;
      let y = event.clientY - tooltipHeight - 10;
      
      // Adjust if tooltip goes off right edge
      if (x + tooltipWidth > viewportWidth) {
        x = event.clientX - tooltipWidth - 10;
      }
      
      // Adjust if tooltip goes off left edge  
      if (x < 10) {
        x = 10;
      }
      
      // Adjust if tooltip goes off top edge
      if (y < 10) {
        y = event.clientY + 20;
      }
      
      // Adjust if tooltip goes off bottom edge
      if (y + tooltipHeight > viewportHeight) {
        y = viewportHeight - tooltipHeight - 10;
      }
      
      tooltip.style.left = x + "px";
      tooltip.style.top = y + "px";
      tooltip.style.visibility = "visible";
    }
    
    // Mouse move event for tooltip positioning
    document.addEventListener("mousemove", function(event) {
      if (!document.body.classList.contains("hover-disabled")) {
        // Find visible cell tooltips
        document.querySelectorAll(".cell-tooltip").forEach(function(tooltip) {
          const computedStyle = window.getComputedStyle(tooltip);
          if (computedStyle.display === "block") {
            const parent = tooltip.closest(".hover-cell");
            if (parent && parent.matches(":hover")) {
              positionTooltip(tooltip, event);
            }
          }
        });
      }
    });
  ')
    
    return(rt)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

