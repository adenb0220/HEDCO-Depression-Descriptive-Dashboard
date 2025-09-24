#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, DT, shiny, plotly, openxlsx, countrycode, forestplot,
               reactable, htmltools, stringi)

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

table(merged$yi, useNA="ifany")

merged %>% 
  select(percent_white) %>% 
  print()

table(studies$percent_female)

table(merged$outcome_measure)


clean_urbanicity <- function(x) {
  # Handle NA or blank cases
  if (is.na(x) || trimws(x) == "") return("Unclear")
  # Handle explicit unclear/cannot tell
  if (tolower(trimws(x)) %in% c("cannot tell", "unclear")) return("Unclear")
  # Lowercase for matching
  x <- tolower(x)
  # Remove numbers, dots, and linebreaks/commas/semicolons
  x <- gsub("[0-9]+\\.", "", x)
  x <- gsub("[\r\n]+", ",", x)
  # Split on comma, semicolon, or space
  parts <- unlist(strsplit(x, "[,;]+"))
  # Trim whitespace
  parts <- trimws(parts)
  # Only keep valid values
  possible <- c("rural", "suburban", "urban")
  found <- unique(parts[parts %in% possible])
  if (length(found) == 0) return("Unclear")
  paste(sort(found), collapse = "+")
}

# Test cases
sapply(c(
  "1. Rural",
  "3. Urban",
  "2. Suburban",
  "1. Rural, 3. Urban",
  "1. Rural 1. Rural \r\n2. Suburban\r\n3. Urban",
  "1. Rural, 2. Suburban",
  "1. Rural \r\n3. Urban",
  "cannot tell",
  "Unclear",
  "",
  "3. Urban\r\n1. Rural"
), clean_urbanicity)


merged %>% 
  filter(country == "Hong Kong")










table(merged$percent_white)

merged %>% 
  group_by(study) %>% 
  slice(1) %>%  # Take first row of each study
  ungroup() %>%
  filter(percent_black > -1) %>% 
  pull(percent_black) %>%
  mean(na.rm = TRUE)

table(merged$percent_black)
table(merged$percent_aian)
table(merged$percent_nhpi)
table(merged$percent_asian)
table(merged$percent_latinx)
table(merged$percent_other)

##############################################
table(merged$school_level)
# Load required libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Classification function based on grade levels
classify_school_level <- function(grade_str) {
  if (is.na(grade_str) || tolower(grade_str) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  
  # Clean the string and extract numbers
  grade_str <- gsub("[^0-9,]", "", grade_str)
  grades <- as.numeric(unlist(strsplit(grade_str, ",")))
  grades <- grades[!is.na(grades)]
  
  if (length(grades) == 0) return("Unclear")
  
  min_grade <- min(grades)
  max_grade <- max(grades)
  
  # Classification logic
  if (min_grade >= 1 && max_grade <= 5) return("Elementary")
  if (min_grade >= 6 && max_grade <= 8) return("Middle") 
  if (min_grade >= 9 && max_grade <= 12) return("High")
  if (min_grade <= 5 && max_grade >= 6 && max_grade <= 8) return("Elementary + Middle")
  if (min_grade >= 6 && max_grade >= 9) return("Middle + High")
  return("Unclear")
}

# Color scale
green_scale_plotly <- c(
  "#8ABB40",  
  "#489D46",  
  "#007030",  
  "#104735"
)

# Process data by study, not by row
plot_data <- merged %>%
  # Group by study and get one grade_level per study
  group_by(study) %>%
  summarise(grade_level = first(grade_level), .groups = "drop") %>%
  # Classify school levels
  mutate(school_level_classified = sapply(grade_level, classify_school_level)) %>%
  # Count studies by school level
  count(school_level_classified, name = "count") %>%
  mutate(
    # Order levels for display
    school_level_classified = factor(
      school_level_classified,
      levels = c("Unclear", "High", "Middle + High", "Middle", 
                 "Elementary + Middle", "Elementary")
    ),
    hover = sprintf(
      "School Level: <b>%s</b><br>Number of Studies: <b>%d</b>",
      school_level_classified, count
    )
  )

# Create the plot
school_level_plot <- ggplot(plot_data, aes(
  x = school_level_classified,
  y = count,
  fill = count,
  text = hover
)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradientn(colors = green_scale_plotly) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  labs(
    title = "Distribution of Studies by School Level",
    x = "School Level", 
    y = "Number of Studies"
  )

# Convert to plotly and display
plotly_plot <- ggplotly(school_level_plot, tooltip = "text") %>%
  layout(
    margin = list(l = 150, r = 20, t = 60, b = 50),
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black", size = 12)
    )
  ) %>%
  layout(dragmode = FALSE) %>%
  config(displayModeBar = FALSE)

# Display the plot
plotly_plot

# Check the counts to see if they match the target
print("Study counts by school level:")
print(table(plot_data$school_level_classified))

###########################
# IMPROVED GRADE LEVEL CLASSIFICATION FUNCTION
###########################

# Grade level creation - Updated scheme (K-8, 9-12, K-12)
classify_grade_level <- function(x) {
  # Store original value for debugging
  original_x <- x
  
  # Clean up input
  x <- gsub(" ", "", x)
  
  # Handle unclear cases first
  if (is.na(x) || x == "" || tolower(x) %in% c("cannot tell", "unclear", "na")) {
    return("Unclear")
  }
  
  # Handle kindergarten - convert K to 0
  x <- gsub("K", "0", x, ignore.case = TRUE)
  
  # Split by common separators (comma, semicolon, dash)
  grades_split <- unlist(strsplit(x, "[,;-]"))
  
  # Remove any empty strings
  grades_split <- grades_split[grades_split != ""]
  
  # Try to convert to numeric
  grades_num <- suppressWarnings(as.numeric(grades_split))
  
  # If any conversion failed, return Unclear
  if (any(is.na(grades_num))) {
    return("Unclear")
  }
  
  # Get range
  min_g <- min(grades_num)
  max_g <- max(grades_num)
  
  # New classification scheme
  if (min_g >= 0 && max_g <= 8) {
    return("K-8")      # Kindergarten through 8th grade
  } else if (min_g >= 9 && max_g <= 12) {
    return("9-12")     # 9th through 12th grade
  } else if (min_g <= 8 && max_g >= 9) {
    return("K-12")     # Spans from K-8 into 9-12
  } else {
    return("Unclear")
  }
}

# Apply the classification to your data
merged <- merged %>%
  mutate(grade_category = sapply(grade_level, classify_grade_level))

# Create school_level_choices for any future use
school_level_choices <- merged %>%
  distinct(grade_category) %>%
  pull(grade_category) %>%
  na.omit() %>%
  unique() %>%
  as.character()

# Order them logically
school_level_choices <- intersect(
  c("K-8", "9-12", "K-12", "Unclear"),
  school_level_choices
)

###########################
# COMPREHENSIVE DIAGNOSTIC CODE
###########################

cat("=== GRADE LEVEL CLASSIFICATION DIAGNOSTICS ===\n")

# 1. Test the classify_grade_level function with various inputs
cat("\n1. Testing classify_grade_level function:\n")
cat("==========================================\n")
test_cases <- c(
  "K,1,2,3",           # Should be K-8
  "0,1,2,3",           # Should be K-8 (if K converted to 0)
  "6,7,8",             # Should be K-8
  "9,10,11,12",        # Should be 9-12
  "K,1,2,3,4,5,6,7,8", # Should be K-8
  "K,1,2,3,9,10",      # Should be K-12 (spans both)
  "6,7,8,9,10",        # Should be K-12 (spans both)
  "1,2,3,4,5,6,7,8,9,10,11,12", # Should be K-12
  "unclear",           # Should be Unclear
  "cannot tell",       # Should be Unclear
  "",                  # Should be Unclear
  "nonsense",          # Should be Unclear
  "K-5",               # Should be K-8 (testing dash separator)
  "9-12"               # Should be 9-12 (testing dash separator)
)

for (test in test_cases) {
  result <- classify_grade_level(test)
  cat(sprintf("  Input: '%-25s' -> Output: '%s'\n", test, result))
}

# 2. Check your actual data
cat("\n2. Checking actual data:\n")
cat("========================\n")

# Show unique values in your grade_level column
cat("Unique values in grade_level column:\n")
unique_grades <- sort(unique(merged$grade_level))
for (i in seq_along(unique_grades)) {
  cat(sprintf("  %2d. '%s'\n", i, unique_grades[i]))
}

# Show the mapping
cat("\n3. Grade Level to Category Mapping:\n")
cat("===================================\n")
grade_mapping <- merged %>%
  distinct(grade_level, grade_category) %>%
  arrange(grade_category, grade_level)

for (i in 1:nrow(grade_mapping)) {
  cat(sprintf("  '%-20s' -> '%s'\n", 
              grade_mapping$grade_level[i], 
              grade_mapping$grade_category[i]))
}

# Count studies by category
cat("\n4. Study counts by grade category:\n")
cat("==================================\n")
category_counts <- merged %>%
  group_by(grade_category) %>%
  summarise(
    n_studies = n_distinct(study),
    n_rows = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_studies))

for (i in 1:nrow(category_counts)) {
  cat(sprintf("  %-10s: %3d studies (%d total rows)\n",
              category_counts$grade_category[i],
              category_counts$n_studies[i],
              category_counts$n_rows[i]))
}

# Check for issues
cat("\n5. Data Quality Checks:\n")
cat("=======================\n")

# Check for NA values
na_count <- sum(is.na(merged$grade_category))
if (na_count > 0) {
  cat(sprintf("  ⚠️  WARNING: Found %d NA values in grade_category\n", na_count))
  
  # Show which grade_levels produced NAs
  na_grades <- merged %>%
    filter(is.na(grade_category)) %>%
    distinct(grade_level) %>%
    pull(grade_level)
  cat("     Grade levels that produced NAs:\n")
  for (grade in na_grades) {
    cat(sprintf("       '%s'\n", grade))
  }
} else {
  cat("  ✓ No NA values found in grade_category\n")
}

# Check for unclear classifications
unclear_count <- sum(merged$grade_category == "Unclear", na.rm = TRUE)
unclear_pct <- round(unclear_count / nrow(merged) * 100, 1)

if (unclear_count > 0) {
  cat(sprintf("  ℹ️  Info: %d rows (%.1f%%) classified as 'Unclear'\n", unclear_count, unclear_pct))
  
  unclear_grades <- merged %>%
    filter(grade_category == "Unclear") %>%
    distinct(grade_level) %>%
    arrange(grade_level) %>%
    pull(grade_level)
  
  cat("     Grade levels classified as 'Unclear':\n")
  for (grade in head(unclear_grades, 10)) {  # Show first 10
    cat(sprintf("       '%s'\n", grade))
  }
  if (length(unclear_grades) > 10) {
    cat(sprintf("       ... and %d more\n", length(unclear_grades) - 10))
  }
} else {
  cat("  ✓ No unclear classifications found\n")
}

# Sample of studies with their classifications
cat("\n6. Sample of studies with classifications:\n")
cat("==========================================\n")
sample_studies <- merged %>%
  select(study, grade_level, grade_category) %>%
  distinct() %>%
  slice_head(n = 10)

for (i in 1:nrow(sample_studies)) {
  cat(sprintf("  Study: %-30s | Grade: %-15s | Category: %s\n",
              substr(sample_studies$study[i], 1, 30),
              substr(sample_studies$grade_level[i], 1, 15),
              sample_studies$grade_category[i]))
}

# School level choices for future use
cat("\n7. School level choices available:\n")
cat("==================================\n")
cat("  Available categories:", paste(sort(school_level_choices), collapse = ", "), "\n")

cat("\n=== DIAGNOSTIC COMPLETE ===\n")

# Create summary data frame for easy viewing
grade_diagnostic_summary <- merged %>%
  group_by(grade_level, grade_category) %>%
  summarise(
    n_studies = n_distinct(study),
    n_rows = n(),
    example_studies = paste(head(unique(study), 3), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(grade_category, desc(n_studies))

cat("\nCreated 'grade_diagnostic_summary' data frame for detailed review\n")
cat("Use View(grade_diagnostic_summary) to examine results in detail\n")

# Quick summary table
cat("\nQuick Summary:\n")
cat("==============\n")
print(table(merged$grade_category, useNA = "ifany"))

#########################################################
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

  # 1. Process interventions and assign the family/group
  merged$processed_intervention_roots <- process_intervention_roots(merged$intervention)
  
  # 2. Get the unique family/group names (these should be the grouped ones!)
  intervention_family_choices <- merged %>%
    distinct(processed_intervention_roots) %>%
    pull(processed_intervention_roots) %>%
    sort()
  
  # 3. Print to verify
  print(intervention_family_choices)
  
  