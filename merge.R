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
