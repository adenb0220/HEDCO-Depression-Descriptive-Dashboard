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
