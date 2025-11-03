# ============================================================================
# Gapminder Data Processing: Wide to Long Format Conversion and Merging
# ============================================================================
# This script reads four Gapminder CSV files (GDP per capita, GDP, life 
# expectancy, and population), converts them from wide format to long format,
# and merges them into a single dataset using country and year as keys.
# ============================================================================

# Load required libraries
# The tidyverse package includes dplyr, tidyr, and readr which we'll use
library(tidyverse)

# ============================================================================
# STEP 1: Read the CSV files
# ============================================================================
# Read each CSV file into a separate data frame
# The files are in wide format where:
#   - Each row represents a country
#   - The first column is 'geo' (country code)
#   - The second column is 'name' (country name)
#   - Remaining columns are years (1800-2100) with corresponding values

# Read GDP per capita data (21st century focus)
gdp_pcap_21 <- read_csv("/mnt/user-data/uploads/Gapminder_gdp_pcap_21.csv", 
                        show_col_types = FALSE)

# Read GDP per capita data (general)
gdp_pcap <- read_csv("/mnt/user-data/uploads/Gapminder_gdppc.csv", 
                     show_col_types = FALSE)

# Read life expectancy data
life_exp <- read_csv("/mnt/user-data/uploads/Gapminder_lifeexp.csv", 
                     show_col_types = FALSE)

# Read population data
population <- read_csv("/mnt/user-data/uploads/Gapminder_pop.csv", 
                       show_col_types = FALSE)


# ============================================================================
# STEP 2: Convert from Wide to Long Format
# ============================================================================
# The pivot_longer() function transforms the data from wide to long format
# We need to:
#   - Keep 'geo' and 'name' columns as identifiers
#   - Convert all year columns into two new columns:
#     * 'year' column containing the year values
#     * A value column containing the measurement for that year

# Convert GDP per capita (21st century) from wide to long
# cols = -c(geo, name) means "pivot all columns except geo and name"
# names_to = "year" creates a new column called 'year' with the column names
# values_to = "gdp_pcap_21" creates a new column with the values
gdp_pcap_21_long <- gdp_pcap_21 %>%
  pivot_longer(
    cols = -c(geo, name),           # Select all columns except geo and name
    names_to = "year",               # Column names become values in 'year'
    values_to = "gdp_pcap_21"       # Values go into 'gdp_pcap_21' column
  ) %>%
  mutate(year = as.integer(year))   # Convert year from character to integer

# Convert GDP per capita from wide to long
gdp_pcap_long <- gdp_pcap %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to = "year",
    values_to = "gdp_pcap"
  ) %>%
  mutate(year = as.integer(year))

# Convert life expectancy from wide to long
life_exp_long <- life_exp %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to = "year",
    values_to = "life_expectancy"
  ) %>%
  mutate(year = as.integer(year))

# Convert population from wide to long
population_long <- population %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year))


# ============================================================================
# STEP 3: Merge the datasets
# ============================================================================
# We'll use left_join() to merge the datasets sequentially
# The joining keys are:
#   - 'geo' (country code)
#   - 'name' (country name) 
#   - 'year' (year)
#
# We use left_join to keep all rows from the left dataset and add matching
# rows from the right dataset. If there's no match, NA values are inserted.

# Start with life expectancy as the base dataset
gapminder_merged <- life_exp_long %>%
  # Add population data
  left_join(
    population_long,
    by = c("geo", "name", "year")   # Join on these three columns
  ) %>%
  # Add GDP per capita data
  left_join(
    gdp_pcap_long,
    by = c("geo", "name", "year")
  ) %>%
  # Add GDP per capita (21st century) data
  left_join(
    gdp_pcap_21_long,
    by = c("geo", "name", "year")
  ) %>%
  # Arrange the data by country code and year for better readability
  arrange(geo, year)


# ============================================================================
# STEP 4: Display summary information
# ============================================================================

# Print the structure of the merged dataset
cat("\n=== Structure of Merged Dataset ===\n")
str(gapminder_merged)

# Print the first few rows
cat("\n=== First 10 rows of Merged Dataset ===\n")
print(head(gapminder_merged, 10))

# Print summary statistics
cat("\n=== Summary Statistics ===\n")
summary(gapminder_merged)

# Check dimensions
cat("\n=== Dataset Dimensions ===\n")
cat("Number of rows:", nrow(gapminder_merged), "\n")
cat("Number of columns:", ncol(gapminder_merged), "\n")

# Check for missing values
cat("\n=== Missing Values Count ===\n")
print(colSums(is.na(gapminder_merged)))


# ============================================================================
# STEP 5: Save the merged dataset
# ============================================================================
# Save the merged long-format dataset to a CSV file
# This creates a clean, analysis-ready dataset

write_csv(
  gapminder_merged, 
  "/mnt/user-data/outputs/gapminder_merged_long.csv"
)

cat("\n=== Processing Complete ===\n")
cat("Merged dataset saved to: /mnt/user-data/outputs/gapminder_merged_long.csv\n")


# ============================================================================
# OPTIONAL: Create a subset for a specific time period
# ============================================================================
# Example: Create a subset with data from 1960 onwards (common in analyses)

gapminder_modern <- gapminder_merged %>%
  filter(year >= 1960) %>%      # Keep only years from 1960 onwards
  filter(!is.na(life_expectancy))  # Remove rows with missing life expectancy

# Save the modern subset
write_csv(
  gapminder_modern,
  "/mnt/user-data/outputs/gapminder_merged_long_1960_onwards.csv"
)

cat("Modern subset (1960+) saved to: /mnt/user-data/outputs/gapminder_merged_long_1960_onwards.csv\n")
cat("\nNumber of rows in modern subset:", nrow(gapminder_modern), "\n")


# ============================================================================
# BONUS: View a sample of the data for verification
# ============================================================================
# Let's look at data for a few countries in recent years to verify our merge

cat("\n=== Sample Data: Recent years for selected countries ===\n")
gapminder_merged %>%
  filter(year >= 2015, year <= 2020) %>%
  filter(geo %in% c("usa", "chn", "ind", "bra", "deu")) %>%
  arrange(geo, year) %>%
  print(n = 30)

# ============================================================================
# End of script
# ============================================================================
