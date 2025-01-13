# Consolidate and streamline data pulling and wrangling from MacroSheds data
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(here)
library(stringr)
library(purrr)
library(macrosheds)

# Step 1: Define function to pull MacroSheds data
load_macrosheds_data <- function() {
  # Define directories
  macrosheds_dir <- here('data/macrosheds_data')  # Dedicated MacroSheds directory
  processed_dir <- here('data/Processed')        # Directory for processed data
  
  # Ensure directories exist
  if (!dir.exists(macrosheds_dir)) dir.create(macrosheds_dir, recursive = TRUE)
  if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
  
  # Download MacroSheds data
  ms_download_core_data(
    macrosheds_root = macrosheds_dir,
    domains = 'all',
    quiet = TRUE,
    skip_existing = TRUE  # Skip domains already downloaded
  )
  
  # Load and filter DOC chemistry data
  doc_chem <- ms_load_product(
    macrosheds_root = macrosheds_dir,
    prodname = 'stream_chemistry',
    filter_vars = 'DOC'
  ) %>%
    filter(ms_interp == 0)  # Remove interpolated values
  
  if (nrow(doc_chem) == 0) stop("Error: No DOC data loaded.")
  
  # Load and filter discharge data
  discharge <- ms_load_product(
    macrosheds_root = macrosheds_dir,
    prodname = 'discharge'
  ) %>%
    filter(ms_interp == 0)  # Remove interpolated values
  
  if (nrow(discharge) == 0) stop("Error: No discharge data loaded.")
  
  # Load and filter pH data
  ph_chem <- ms_load_product(
    macrosheds_root = macrosheds_dir,
    prodname = 'stream_chemistry',
    filter_vars = 'pH'
  ) %>%
    filter(ms_interp == 0) %>%  # Remove interpolated values
    rename(ph_val = val)        # Rename 'val' to 'ph_val' for clarity
  
  if (nrow(ph_chem) == 0) stop("Error: No pH data loaded.")
  
  # Ensure date columns are in Date format
  doc_chem$date <- as.Date(doc_chem$date)
  discharge$date <- as.Date(discharge$date)
  ph_chem$date <- as.Date(ph_chem$date)
  
  # Filter datasets to align dates and site codes
  discharge <- discharge %>%
    filter(date %in% doc_chem$date & site_code %in% doc_chem$site_code)
  
  doc_chem <- doc_chem %>%
    filter(date %in% discharge$date & site_code %in% discharge$site_code)
  
  ph_chem <- ph_chem %>%
    filter(date %in% discharge$date & site_code %in% discharge$site_code)
  
  # Combine DOC, discharge, and pH data by date and site_code
  combined_df <- inner_join(doc_chem, discharge, by = c("date", "site_code")) %>%
    left_join(ph_chem, by = c("date", "site_code"))
  
  # Add domain information
  site_data <- ms_load_sites()
  combined_with_domain <- combined_df %>%
    left_join(site_data, by = "site_code")
  
  # Filter for domains in the continental US
  conus_doc_chem <- combined_with_domain %>%
    filter(latitude >= 24.396308 & latitude <= 49.384358 &
             longitude >= -125.0 & longitude <= -66.93457)
  
  return(conus_doc_chem)
}

# Step 2: Add water year
add_water_year <- function(data) {
  data <- data %>%
    mutate(
      water_year = if_else(
        month(date) >= 10, year(date) + 1, year(date)
      )
    )
  return(data)
}

# Step 3: Consolidate data wrangling
process_macrosheds_data <- function() {
  processed_file <- here("data/Processed/processed_macrosheds_data.rds")
  
  # Skip processing if file already exists
  if (file.exists(processed_file)) {
    message("Processed data already exists. Loading from file...")
    return(readRDS(processed_file))
  }
  
  # Step 3a: Load and merge data
  combined_data <- load_macrosheds_data()
  
  # Step 3b: Add water year
  combined_data <- add_water_year(combined_data)
  
  # Step 3c: Save processed data for reuse
  saveRDS(combined_data, file = processed_file)
  message("Data saved to: ", processed_file)
  
  return(combined_data)
}

# Step 4: Run the full data preparation pipeline
macrosheds_data <- process_macrosheds_data()
