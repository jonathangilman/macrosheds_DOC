#install.packages("ggrepel")
library(macrosheds)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(here)
library(patchwork)
library(ggrepel)
library(scales)
library(maps)
library(tidyverse)

help(package = macrosheds)


############# Retrieve DOC data

?ms_load_product
# choose a directory to save data to
my_ms_dir <- here('data')

# retrieve chemistry data
doc_chem <- ms_load_product(
  my_ms_dir,
  prodname = 'stream_chemistry',
  filter_vars = 'DOC'
)

# load sites
?ms_load_sites
site_data <- ms_load_sites()

# add domain to doc_chem
doc_chem <- doc_chem %>%
  left_join(site_data, by = "site_code")

#filter for domains in continental US
conus_doc_chem <- doc_chem %>%
  filter(latitude >= 24.396308 & latitude <= 49.384358 &
           longitude >= -125.0 & longitude <= -66.93457)
unique(conus_doc_chem$domain)

# retrieve pH data
ph_chem <- ms_load_product(
  my_ms_dir,
  prodname = 'stream_chemistry',
  filter_vars = 'pH'
)

# add domain to ph_chem
ph_chem <- ph_chem %>%
  left_join(site_data, by = "site_code")

# Rename 'val' to 'ph_val' in ph_chem
ph_chem <- ph_chem %>%
  rename(ph_val = val)

# Rename 'val' to 'doc_val' in conus_doc_chem
conus_doc_chem <- conus_doc_chem %>%
  rename(doc_val = val)

#### join pH data the conus_doc_chem
# Keeping all historical pH data but only include domains included in conus_doc_chem

# Perform a full join to combine both doc_val and ph_val, keeping all data
combined_data <- full_join(conus_doc_chem, ph_chem, by = c("date", "domain"))

# Get the list of the 16 unique domains in conus_doc_chem
relevant_domains <- unique(conus_doc_chem$domain)

# Filter combined_data to include only rows from those domains
filtered_combined_data <- combined_data %>%
  filter(domain %in% relevant_domains)

# Perform a full join to retain all rows, allowing NAs for missing doc_val or ph_val
final_combined_data <- full_join(
  conus_doc_chem, 
  ph_chem %>% select(date, domain, ph_val), 
  by = c("date", "domain")
)
# Filter the final combined data to only include the relevant 16 domains
final_combined_data <- final_combined_data %>%
  filter(domain %in% relevant_domains)
unique(final_combined_data$domain)

#########################################################################

# Calculate VWM DOC for each site and water year

# Step 1: Group by domain and date, and calculate the VWM DOC (Volume Weighted Mean DOC) for each
# The formula for VWM DOC is the sum of (DOC * Discharge) / sum of Discharge for the corresponding water year.

final_combined_data <- final_combined_data %>%
  mutate(
    water_year = if_else(month(date) >= 10, year(date) + 1, year(date))  # Add 1 to the year for months Oct-Dec
  )

vwm_doc_data <- final_combined_data %>%
  group_by(domain, site_code, water_year) %>%
  summarize(
    vwm_doc = sum(doc_val * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # VWM DOC calculation
    avg_q = mean(val.y, na.rm = TRUE),  # Calculate average discharge for each site and water year
    avg_ph = mean(ph_val, na.rm = TRUE),  # Calculate average pH for each site and water year
    .groups = "drop"
  )

# Step 2: Plot DOC and pH with VWM DOC for each domain over time

# Create a vectorized function to calculate the water year
water_year <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  # Water year starts in October, so adjust the year accordingly
  dplyr::if_else(month >= 10, year, year - 1)
}

# Add water year to the dataset
vwm_doc_data <- vwm_doc_data %>%
  mutate(water_year = water_year(date))

# Plot the VWM DOC data over time, with separate lines for DOC and pH, facet by domain
doc_with_ph_vwm <- ggplot(vwm_doc_data, aes(x = water_year)) +
  geom_line(aes(y = vwm_doc, color = "VWM DOC"), linewidth = 1) +    # Line for VWM DOC (left y-axis)
  geom_line(aes(y = avg_ph, color = "pH"), linewidth = 1) +      # Line for pH (right y-axis)
  facet_wrap(~ domain, scales = "free_y") +                      # Facet by domain
  labs(x = "Water Year", y = "VWM DOC (mg/L)", title = "VWM DOC and pH over Time by Domain") +
  theme_minimal() +
  scale_color_manual(name = "Variable", values = c("VWM DOC" = "blue", "pH" = "red")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(                                     # Primary y-axis for VWM DOC
    sec.axis = sec_axis(~ ., name = "pH")                 # Secondary y-axis for pH with no scaling
  )

doc_with_ph_vwm

######################################################

# Water year VWM DOC and pH across time, ordered by the number of DOC measurements

# Step 1: Add water year to the dataset
vwm_doc_data <- vwm_doc_data %>%
  mutate(water_year = water_year(date))

# Step 2: Count the number of VWM DOC measurements and the number of sites per domain
domain_stats_vwm <- vwm_doc_data %>%
  group_by(domain) %>%
  summarize(
    vwm_doc_count = sum(!is.na(vwm_doc)),  # Count of VWM DOC measurements
    site_count = n_distinct(site_code)  # Number of unique sites
  )

# Step 3: Add domain statistics to the dataset before averaging
vwm_doc_data <- vwm_doc_data %>%
  left_join(domain_stats_vwm, by = "domain")  # Join the domain statistics

# Step 4: Reorder the domains based on the number of VWM DOC measurements
vwm_doc_data <- vwm_doc_data %>%
  mutate(domain = forcats::fct_reorder(domain, vwm_doc_count, .desc = TRUE))  # Reorder by VWM DOC count

# Step 5: Group by domain and water_year, and calculate the average values for vwm_doc and ph_val
avg_data_vwm <- vwm_doc_data %>%
  group_by(domain, water_year) %>%
  summarize(
    avg_vwm_doc = mean(vwm_doc, na.rm = TRUE),
    avg_ph = mean(ph_val, na.rm = TRUE),
    site_count = first(site_count)  # Retain the number of sites in each domain
  )

# Step 6: Plot VWM DOC and pH by water year with "sites=__" in each facet
doc_with_ph_vwm_water_year <- ggplot(avg_data_vwm, aes(x = water_year)) +
  geom_line(aes(y = avg_vwm_doc, color = "VWM DOC"), linewidth = 0.75) +    # Line for VWM DOC (left y-axis)
  geom_line(aes(y = avg_ph, color = "pH"), linewidth = 0.75) +      # Line for pH (right y-axis)
  facet_wrap(~ domain, scales = "free_y") +                      # Facet by domain
  labs(x = "Water Year", y = "VWM DOC (mg/L)", title = "VWM DOC and pH by Water Year") +
  theme_minimal() +
  scale_color_manual(name = "Variable", values = c("VWM DOC" = "black", "pH" = "red")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(                                     # Primary y-axis for VWM DOC
    sec.axis = sec_axis(~ ., name = "pH")                 # Secondary y-axis for pH with no scaling
  ) +
  theme(panel.grid = element_blank(),  # Remove gridlines 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)  # Add a black border around each facet
  ) +
  # Step 7: Add "sites=__" in the top left of each facet
  geom_text(aes(x = -Inf, y = Inf, label = paste("sites =", site_count)), 
            hjust = -0.1, vjust = 1.5, size = 2.3, color = "black")  # Position text in top left

# Display the plot
doc_with_ph_vwm_water_year
# Save the plot using the here package
ggsave(here("figures/temporal_analyses/doc_with_pH_vwm_water_year.png"), plot = doc_with_ph_vwm_water_year, width = 8, height = 6, dpi = 300)



##########################################################################################
####### lines on scatterplot


# Retrieve discharge data and filter out interpolated values
discharge <- ms_load_product(
  my_ms_dir,
  prodname = 'discharge'
)
filtered_discharge <- discharge %>%
  filter(ms_interp == 0)

#################### Merge pH and Discharge ##########################

# Ensure the date columns are in Date format
ph_chem$date <- as.Date(ph_chem$date)
filtered_discharge$date <- as.Date(filtered_discharge$date)

# Filter filtered_discharge to keep only dates and site_codes present in ph_chem
filtered_discharge <- filtered_discharge %>%
  filter(date %in% ph_chem$date & site_code %in% ph_chem$site_code)

# Filter ph_chem to keep only dates and site_codes present in filtered_discharge
filtered_ph <- ph_chem %>%
  filter(date %in% filtered_discharge$date & site_code %in% filtered_discharge$site_code)

# Combine both dataframes based on the date and site_code columns
combined_ph <- inner_join(filtered_ph, filtered_discharge, by = c("date", "site_code"))

# Verify combined data
print(head(combined_ph))

# Add water year to the combined dataset
combined_ph <- combined_ph %>%
  mutate(water_year = if_else(month(date) >= 10, year(date) + 1, year(date)))

# Calculate VWM pH
vwm_ph <- combined_ph %>%
  group_by(site_code, water_year) %>%
  summarize(
    VWM_pH = sum(ph_val * val, na.rm = TRUE) / sum(val, na.rm = TRUE),  # VWM pH
    Avg_Q = mean(val, na.rm = TRUE),  # Average discharge
    .groups = "drop"
  )

# Step 3: Determine significant trends for pH
significant_trends <- vwm_ph %>%
  group_by(site_code) %>%
  summarize(
    p_value = tryCatch(
      summary(lm(VWM_pH ~ water_year))$coefficients[2, 4],  # p-value for the slope
      error = function(e) NA  # Handle errors gracefully
    ),
    slope = tryCatch(
      summary(lm(VWM_pH ~ water_year))$coefficients[2, 1],  # Slope of the trendline
      error = function(e) NA  # Handle errors gracefully
    )
  ) %>%
  mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance threshold of 0.05

# Merge trend significance back into the VWM pH data
vwm_ph <- vwm_ph %>%
  left_join(significant_trends, by = "site_code")

# Step 4: Add site metadata
vwm_ph <- vwm_ph %>%
  left_join(
    ph_chem %>%
      select(site_code, site_fullname, domain) %>%
      distinct(),
    by = "site_code"
  )

# Step 5: Identify the last points for each significant trendline
last_points <- vwm_ph %>%
  filter(significant == TRUE) %>%
  group_by(site_code) %>%
  slice_tail(n = 1) %>%  # Get the last point of each trendline
  ungroup() %>%
  select(site_code, site_fullname, domain, water_year, VWM_pH)  # Relevant columns

# Step 6: Generate a scatterplot with trendlines for significant trends
ph_trend_plot <- ggplot(vwm_ph, aes(x = water_year, y = VWM_pH, group = site_code)) +
  geom_point(aes(color = significant), alpha = 0.6, size = 2) +  # Points colored by significance
  geom_smooth(
    data = subset(vwm_ph, significant == TRUE),
    method = "lm", se = FALSE, color = "blue", linewidth = 0.75
  ) +  # Trendlines for significant trends
  scale_color_manual(values = c("grey", "black")) +  # Grey for non-significant, black for significant
  labs(
    x = "Water Year",
    y = "VWM pH",
    color = "Trend Significance",
    title = "Scatterplot of VWM pH Trends Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  ) +
  # Add the number of significant trends in the bottom-left corner
  annotate("text", x = min(vwm_ph$water_year, na.rm = TRUE), y = min(vwm_ph$VWM_pH, na.rm = TRUE),
           label = paste0(nrow(last_points), " sites with significant trends"),
           hjust = 0, vjust = 0, size = 4, color = "blue")

# Step 7: Save and display the plot
ggsave(here("figures/Q/ph_trend_plot.png"), plot = ph_trend_plot, width = 9, height = 7, dpi = 300)
print(ph_trend_plot)

