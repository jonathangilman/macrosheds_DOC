# Analyze MacroSheds data
# Load libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(here)
library(grid)
library(maps)
library(kableExtra)


############################################
# STEP 1: Load and Filter Data
############################################

# Load processed data
processed_data <- readRDS(here("data/Processed/processed_macrosheds_data.rds"))

# Step 1: Count the number of observations per site, year, and month
data_with_counts <- processed_data %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(site_code, year, month) %>%
  summarize(obs_per_month = n(), .groups = "drop")

# Step 2: Apply the filtering criterion - Minimum 1 observation per month for at least 10 months in a year
filtered_1_obs <- data_with_counts %>%
  filter(obs_per_month >= 1) %>%
  group_by(site_code, year) %>%
  summarize(months_with_1_obs = n_distinct(month), .groups = "drop") %>%
  filter(months_with_1_obs >= 10)

# Step 3: Join the filtered years back to the main dataset
filtered_data_1_obs <- processed_data %>%
  mutate(year = year(date)) %>%
  semi_join(filtered_1_obs, by = c("site_code", "year"))

# Step 4: Ensure water_year column is correctly retained
filtered_data_1_obs <- filtered_data_1_obs %>%
  mutate(water_year = if_else(month(date) >= 10, year(date) + 1, year(date)))

############################################
# STEP 2: Calculate VWM DOC and Analyze Trends
############################################

# Calculate Volume Weighted Mean (VWM) DOC for each site and water year
vwm_doc <- filtered_data_1_obs %>%
  group_by(site_code, water_year) %>%
  summarize(
    VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
    Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge
    .groups = "drop"
  )

# Step 2: Determine significant trends for VWM DOC
significant_trends <- vwm_doc %>%
  group_by(site_code) %>%
  summarize(
    p_value = tryCatch(
      summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4],
      error = function(e) NA  # Handle errors gracefully
    ),
    slope = tryCatch(
      summary(lm(VWM_DOC ~ water_year))$coefficients[2, 1],
      error = function(e) NA
    ),
    .groups = "drop"
  ) %>%
  mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance threshold of 0.05

# Merge significance data back into the VWM DOC data
vwm_doc <- vwm_doc %>%
  left_join(significant_trends, by = "site_code")

############################################
# STEP 3: Add Site Metadata
############################################

# Join metadata (site_fullname, domain, ws_status) into the VWM DOC data
vwm_doc <- vwm_doc %>%
  left_join(
    filtered_data_1_obs %>%
      select(site_code, site_fullname, domain, ws_status) %>%
      distinct(),
    by = "site_code"
  )

############################################
# STEP 4: Identify Significant Trends
############################################

# Extract the last points for significant trendlines
last_points <- vwm_doc %>%
  filter(significant == TRUE) %>%
  group_by(site_code) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(site_code, site_fullname, domain, water_year, VWM_DOC)


# Generate a list of significant sites with slopes and p-values
significant_sites_list <- vwm_doc %>%
  filter(significant == TRUE) %>%
  group_by(site_code) %>%
  summarize(
    slope = first(slope),
    p_value = first(p_value),
    domain = first(domain),
    .groups = "drop"
  )

# Create a clean table for the console
significant_sites_list %>%
  kable(
    format = "latex",  # Ensures LaTeX output
    col.names = c("Site Code", "Slope", "P-value", "Domain"),
    caption = "Significant Trends by Site"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )


############################################
# STEP 5: Create Scatterplot with Trends
############################################

# Calculate the total number of unique sites
n_total_sites <- vwm_doc %>%
  distinct(site_code) %>%
  nrow()

# Calculate the number of sites with significant trends
n_significant_sites <- vwm_doc %>%
  filter(significant == TRUE) %>%
  distinct(site_code) %>%
  nrow()

# Print the numbers for verification
print(paste("Total sites:", n_total_sites))
print(paste("Significant sites:", n_significant_sites))

# Scatterplot for VWM DOC trends
scatterplot_vwm_doc <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
  geom_point(aes(color = significant), alpha = 0.6, size = 2) +
  scale_y_log10(labels = scales::label_number()) +
  geom_smooth(
    data = subset(vwm_doc, significant == TRUE),
    method = "lm", se = FALSE, color = "blue", linewidth = 0.75
  ) +
  scale_color_manual(values = c("grey", "black")) +
  labs(
    x = "Water Year",
    y = "Log VWM DOC (mg/L)",
    title = "Scatterplot of VWM DOC Trends Over Time",
    color = "Trend Significance"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  ) +
  annotate("text", x = min(vwm_doc$water_year), y = 0.1,
           label = paste(n_total_sites, "sites,", n_significant_sites, "significant"),
           hjust = 0, vjust = 0, size = 4, color = "blue")

# Save scatterplot
ggsave(here("figures/analysis_figs/sig_trend.png"), plot = scatterplot_vwm_doc, width = 9, height = 7, dpi = 300)

############################################
# STEP 6: Scatterplot for Experimental vs Non-Experimental Sites
############################################

scatterplot_experimental <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
  geom_point(aes(color = ifelse(significant, "black", "grey")), alpha = 0.6, size = 2) +
  scale_y_log10(labels = scales::label_number()) +
  geom_smooth(
    data = subset(vwm_doc, significant == TRUE),
    method = "lm", se = FALSE,
    aes(color = ifelse(ws_status == "experimental", "darkolivegreen3", "blue")),
    linewidth = 1.0
  ) +
  scale_color_manual(values = c("black", "blue", "darkolivegreen3", "grey")) +
  labs(
    x = "Water Year",
    y = "Log VWM DOC (mg/L)",
    title = "VWM DOC Trends for Experimental vs Non-Experimental Sites"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.title = element_text(size = 14)
  ) +
  facet_wrap(~ ws_status, scales = "free_y")

# Save experimental plot
ggsave(here("figures/analysis_figs/sig_trend_exp.png"), plot = scatterplot_experimental, width = 9, height = 6, dpi = 300)

# Generate a list of significant sites with slopes and p-values for experimental vs non-experimental
significant_sites_exp_vs_nonexp <- vwm_doc %>%
  filter(significant == TRUE) %>%
  group_by(site_code) %>%
  summarize(
    slope = first(slope),
    p_value = first(p_value),
    domain = first(domain),
    ws_status = first(ws_status),  # Include experimental vs non-experimental status
    .groups = "drop"
  )

# Create a clean table for the console with experimental vs non-experimental grouping
significant_sites_exp_vs_nonexp %>%
  kable(
    format = "latex",  # Ensures LaTeX output
    col.names = c("Site Code", "Slope", "P-value", "Domain", "Watershed Status"),
    caption = "Significant Trends by Site (Experimental vs Non-Experimental)"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )


############################################
# STEP 7: Perform pH Analysis with Separate Filtering
############################################

# Step 1: Filter for valid pH data
filtered_data_with_ph <- processed_data %>%
  filter(!is.na(ph_val))  # Keep only rows with valid pH data

# Step 2: Apply filtering criteria for pH data
# Derive `year` and `month` from the `date` column
filtered_data_with_ph <- filtered_data_with_ph %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  )

# Criterion: Minimum 1 observation per month for at least 10 months in a year
data_with_counts_ph <- filtered_data_with_ph %>%
  group_by(site_code, year, month) %>%
  summarize(obs_per_month = n(), .groups = "drop")

filtered_ph <- data_with_counts_ph %>%
  filter(obs_per_month >= 1) %>%
  group_by(site_code, year) %>%
  summarize(months_with_1_obs = n_distinct(month), .groups = "drop") %>%
  filter(months_with_1_obs >= 10)

filtered_data_with_ph <- filtered_data_with_ph %>%
  semi_join(filtered_ph, by = c("site_code", "year"))

# Step 3: Calculate Volume Weighted Mean (VWM) pH for each site and water year
vwm_ph <- filtered_data_with_ph %>%
  group_by(site_code, water_year) %>%
  summarize(
    VWM_pH = sum(ph_val * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # VWM pH
    Avg_Q = mean(val.y, na.rm = TRUE),  # Average discharge
    .groups = "drop"
  )

# Step 4: Perform trend analysis for VWM pH over time
significant_ph_trends <- vwm_ph %>%
  group_by(site_code) %>%
  summarize(
    p_value = tryCatch(
      summary(lm(VWM_pH ~ water_year))$coefficients[2, 4],  # p-value for slope
      error = function(e) NA
    ),
    slope = tryCatch(
      summary(lm(VWM_pH ~ water_year))$coefficients[2, 1],  # Slope of the trendline
      error = function(e) NA
    ),
    .groups = "drop"
  ) %>%
  mutate(significant = !is.na(p_value) & p_value < 0.05)  # Mark significant trends

# Step 5: Merge trend analysis back into the VWM pH data
vwm_ph <- vwm_ph %>%
  left_join(significant_ph_trends, by = "site_code") %>%
  left_join(
    filtered_data_with_ph %>%
      select(site_code, site_fullname, domain) %>%
      distinct(),
    by = "site_code"
  )

# Step 6: Count the number of sites with significant trends
n_significant_sites <- vwm_ph %>%
  filter(significant == TRUE) %>%
  distinct(site_code) %>%
  count() %>%
  pull(n)

# Print the number of significant sites
print(paste(n_significant_sites, "sites with significant trends"))

############################################
# Plot VWM pH Trends
############################################

ph_trend_plot <- ggplot(vwm_ph, aes(x = water_year, y = VWM_pH, group = site_code)) +
  geom_point(aes(color = significant), alpha = 0.6, size = 2) +
  geom_smooth(
    data = subset(vwm_ph, significant == TRUE),
    method = "lm", se = FALSE, color = "blue", linewidth = 0.75
  ) +
  scale_color_manual(values = c("grey", "black")) +
  labs(
    x = "Water Year",
    y = "VWM pH",
    color = "Trend Significance",
    title = "VWM pH Trends Over Time"
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
  annotate(
    "text",
    x = min(vwm_ph$water_year, na.rm = TRUE), 
    y = min(vwm_ph$VWM_pH, na.rm = TRUE), 
    label = paste(n_significant_sites, "sites with significant trends"),
    hjust = 0, vjust = 0, size = 4, color = "blue"
  )

# Save and display the plot
ggsave(here("figures/analysis_figs/ph_trend_plot_separate_filter.png"), plot = ph_trend_plot, width = 9, height = 7, dpi = 300)
print(ph_trend_plot)


############################################
# Add ws_status to vwm_ph
############################################

# Join ws_status from processed_data into vwm_ph
vwm_ph <- vwm_ph %>%
  left_join(
    processed_data %>%
      select(site_code, ws_status) %>%
      distinct(),
    by = "site_code"
  )

# Check if ws_status is now in vwm_ph
print("Columns in vwm_ph after adding ws_status:")
print(colnames(vwm_ph))

############################################
# Create a Table of Significant Trends
############################################

# Filter significant trends and select relevant columns
significant_trends_table <- vwm_ph %>%
  filter(significant == TRUE) %>%
  select(
    domain,          # Domain
    site_fullname,   # Site name
    p_value,         # P-value of the trend
    slope,           # Slope of the trendline
    ws_status        # Watershed status
  ) %>%
  distinct() %>%   # Ensure no duplicates
  arrange(domain, site_fullname)  # Sort by domain and site name

############################################
# Display the Table
############################################

library(kableExtra)

significant_trends_table %>%
  kable(
    format = "html",  # Format for rendering
    col.names = c("Domain", "Site", "P-value", "Slope", "Watershed Status"),
    caption = "Table of Significant VWM pH Trends"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

############################################
# Save the Table to a CSV File
############################################

write.csv(significant_trends_table, here("figures/tables/significant_trends_pH.csv"), row.names = FALSE)


############################################
# Filter for Sites with Both Significant pH and DOC Trends
############################################

# Identify sites where both pH and DOC are significant
significant_sites_both <- intersect(
  vwm_ph %>% filter(significant == TRUE) %>% pull(site_code),
  vwm_doc %>% filter(significant == TRUE) %>% pull(site_code)
)

# Filter the pH data for the plot
ph_plot_data <- vwm_ph %>%
  filter(site_code %in% significant_sites_both) %>%
  mutate(variable = "VWM pH") %>%
  rename(value = VWM_pH)

# Filter the DOC data for the plot
doc_plot_data <- vwm_doc %>%
  filter(site_code %in% significant_sites_both) %>%
  mutate(variable = "VWM DOC") %>%
  rename(value = VWM_DOC)

# Combine pH and DOC data
combined_plot_data <- bind_rows(ph_plot_data, doc_plot_data)

############################################
# Create the Unified Faceted Plot
############################################

unified_trend_plot <- ggplot(combined_plot_data, aes(x = water_year, y = value, group = site_code)) +
  geom_point(aes(color = significant), alpha = 0.6, size = 2) +
  geom_smooth(
    data = subset(combined_plot_data, significant == TRUE),
    method = "lm", se = FALSE, color = "blue", linewidth = 0.75
  ) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +  # Two rows: VWM pH on top, VWM DOC on bottom
  scale_color_manual(values = c("grey", "black")) +
  labs(
    x = "Water Year",
    y = "Value",
    color = "Trend Significance",
    title = "Unified Faceted Scatterplot of VWM pH and DOC Trends (Both Significant)"
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
  )

############################################
# Save and Display the Plot
############################################

ggsave(here("figures/Q/unified_trend_plot_both_sig.png"), plot = unified_trend_plot, width = 10, height = 8, dpi = 300)
print(unified_trend_plot)



