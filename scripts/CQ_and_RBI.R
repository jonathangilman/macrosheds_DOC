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
library(sf)

help(package = macrosheds)

# load sites
?ms_load_sites
ms_sites <- ms_load_sites()
colnames(ms_sites)
site_data <- ms_load_sites()

# load variables
?ms_vars_ts
?ms_load_variables
ms_vars <- ms_load_variables(var_set = 'timeseries')
head(ms_vars)

?ms_load_product
# choose a directory to save data to
my_ms_dir <- here('data')

ms_download_core_data(
  macrosheds_root = my_ms_dir,
  domains = 'all',
  quiet = TRUE
)
macrosheds::ms_download_ws_attr(
  macrosheds_root = my_ms_dir,
  dataset = 'all'
)

# retrieve chemistry data and filter out interp
doc_chem <- ms_load_product(
  my_ms_dir,
  prodname = 'stream_chemistry',
  filter_vars = 'DOC'
)
filtered_doc <- doc_chem %>%
  filter(ms_interp == 0)

# retrieve discharge data and filter out interp
discharge <- ms_load_product(
  my_ms_dir,
  prodname = 'discharge'
)
filtered_discharge <- discharge %>%
  filter(ms_interp == 0)


#################### Merge DOC and Q ##########################

# Ensure the date columns are in Date format
filtered_doc$date <- as.Date(filtered_doc$date)
filtered_discharge$date <- as.Date(filtered_discharge$date)

# Filter filtered_discharge to keep only dates and site_codes present in filtered_doc
filtered_discharge <- filtered_discharge %>%
  filter(date %in% filtered_doc$date & site_code %in% filtered_doc$site_code)

# Filter filtered_doc to keep only dates and site_codes present in filtered_discharge
filtered_doc <- filtered_doc %>%
  filter(date %in% filtered_discharge$date & site_code %in% filtered_discharge$site_code)

# Combine both dataframes based on the date and site_code columns
combined_df <- inner_join(filtered_doc, filtered_discharge, by = c("date", "site_code"))

###################### Add domain #########################

# add domain to combined_df
combined_with_domain <- combined_df %>%
  left_join(site_data, by = "site_code")

#filter for domains in continental US
conus_doc_chem <- combined_with_domain %>%
  filter(latitude >= 24.396308 & latitude <= 49.384358 &
           longitude >= -125.0 & longitude <= -66.93457)
unique(conus_doc_chem$domain)

##################### CQ PLOTS ######################################

# Add a season column to conus_doc_chem based on the date
conus_doc_chem <- conus_doc_chem %>%
  mutate(
    season = case_when(
      month(date) %in% c(9, 10, 11) ~ "Fall",
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer"
    ),
    season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")),  # Set facet order
    year = year(date),  # Extract year for the color gradient
    month = month(date, label = TRUE, abbr = TRUE)  # Extract abbreviated month names
  )

# Plot CQ scatterplot with year as a color gradient, faceted by season
cq_plot <- ggplot(conus_doc_chem, aes(x = val.y, y = val.x, color = year)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q) with no scientific notation
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C) with no scientific notation
  #scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue to red by year
  scale_color_viridis_c(option = "viridis")+
  facet_wrap(~ season) +
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black"),
        strip.text = element_text(color = "black") )
cq_plot

ggsave(here("figures/Q/cq_plot_season.png"), plot = cq_plot, width = 10, height = 7, dpi = 300)

# Plot CQ scatterplot with year as a color gradient, faceted by season
cq_plot_monthly <- ggplot(conus_doc_chem, aes(x = val.y, y = val.x, color = year)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q) with no scientific notation
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C) with no scientific notation
  #scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue to red by year
  scale_color_viridis_c(option = "viridis")+
  facet_wrap(~ month) +
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black"),
        strip.text = element_text(color = "black") )
cq_plot_monthly

ggsave(here("figures/Q/cq_plot_monthly.png"), plot = cq_plot_monthly, width = 13, height = 7, dpi = 300)




# Find the top 20 rows where val.y is the greatest
top_20_discharge <- conus_doc_chem %>%
  arrange(desc(val.y)) %>%  # Sort by val.y in descending order
  slice_head(n = 20)  # Select the top 20 rows

# View the result
head(top_20_discharge)


########### ECOREGIONS ############
# Load the shapefile from EPA
ecoregions <- st_read(here("data/v1/ecoregions/NA_CEC_Eco_Level1.shp"))

conus_doc_chem_sf <- conus_doc_chem %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # Set CRS to WGS84 (EPSG: 4326)

# Step 1: Check CRS of ecoregions and conus_doc_chem_sf
st_crs(ecoregions)  # Check ecoregions CRS
st_crs(conus_doc_chem_sf)  # Check conus_doc_chem_sf CRS
# Ensure ecoregions is in the same CRS as conus_doc_chem_sf
if (st_crs(conus_doc_chem_sf) != st_crs(ecoregions)) {
  conus_doc_chem_sf <- st_transform(conus_doc_chem_sf, st_crs(ecoregions))
}

# Perform the spatial join
conus_doc_chem_with_ecoregion <- st_join(conus_doc_chem_sf, ecoregions)
# Step 4: Drop geometry if you want to return to a non-spatial dataframe
conus_doc_chem_with_ecoregion <- conus_doc_chem_with_ecoregion %>%
  st_drop_geometry()
# Step 4: Create a unique site-ecoregion dataframe
site_ecoregion <- conus_doc_chem_with_ecoregion %>%
  select(site_code, NA_L1NAME) %>%
  distinct()

# Step 5: Join with conus_doc_chem using the unique site-ecoregion pairs
conus_doc_chem_with_ecoregion <- conus_doc_chem %>%
  left_join(site_ecoregion, by = "site_code")

#########

# Filter out the "Temperate Sierras" ecoregion
conus_doc_chem_with_ecoregion_filtered <- conus_doc_chem_with_ecoregion %>%
  filter(NA_L1NAME != "TEMPERATE SIERRAS")  # Exclude the "Temperate Sierras" ecoregion
unique(conus_doc_chem_with_ecoregion_filtered$NA_L1NAME)

# Create a faceted CQ plot with year as a color gradient, faceted by NA_L1NAME (ecoregion)
cq_plot_ecoregion_filtered <- ggplot(conus_doc_chem_with_ecoregion_filtered, aes(x = val.y, y = val.x, color = year)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q)
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C)
  scale_color_viridis_c(option = "viridis") +  # Gradient for year color
  facet_wrap(~ NA_L1NAME) +  # Faceting by ecoregion (excluding "Temperate Sierras")
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  )

# Print the plot
cq_plot_ecoregion_filtered


# Create CQ plots faceted by both ecoregion and month
cq_plot_ecoregion_month <- ggplot(conus_doc_chem_with_ecoregion_filtered, aes(x = val.y, y = val.x, color = year)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q)
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C)
  scale_color_viridis_c(option = "viridis") +  # Gradient for year color
  facet_grid(NA_L1NAME ~ month) +  # Faceting by ecoregion (NA_L1NAME) and month
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  )

# Print the plot
cq_plot_ecoregion_month
ggsave(here("figures/Q/cq_plot_ecoregion_month.png"), plot = cq_plot_ecoregion_month, width = 20, height = 10, dpi = 300)

# Create CQ plots faceted by both ecoregion and season
cq_plot_ecoregion_season <- ggplot(conus_doc_chem_with_ecoregion_filtered, aes(x = val.y, y = val.x, color = year)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q)
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C)
  scale_color_viridis_c(option = "viridis") +  # Gradient for year color
  facet_grid(NA_L1NAME ~ season) +  # Faceting by ecoregion (NA_L1NAME) and month
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  )

# Print the plot
cq_plot_ecoregion_season
ggsave(here("figures/Q/cq_plot_ecoregion_season.png"), plot = cq_plot_ecoregion_season, width = 15, height = 10, dpi = 300)

###### CQ plots for niwot, konza, hbef

# Define the domains of interest
specific_domains <- c("niwot", "konza", "hbef") 

# Filter the data for the specific domains
filtered_domains_data <- conus_doc_chem_with_ecoregion_filtered %>%
  filter(domain %in% specific_domains)

# Create CQ plots faceted by both domain and month
cq_plot_domains_month <- ggplot(filtered_domains_data, aes(x = val.y, y = val.x, color = year)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_x_log10(labels = label_number()) +  # Log scale for discharge (Q)
  scale_y_log10(labels = label_number()) +  # Log scale for DOC concentration (C)
  scale_color_viridis_c(option = "viridis") +  # Gradient for year color
  facet_grid(domain ~ month) +  # Faceting by domain and month
  labs(
    x = "Log Discharge (L/s)",
    y = "Log DOC Concentration (mg/L)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels for better readability
  )

# Display the plot
print(cq_plot_domains_month)
ggsave(here("figures/Q/cq_plot_domains_month.png"), plot = cq_plot_domains_month, width = 12, height = 5, dpi = 300)






#######################################################
######################## RBI ##########################

# Function to calculate the Richards-Baker Flashiness Index (RBI)
rbi_print <- function(x) {
  d <- diff(x)  # Calculate day-to-day differences
  RBI <- sum(abs(d)) / sum(x[2:length(x)])  # Calculate RBI
  return(RBI)
}

# Ensure that 'val.y' is discharge data, and add a 'water_year' column
conus_doc_chem <- conus_doc_chem %>%
  mutate(
    water_year = if_else(month(date) >= 10, year(date) + 1, year(date))
  )

rbi_water_year <- conus_doc_chem %>%
  group_by(site_code, water_year) %>%
  summarize(RBI = rbi_print(val.y), .groups = "drop")

# Calculate VWM DOC for each site and water year
# Assuming 'val.x' is DOC concentration and 'val.y' is discharge (Q)
vwm_doc <- conus_doc_chem %>%
  group_by(site_code, water_year) %>%
  summarize(
    VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),
    Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average Q for the x-axis
    .groups = "drop"
  )

# Merge RBI and VWM DOC data
plot_data <- rbi_water_year %>%
  left_join(vwm_doc, by = c("site_code", "water_year"))

# Create the plot
rbi_plot <- ggplot(plot_data, aes(x = Avg_Q, y = RBI, color = VWM_DOC)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_x_log10(labels = label_number()) +  
  scale_y_log10(labels = label_number()) + 
  scale_color_viridis_c(option = "viridis", direction = -1) +  # Color gradient for VWM DOC
  labs(
    x = "Log Average Discharge (L/s)",
    y = "Log Average RBI",
    color = "VWM DOC (mg/L)"
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
rbi_plot

ggsave(here("figures/Q/rbi_plot.png"), plot = rbi_plot, width = 9, height = 7, dpi = 300)




##########################################################################################
####### scatterplot of water year vs DOC VWM with trendlines for sign. relationships ######

##### thinking through minimum data requirements first

# Step 1: Count the number of observations per site, year, and month
data_with_counts <- conus_doc_chem %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(site_code, year, month) %>%
  summarize(obs_per_month = n(), .groups = "drop")

# Step 2: Filter for each criterion

conus_doc_chem <- conus_doc_chem %>%
  mutate(year = year(date))  # Extract year from the date column

# Criterion 1: Minimum 2 observations per month for at least 10 months in a year
filtered_2_obs <- data_with_counts %>%
  filter(obs_per_month >= 2) %>%
  group_by(site_code, year) %>%
  summarize(months_with_2_obs = n_distinct(month), .groups = "drop") %>%
  filter(months_with_2_obs >= 10)



# Join back to the main data to get only those years that meet the criteria
filtered_data_2_obs <- conus_doc_chem %>%
  semi_join(filtered_2_obs, by = c("site_code", "year"))

# Criterion 2: Minimum 1 observation per month for at least 10 months in a year
filtered_1_obs <- data_with_counts %>%
  filter(obs_per_month >= 1) %>%
  group_by(site_code, year) %>%
  summarize(months_with_1_obs = n_distinct(month), .groups = "drop") %>%
  filter(months_with_1_obs >= 10)

# Join back to the main data to get only those years that meet the criteria
filtered_data_1_obs <- conus_doc_chem %>%
  semi_join(filtered_1_obs, by = c("site_code", "year"))

# Step 3: Compare data retention for each criterion
data_retention <- data.frame(
  Original_Data = nrow(conus_doc_chem),
  Filtered_2_Obs = nrow(filtered_data_2_obs),
  Filtered_1_Obs = nrow(filtered_data_1_obs)
)
site_retention <- data.frame(
  Original_Sites = n_distinct(conus_doc_chem$site_code),
  Filtered_2_Obs_Sites = n_distinct(filtered_data_2_obs$site_code),
  Filtered_1_Obs_Sites = n_distinct(filtered_data_1_obs$site_code)
)

print(data_retention)
# Original_Data   Filtered_2_Obs   Filtered_1_Obs
#    37743            20853           26741
print(site_retention)
#     89                32              60

############### going to move forward with Filtered_1_obs 


library(ggplot2)
library(dplyr)


filtered_data_1_obs <- filtered_data_1_obs %>%
  mutate(
    water_year = if_else(month(date) >= 10, year(date) + 1, year(date))  # Add 1 to the year for months Oct-Dec
  )

# Step 1: Calculate VWM DOC for each site and water year using filtered_data_1_obs
vwm_doc <- filtered_data_1_obs %>%
  group_by(site_code, water_year) %>%
  summarize(
    VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
    Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge for each site and water year
    .groups = "drop"
  )

# Step 2: Determine significant trends for each site and water year
significant_trends <- vwm_doc %>%
  group_by(site_code) %>%
  summarize(
    p_value = tryCatch(
      summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4], 
      error = function(e) NA  # If error, set p_value to NA
    )
  ) %>%
  mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance level of 0.05

# Merge trend significance back into the VWM DOC data
vwm_doc <- vwm_doc %>%
  left_join(significant_trends, by = "site_code")

# Step 3: Join the original dataset to include site_fullname and domain
vwm_doc <- vwm_doc %>%
  left_join(filtered_data_1_obs %>%
              select(site_code, site_fullname, domain) %>%
              distinct(), by = "site_code")

# Step 4: Identify the last points for each significant trendline (only significant ones)
last_points <- vwm_doc %>%
  filter(significant == TRUE) %>%
  group_by(site_code) %>%
  slice_tail(n = 1) %>%  # Get the last point of each trendline
  ungroup() %>%
  select(site_code, site_fullname, domain, water_year, VWM_DOC)  # Select relevant columns

# Step 5: Check that last_points contains the correct number of sites
print(nrow(last_points))  # Should be 12 significant sites

# Step 6: Generate the list of significant sites with their slopes and p-values, and include domain and site_fullname
significant_sites_list <- significant_trends %>%
  filter(significant == TRUE) %>%
  select(site_code, p_value) %>%
  left_join(
    vwm_doc %>%
      group_by(site_code) %>%
      summarize(
        slope = summary(lm(VWM_DOC ~ water_year))$coefficients[2, 1],  # Get the slope
        site_fullname = first(site_fullname),  # Get the site_fullname (first in the group)
        domain = first(domain)  # Get the domain (first in the group)
      ),
    by = "site_code"  # Join by site_code
  )  # Merge the slope, site_fullname, and domain into the list of significant sites

# Display the significant sites and their slopes, p-values, domain, and site_fullname
print(significant_sites_list)

# Step 7: Create the scatterplot with trendlines (no labels on the trendlines) and labels for significant trendlines at the end
sig_trend_plot <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
  geom_point(aes(color = significant), alpha = 0.6, size = 2) +
  scale_y_log10(labels = scales::label_number()) + 
  geom_smooth(
    data = subset(vwm_doc, significant == TRUE), 
    method = "lm", se = FALSE, color = "blue", linewidth = 0.75
  ) +  # Trendline only for significant trends
  scale_color_manual(values = c("grey", "black")) +  # Grey for non-significant, black for significant
  labs(
    x = "Water Year",
    y = "Log VWM DOC (mg/L)",
    color = "Trend Significance"
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
  # Add a label in the bottom-left corner with the number of significant trends
  annotate("text", x = min(vwm_doc$water_year), y = 0.1, 
           label = paste("60 sites, 12 significant"), 
           hjust = 0, vjust = 0, size = 4, color = "blue")

# Display the plot without the labels on the trendlines
print(sig_trend_plot)


ggsave(here("figures/Q/sig_trend_plot.png"), plot = sig_trend_plot, width = 9, height = 7, dpi = 300)


#################### same thing but parse out experimental vs non

# Step 1: Calculate VWM DOC for each site and water year using filtered_data_1_obs
vwm_doc <- filtered_data_1_obs %>%
  group_by(site_code, site_fullname, water_year) %>%
  summarize(
    VWM_DOC = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),  # Calculate VWM DOC
    Avg_Q = mean(val.y, na.rm = TRUE),  # Calculate average discharge for each site and water year
    .groups = "drop"
  )

# Step 2: Determine significant trends for each site and water year
significant_trends <- vwm_doc %>%
  group_by(site_code) %>%
  summarize(
    p_value = tryCatch(
      summary(lm(VWM_DOC ~ water_year))$coefficients[2, 4], 
      error = function(e) NA  # If error, set p_value to NA
    )
  ) %>%
  mutate(significant = !is.na(p_value) & p_value < 0.05)  # Significance level of 0.05

# Merge trend significance back into the VWM DOC data
vwm_doc <- vwm_doc %>%
  left_join(significant_trends, by = "site_code")

# Step 3: Join the original dataset to include site_fullname, domain, and ws_status (experimental vs non-experimental)
vwm_doc <- vwm_doc %>%
  left_join(filtered_data_1_obs %>%
              select(site_code, site_fullname, domain, ws_status) %>%
              distinct(), by = "site_code")

# Step 4: Create the scatterplot with trendlines, differentiating by ws_status and significance
sig_trend_plot2 <- ggplot(vwm_doc, aes(x = water_year, y = VWM_DOC, group = site_code)) +
  # Points: grey for non-significant sites, black for significant sites
  geom_point(aes(color = ifelse(significant, "black", "grey")), alpha = 0.6, size = 2) +
  scale_y_log10(labels = scales::label_number()) + 
  # Trendlines: green for experimental sites with significant trends, blue for non-experimental sites
  geom_smooth(
    data = subset(vwm_doc, significant == TRUE), 
    method = "lm", se = FALSE, 
    aes(color = ifelse(ws_status == "experimental", "darkolivegreen3", "blue")), 
    linewidth = 1.0
  ) +  # Trendline only for significant trends
  scale_color_manual(values = c("black", "blue", "darkolivegreen3", "grey")) +  # Assign color for each condition
  labs(
    x = "Water Year",
    y = "Log VWM DOC (mg/L)",
    color = "Trend Significance"
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
  # Facet by experimental vs non-experimental status
  facet_wrap(~ ws_status, scales = "free_y") 
  # Add a label in the bottom-left corner with the number of significant trends
  #annotate("text", x = min(vwm_doc$water_year), y = 0.1, 
  #         label = paste("12 significant trends"), 
   #        hjust = 0, vjust = 0, size = 4, color = "blue")

# Display the plot
print(sig_trend_plot2)

ggsave(here("figures/Q/sig_trend_plot_experimental.png"), plot = sig_trend_plot2, width = 9, height = 6, dpi = 300)









