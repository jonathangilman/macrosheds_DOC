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

# Check the resulting combined dataframe
head(combined_df)  # Display the first few rows

unique(combined_df$site_code)

###################### Add domain #########################

# add domain to combined_df
combined_with_domain <- combined_df %>%
  left_join(site_data, by = "site_code")

#filter for domains in continental US
conus_doc_chem <- combined_with_domain %>%
  filter(latitude >= 24.396308 & latitude <= 49.384358 &
           longitude >= -125.0 & longitude <= -66.93457)
unique(conus_doc_chem$domain)

# remove NEON domain for now
conus_doc_chem <- conus_doc_chem %>%
  filter(domain != "neon")
unique(conus_doc_chem$domain)


#################### Calculate MONTHLY VWM ###########################


conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.character(date))

# Now try converting the Date column to Date format again
conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))  # Adjust format if needed

# Create a new column for the month in "YYYY-MM" format
conus_doc_chem <- conus_doc_chem %>%
  mutate(Month = format(date, "%Y-%m"))  # Extract month now that Date is properly formatted

# Calculate VWM for each domain and month
vwm_results <- conus_doc_chem %>%
  group_by(domain, Month) %>%
  summarize(VWM = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE)) %>%
  ungroup()


########################### Plot monthly VWM #############################

# Extract month from the Month column (e.g., "2020-01" becomes "01")
average_vwm <- vwm_results %>%
  mutate(Month_Only = format(as.Date(paste0(Month, "-01")), "%m")) %>%  # Create a new column for the month
  group_by(domain, Month_Only) %>%
  summarize(Mean_VWM = mean(VWM, na.rm = TRUE), .groups = 'drop')  # Calculate the mean VWM by month

# Map numeric month to month abbreviations
average_vwm$Month_Only <- factor(average_vwm$Month_Only,
                                 levels = c("01", "02", "03", "04", "05", "06", 
                                            "07", "08", "09", "10", "11", "12"),
                                 labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Create a plot for the average VWM by domain
vwm_avg_plot <- ggplot(average_vwm, aes(x = Month_Only, y = Mean_VWM, group = domain)) +
  geom_point(size = 2) +  # Add points for mean VWM
  geom_line(size = 1) +   # Connect points with lines
  labs(title = "Average Monthly VWM by Domain",
       x = "Month",
       y = "Mean VWM (mg/L)") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend for clarity in faceting
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  facet_wrap(~ domain, nrow = 5, scales = "free_y")  # Unique x-axis for each facet

# Display the plot
print(vwm_avg_plot)

# Save the average VWM plot if desired
ggsave("figures/vwm/average_monthly_vwm_faceted_by_domain.png", plot = vwm_avg_plot, width = 10, height = 8, dpi = 300)



########################### Plot water year VWM #############################
library(dplyr)
library(lubridate)

# Ensure the Month column is in the right format
vwm_results$Month <- as.Date(paste0(vwm_results$Month, "-01"))  # Convert to date by adding a day

# Extract the year and month
vwm_results <- vwm_results %>%
  mutate(
    Year = year(Month),
    Month_Num = month(Month)
  )

# Calculate the Water Year
vwm_results <- vwm_results %>%
  mutate(Water_Year = ifelse(Month_Num %in% c(10, 11, 12), Year + 1, Year))

# Calculate the average VWM for each domain and water year
average_vwm_water_year <- vwm_results %>%
  group_by(domain, Water_Year) %>%
  summarize(Mean_VWM = mean(VWM, na.rm = TRUE), .groups = 'drop')  # Calculate the mean VWM

# Create a faceted plot for the average VWM by domain and water year
vwm_water_year_plot <- ggplot(average_vwm_water_year, aes(x = Water_Year, y = Mean_VWM, group = domain)) +
  geom_point(size = 2) +  # Add points for mean VWM
  geom_line(size = 1) +   # Connect points with lines
  #geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.5) +
   labs(title = "Average VWM by Water Year and Domain",
       x = "Water Year",
       y = "Mean VWM (mg/L)") +
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend for clarity in faceting
  facet_wrap(~ domain, nrow = 5, scales = "free")  # Unique y-axis for each facet

# Display the plot
print(vwm_water_year_plot)

# Save the average VWM by water year plot if desired
ggsave("figures/vwm/average_vwm_by_water_year_faceted_by_domain.png", plot = vwm_water_year_plot, width = 10, height = 8, dpi = 300)





############################ MAP MONTHlY VWM #############################

# Assuming you have calculated VWM and stored it in vwm_results
# vwm_results should have columns: domain, Month, VWM

# Get lat/long for each domain
site_lat_long_grouped <- site_data %>%
  group_by(domain) %>%
  summarize(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE))

# Create a boxplot for each domain using the VWM values
create_boxplot_monthly <- function(data) {
  boxplot <- ggplot(data, aes(x = Month, y = VWM, fill = Month)) +
    geom_boxplot(width = 0.5) +
    theme_minimal() +
    theme(legend.position = "none", 
          text = element_text(size = 8), 
          plot.title = element_text(size = 10),
          axis.text.x = element_blank(),  # Optional: remove x-axis labels
          panel.grid = element_blank()) +  # Remove grid lines
    labs(title = NULL, x = NULL, y = NULL)  # Remove x and y labels
  ggplotGrob(boxplot)
}

# Plot the US base map with a dummy layer for the legend
us_map_plot <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  coord_fixed(1.3) +
  # Dummy geom to create the fill legend for the months
  geom_point(aes(x = -130, y = 50, fill = factor(month.abb)), 
             shape = 21, size = 0, show.legend = TRUE) +  # Dummy layer for legend
  scale_fill_manual(values = rainbow(12), name = "Month") +  # Add a legend for the months
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# Function to place boxplots for each domain on the map
add_boxplot_to_map_monthly <- function(base_map, domain_name, data, lat_long_data) {
  # Generate boxplot grob for the domain
  boxplot_grob <- create_boxplot_monthly(data %>% filter(domain == domain_name))
  
  # Get the lat/long for the domain
  domain_lat_long <- lat_long_data %>% filter(domain == domain_name)
  
  # Place the boxplot grob on the map
  base_map +
    annotation_custom(grob = boxplot_grob,
                      xmin = domain_lat_long$longitude - 5,  # Adjust positioning
                      xmax = domain_lat_long$longitude + 5,  # Control width
                      ymin = domain_lat_long$latitude - 5,   # Adjust positioning
                      ymax = domain_lat_long$latitude + 5) +  # Control height
    geom_rect(aes(xmin = domain_lat_long$longitude - 5, xmax = domain_lat_long$longitude + 5,
                  ymin = domain_lat_long$latitude - 5, ymax = domain_lat_long$latitude + 5), 
              fill = NA, color = "black", size = 0.5)  # Add a border around the boxplot
}

# Loop through each domain and add its boxplot to the map
final_map <- us_map_plot
for (domain in unique(vwm_results$domain)) {
  final_map <- add_boxplot_to_map_monthly(final_map, domain, vwm_results, site_lat_long_grouped)
}

# Print the final map with boxplots and the month legend
print(final_map)

# Save the plot using the here package
ggsave(here("figures/monthly/map_domains_monthly_vwm.png"), plot = final_map, width = 8, height = 4, dpi = 300)






