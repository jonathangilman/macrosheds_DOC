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

#separate neon
neon_doc_chem <- conus_doc_chem %>%
  filter(domain == "neon")
unique(neon_doc_chem$domain)

# remove NEON domain for now
conus_doc_chem <- conus_doc_chem %>%
  filter(domain != "neon")
unique(conus_doc_chem$domain)




#################### Calculate MONTHLY VWM ###########################


conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.character(date))
neon_doc_chem <- neon_doc_chem %>%
  mutate(date = as.character(date))

# Now try converting the Date column to Date format again
conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
neon_doc_chem <- neon_doc_chem %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Create a new column for the month in "YYYY-MM" format
conus_doc_chem <- conus_doc_chem %>%
  mutate(Month = format(date, "%Y-%m"))  # Extract month now that Date is properly formatted
neon_doc_chem <- neon_doc_chem %>%
  mutate(Month = format(date, "%Y-%m"))

# Calculate VWM for each domain and month
vwm_results <- conus_doc_chem %>%
  group_by(domain, Month) %>%
  summarize(VWM = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE)) %>%
  ungroup()

# Calculate VWM for each site and month
neon_vwm_results <- neon_doc_chem %>%
  group_by(site_fullname, Month) %>%
  summarize(VWM = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE)) %>%
  ungroup()


########################### Plot monthly VWM #############################

# Extract month from the Month column (e.g., "2020-01" becomes "01")
average_vwm <- vwm_results %>%
  mutate(Month_Only = format(as.Date(paste0(Month, "-01")), "%m")) %>%  # Create a new column for the month
  group_by(domain, Month_Only) %>%
  summarize(Mean_VWM = mean(VWM, na.rm = TRUE), .groups = 'drop')  # Calculate the mean VWM by month
average_vwm_neon <- neon_vwm_results %>%
  mutate(Month_Only = format(as.Date(paste0(Month, "-01")), "%m")) %>%  # Create a new column for the month
  group_by(site_fullname, Month_Only) %>%
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
       y = "Mean VWM DOC (mg/L)") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend for clarity in faceting
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black"),
        strip.text = element_text(color = "black") ) + 
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
   labs(title = "Average VWM DOC by Water Year and Domain",
       x = "Water Year",
       y = "Mean VWM DOC (mg/L)") +
  theme_minimal() +theme(legend.position = "none",  # Remove legend for clarity in faceting
                         axis.text.x = element_text(angle = 45, hjust = 1),
                         panel.border = element_rect(color = "black", fill = NA, size = 1),
                         axis.ticks = element_line(color = "black"),
                         axis.title = element_text(size = 14),
                         strip.background = element_rect(fill = "grey80", color = "black"),
                         strip.text = element_text(color = "black") ) + theme(legend.position = "none") +  # Remove legend for clarity in faceting
  facet_wrap(~ domain, nrow = 5, scales = "free")  # Unique y-axis for each facet

# Display the plot
print(vwm_water_year_plot)

# Save the average VWM by water year plot if desired
ggsave("figures/vwm/average_vwm_by_water_year_faceted_by_domain.png", plot = vwm_water_year_plot, width = 10, height = 10, dpi = 300)





############################ MAP MONTHlY VWM #############################
#########################################################################
# plot DOC values for each domain for fall, winter, spring, and summer
# fall = months 9,10,11
# winter = months 12,1,2
# spring = months 3, 4, 5
# summer = months 6, 7, 8


# Ensure the date column is in Date format and create a 'season' column
vwm_results <- vwm_results %>%
  mutate(
         season = case_when(
           Month_Num %in% c(9, 10, 11) ~ "Fall",
           Month_Num %in% c(12, 1, 2) ~ "Winter",
           Month_Num %in% c(3, 4, 5) ~ "Spring",
           Month_Num %in% c(6, 7, 8) ~ "Summer"
         ))

# Convert 'season' to a factor with correct order
vwm_results <- vwm_results %>%
  mutate(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")))

# plot seasonal VWM DOC
plot <- ggplot(vwm_results, aes(x = season, y = VWM, fill = season)) +
  geom_boxplot() +
  labs(title = "VWM DOC Values by Season Across Domains", 
       x = "Season", y = "Mean DOC (val)") +
  facet_wrap(~ domain, scales = "free_y", ncol = 5) +  # Facet by domain
  scale_fill_manual(values = c("Spring" = "springgreen3", "Summer" = "gold", 
                               "Fall" = "tan3", "Winter" = "lightblue")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))

plot


# Save the facet plot using the here package
#ggsave(here("figures/seasonal/boxplot_domain.png"), plot = plot, width = 8, height = 6, dpi = 300)


####################
####### MAPS ########
####################

# Get lat/long for each domain from site_data
site_lat_long <- site_data %>%
  group_by(domain) %>%
  summarize(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE))

# Create a boxplot for each domain based on VWM values and return it as a grob with smaller size and no x-axis labels
create_boxplot_domain <- function(domain_name, data) {
  plot_data <- data %>% filter(domain == domain_name)
  if (nrow(plot_data) == 0) return(NULL)  # Return NULL if there is no data for the domain
  
  boxplot <- ggplot(plot_data, aes(x = factor(season), y = VWM, fill = season)) +
    geom_boxplot(width = 0.8) +
    scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold"),
                      limits = c("Fall", "Winter", "Spring", "Summer")) +  # Correct the legend order
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual boxplots
      text = element_text(size = 7),  # Adjust text size
      plot.title = element_text(hjust = 0.5, size = 9),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),  # Remove gridlines from boxplots
      panel.background = element_rect(fill = "white", color = NA),  # Set white background for the boxplot
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = domain_name, x = NULL, y = NULL)  # Remove x and y labels
  ggplotGrob(boxplot)
}

# Function to place boxplots for each domain on the map with a red dot and line connecting to the boxplot
add_boxplot_to_map_domain <- function(base_map, domain_name, data, lat_long_data, x_offset = 0, y_offset = 0) {
  # Generate boxplot grob for the domain
  boxplot_grob <- create_boxplot_domain(domain_name, data)
  
  # Skip if boxplot_grob is NULL (no data for that domain)
  if (is.null(boxplot_grob)) return(base_map)
  
  # Get the lat/long for the domain from site_data with the applied offset to avoid overlap
  domain_lat_long <- lat_long_data %>% filter(domain == domain_name)
  long <- domain_lat_long$longitude + x_offset  # Apply longitude offset to avoid overlap
  lat <- domain_lat_long$latitude + y_offset  # Apply latitude offset to avoid overlap
  
  # Adjust the coordinates so the line goes to the boxplot border instead of the center
  xend <- long - 4  # Stops the line at the left border of the boxplot (adjust as needed)
  yend <- lat       # Stops the line at the bottom border of the boxplot (adjust as needed)
  
  # Place the boxplot grob on the map and add a red dot with a connecting line
  base_map +
    # Add a red dot at the domain's latitude and longitude
    geom_point(aes(x = domain_lat_long$longitude, y = domain_lat_long$latitude), color = "black", fill = "red", shape = 21, size = 3) +
    # Draw a line connecting the red dot to the boxplot
    geom_segment(aes(x = domain_lat_long$longitude, y = domain_lat_long$latitude, 
                     xend = long, yend = lat), color = "black", linetype = "dashed") +
    annotation_custom(grob = boxplot_grob,
                      xmin = long - 4,  # Adjust positioning (make it smaller)
                      xmax = long + 4.5,  # Control width
                      ymin = lat - 4,   # Adjust positioning (make it smaller)
                      ymax = lat + 4) +  # Control height
    geom_rect(aes(xmin = long - 4, xmax = long + 4.5,
                  ymin = lat - 4, ymax = lat + 4), 
              fill = NA, color = "black", size = 0.5)  # Add a border around the boxplot
}

# Plot the US base map with adjusted blank space
us_map_plot <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  coord_cartesian(xlim = c(-135, -50), ylim = c(18, 58)) +  # Adjusted limits to reduce blank space
  # Add a dummy geom to create the fill legend for the seasons
  geom_point(aes(x = -130, y = 50, fill = factor(c("Fall", "Winter", "Spring", "Summer"))), 
             shape = 21, size = 0, show.legend = TRUE) +  # Dummy layer to create the legend
  scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold"),
                    name = "Season") +  # Add a legend for the season
  theme_minimal() +
  theme(panel.grid = element_blank()) +  # Remove gridlines
  guides(fill = guide_legend(override.aes = list(size = 5)))

# Apply offsets to avoid overlaps and place the boxplots on the map
final_map <- us_map_plot
offsets <- list(
  "loch_vale" = c(-8, 12),  # Example offset for loch_vale, add more as needed
  "niwot" = c(-25, 0),
  "boulder" = c(-11, -13),
  "east_river" = c(-20, -7),
  "bear" = c(-3, 7),  
  "hbef" = c(13, 0),
  "hjandrews" = c(-10, 5),
  "konza" = c(-3, -15),
  "neon" = c(-5, -13),  
  "walker_branch" = c(17, -10),
  "panola" = c(-3, -10),
  "plum" = c(13, -7),
  "santee" = c(3, -10),  
  "shale_hills" = c(10, -6),
  "sleepers" = c(-10, 5),
  "trout_lake" = c(-8, 7)
  # Add offsets for all other domains similarly...
)

for (domain in unique(vwm_results$domain)) {
  offset <- offsets[[domain]] %||% c(0, 0)  # Default to no offset if not specified
  final_map <- add_boxplot_to_map_domain(final_map, domain, vwm_results, site_lat_long, offset[1], offset[2])
}

# Print the final map with boxplots and the season legend
print(final_map)
# Save the plot using the here package
ggsave(here("figures/seasonal/map_individual_domains_VWM.png"), plot = final_map, width = 12, height = 7, dpi = 300)


##############################  NEON MAP #####################################

# Ensure the date column is in Date format and create a 'season' column
average_vwm_neon <- average_vwm_neon %>%
  mutate(
    Month_Only = as.numeric(Month_Only),
    season = case_when(
      Month_Only %in% c(09, 10, 11) ~ "Fall",
      Month_Only %in% c(12, 01, 02) ~ "Winter",
      Month_Only %in% c(03, 04, 05) ~ "Spring",
      Month_Only %in% c(06, 07, 08) ~ "Summer"
    ))

# Convert 'season' to a factor with correct order
average_vwm_neon <- average_vwm_neon %>%
  mutate(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")))


# Summarize lat/long for each site in the neon domain from neon_doc_chem
site_lat_long <- neon_doc_chem %>%
  group_by(site_fullname) %>%
  summarize(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE))

us_map_plot <- ggplot() +
  # Base map with state boundaries
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  
  # Add red dots for each NEON site
  geom_point(data = site_lat_long, aes(x = longitude, y = latitude), color = "black", fill = "red", shape = 21, size = 3) +
  
  # Add labels with repelling to avoid overlap and connecting lines
  geom_text_repel(data = site_lat_long, aes(x = longitude, y = latitude, label = site_fullname),
                  color = "black", size = 3, 
                  box.padding = 0.3, point.padding = 0.3, # Adjust padding to control distance
                  segment.color = "black", segment.size = 0.5) +  # Customize line appearance
  
  # Set map limits to focus on the US region
  coord_cartesian(xlim = c(-125, -67), ylim = c(25, 50)) +
  
  # Add title and adjust theme
  labs(title = "Map of NEON Sites", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove gridlines for a cleaner look

# Display the map
print(us_map_plot)
ggsave(here("figures/seasonal/map_neon_sites.png"), plot = us_map_plot, width = 12, height = 7, dpi = 300)



library(ggplot2)
library(dplyr)

# Define the custom order of the sites manually
site_order <- c("Martha Creek", "McRae Creek", "Upper Big Creek", "Teakettle 2 Creek",
                "Red Butte Creek", "Sycamore Creek", "Blacktail Deer Creek", "West St Louis Creek", "Como Creek", "Arikaree River",
                "Pringle Creek", "Blue River", "Kings Creek", "McDiffett Creek",
                "Lower Tombigbee River at Choctaw Refuge", "Black Warrior River near Dead Lake",
                "Mayfield Creek", "Walker Branch", "LeConte Creek", "Flint River", "Posey Creek", "Lewis Run",
                "Lower Hop Brook")  # Replace with your desired site order

# Reorder 'site_fullname' factor based on the custom order
average_vwm_neon$site_fullname <- factor(average_vwm_neon$site_fullname, levels = site_order)

# Create the faceted boxplot with manually ordered facets
facet_boxplot <- ggplot(average_vwm_neon, aes(x = season, y = Mean_VWM, fill = season)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold")) +
  labs(title = "Seasonal VWM DOC Across NEON Sites", 
       x = "Season", y = "VWM DOC (mg/L) ") +
  facet_wrap(~ site_fullname, scales = "free_y") +  # Facet by site with free y-axis scaling
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Optional: Remove major grid lines for a cleaner look
        panel.grid.minor = element_blank(),  # Optional: Remove minor grid lines for a cleaner look
        legend.position = "none")  # Optional: Remove legend if colors are self-explanatory

# Display the plot
print(facet_boxplot)
ggsave(here("figures/seasonal/facet_boxplot_neon.png"), plot = facet_boxplot, width = 9, height = 7, dpi = 300)


