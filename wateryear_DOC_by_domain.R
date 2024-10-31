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

#########################################################################

# Create a vectorized function to calculate the water year
water_year <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  
  # Use if_else to handle vectorized conditions
  dplyr::if_else(month >= 10, year, year - 1)
}

# Calculate the water year and yearly mean DOC value by domain (across all sites)
mean_doc_by_domain_year <- conus_doc_chem %>%
  mutate(date = as.Date(date),  # Ensure the 'date' column is in Date format
         water_year = water_year(date)) %>%  # Apply the vectorized water year function
  group_by(domain, water_year) %>%
  summarize(
    mean_val = mean(val, na.rm = TRUE),  # Average DOC across all sites
    n_obs = n()  # Number of observations for each domain and water year
  )

create_yearly_mean_plot <- function(domain_name, data) {
  plot_data <- data %>% filter(domain == domain_name)
  if (nrow(plot_data) == 0) return(NULL)  # Return NULL if there is no data for the domain
  
  # Calculate the total number of observations across all years for the given domain
  total_obs <- sum(plot_data$n_obs)
  
  # Format the total_obs with commas
  total_obs_formatted <- scales::comma(total_obs)
  
  # Create a line plot to show the yearly means across water years
  yearly_plot <- ggplot(plot_data, aes(x = water_year, y = mean_val)) +
    geom_line(size = 1, color = "blue") +  # Single line for each domain
    geom_point(size = 0.5) +  # Add points to highlight yearly means+
    # Add a red trendline using geom_smooth with a linear model
    geom_smooth(method = "lm", color = "red", linetype = "solid", se = FALSE) +
    # Use annotate to add a single "n=__" label in the top right corner with formatted number
    #annotate("text", label = paste("n =", total_obs_formatted), x = Inf, y = Inf, 
    #         hjust = 1.1, vjust = 1.1, size = 3, color = "black") +  
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      text = element_text(size = 7),  # Adjust text size
      plot.title = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
      panel.grid = element_blank(),  # Remove gridlines
      panel.background = element_rect(fill = "white", color = NA)  # Set white background for the plot
    ) +
    labs(title = domain_name, x = "Water Year", y = "Mean DOC")
  
  ggplotGrob(yearly_plot)
}

# Function to place yearly mean plots for each domain on the map with a red dot and line connecting to the plot
add_plot_to_map_domain <- function(base_map, domain_name, data, lat_long_data, x_offset = 0, y_offset = 0) {
  # Generate plot grob for the domain
  yearly_mean_grob <- create_yearly_mean_plot(domain_name, data)
  
  # Skip if plot grob is NULL (no data for that domain)
  if (is.null(yearly_mean_grob)) return(base_map)
  
  # Get the lat/long for the domain and apply an offset to avoid overlap
  domain_lat_long <- lat_long_data %>% filter(domain == domain_name)
  long <- domain_lat_long$longitude + x_offset  # Apply longitude offset to avoid overlap
  lat <- domain_lat_long$latitude + y_offset  # Apply latitude offset to avoid overlap
  
  # Adjust the coordinates so the line goes to the plot border instead of the center
  xend <- long - 4  # Stops the line at the left border of the plot (adjust as needed)
  yend <- lat       # Stops the line at the bottom border of the plot (adjust as needed)
  
  # Place the plot grob on the map and add a red dot with a connecting line
  base_map +
    # Add a red dot at the domain's latitude and longitude
    geom_point(aes(x = domain_lat_long$longitude, y = domain_lat_long$latitude), color = "black", fill = "red", shape = 21, size = 3) +
    # Draw a line connecting the red dot to the plot
    geom_segment(aes(x = domain_lat_long$longitude, y = domain_lat_long$latitude, 
                     xend = long, yend = lat), color = "black", linetype = "dashed") +
    annotation_custom(grob = yearly_mean_grob,
                      xmin = long - 4,  # Adjust positioning
                      xmax = long + 4,  # Control width
                      ymin = lat - 4,   # Adjust positioning
                      ymax = lat + 4) +  # Control height
    geom_rect(aes(xmin = long - 4, xmax = long + 4,
                  ymin = lat - 4, ymax = lat + 4), 
              fill = NA, color = "black", size = 0.5)  # Add a border around the plot
}

# Plot the US base map with adjusted blank space
us_map_plot <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  coord_cartesian(xlim = c(-140, -50), ylim = c(18, 58)) +  # Adjusted limits to reduce blank space
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove gridlines

# Apply offsets to avoid overlaps and place the yearly mean plots on the map
final_map <- us_map_plot
offsets <- list(
  "loch_vale" = c(-8, 12),  # Example offset for loch_vale, add more as needed
  "niwot" = c(-25, 0),
  "boulder" = c(-13, -13),
  "east_river" = c(-20, -7),
  "bear" = c(-3, 7),  # Example offset for loch_vale, add more as needed
  "hbef" = c(13, 0),
  "hjandrews" = c(-10, 5),
  "konza" = c(1, -15),
  "neon" = c(-5, -13),  # Example offset for loch_vale, add more as needed
  "walker_branch" = c(17, -10),
  "panola" = c(-3, -10),
  "plum" = c(13, -7),
  "santee" = c(3, -10),  # Example offset for loch_vale, add more as needed
  "shale_hills" = c(10, -6),
  "sleepers" = c(-10, 5),
  "trout_lake" = c(-8, 7)
  # Add offsets for all other domains similarly...
)

for (domain in unique(mean_doc_by_domain_year$domain)) {
  offset <- offsets[[domain]] %||% c(0, 0)  # Default to no offset if not specified
  final_map <- add_plot_to_map_domain(final_map, domain, mean_doc_by_domain_year, site_lat_long, offset[1], offset[2])
}

# Print the final map with yearly mean plots
print(final_map)


# Save the plot using the here package
ggsave(here("figures/seasonal/map_water_year.png"), plot = final_map, width = 10, height = 6, dpi = 300)


############################################################
# facet plot of same graphs just not on map
# order facets by number of DOC measurements

# Recalculate the number of unique DOC observations for each domain and water year
mean_doc_by_domain_year <- conus_doc_chem %>%
  mutate(date = as.Date(date),  # Ensure the 'date' column is in Date format
         water_year = water_year(date)) %>%  # Calculate water year
  group_by(domain, water_year) %>%
  summarize(
    mean_val = mean(val, na.rm = TRUE),  # Calculate mean DOC value for each domain and water year
    n_obs = n()  # Recalculate the number of observations (records) for each domain and water year
  )

# Check if the total number of observations per domain is now correct
domain_summary <- mean_doc_by_domain_year %>%
  group_by(domain) %>%
  summarize(total_obs = sum(n_obs))

print(domain_summary)  # Verify that HBEF has around 10,000 observations


create_faceted_yearly_mean_plot <- function(data) {
  
  # Group by domain to calculate the total number of observations
  domain_summary <- data %>%
    group_by(domain) %>%
    summarize(
      total_obs = sum(n_obs)  # Sum the number of observations for each domain
    )
  
  # Join the summary data back to the original dataset
  data_with_slope <- data %>%
    left_join(domain_summary, by = "domain") %>%
    group_by(domain) %>%
    mutate(
      slope = coef(lm(mean_val ~ water_year))[2]  # Extract the slope from the linear model
    )
  
  # Create the base plot with all domains
  yearly_plot <- ggplot(data_with_slope, aes(x = water_year, y = mean_val)) +
    geom_line(size = 0.75, color = "black") +  # Single line for each domain
    geom_point(size = 0.5) +  # Add points to highlight yearly means
    # Add a red trendline using geom_smooth with a linear model
    geom_smooth(method = "lm", color = "red", linetype = "solid", se = FALSE, size = 0.75) +
    # Use facet_wrap to create a separate panel for each domain, ordered by total_obs
    facet_wrap(~ domain, scales = "free_y") +  # Separate by domain, free_y allows different y-axis scales
    # Annotate the total number of observations (n=__) in black for each domain
    geom_text(aes(label = paste0("n = ", scales::comma(total_obs))), 
              x = 1975, y = Inf, hjust = 0, vjust = 2, color = "black", size = 1.75) +
    # Annotate the slope in red for each domain
    geom_text(aes(label = paste0("Slope: ", round(slope, 3))), 
              x = 1975, y = Inf, hjust = 0, vjust = 3.5, color = "red", size = 1.75) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      text = element_text(size = 9),  # Adjust text size
      plot.title = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
      panel.grid = element_blank(),  # Remove gridlines 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)  # Add a black border around each facet
    ) +
    labs(x = "Water Year", y = "Mean DOC", title = "DOC by Water Year")
  
  # Return the plot
  yearly_plot
}
# Create the faceted plot for all domains
faceted_plot <- create_faceted_yearly_mean_plot(mean_doc_by_domain_year)

# Save the plot
ggsave(here("figures/seasonal/water_year.png"), plot = faceted_plot, width = 8, height = 6, dpi = 300)
