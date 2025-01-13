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

head(doc_chem)

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


panola <- conus_doc_chem %>%
  filter(domain == "panola")

hbef <- conus_doc_chem %>%
  filter(domain == "hbef")

min(hbef$date)


#########################################################################
# plot DOC values for each domain for fall, winter, spring, and summer

################################ NOT VWM THOUGH

# fall = months 9,10,11
# winter = months 12,1,2
# spring = months 3, 4, 5
# summer = months 6, 7, 8

# Ensure the date column is in Date format and create a 'season' column
conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.Date(date),
         month = month(date),
         season = case_when(
           month %in% c(9, 10, 11) ~ "Fall",
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% c(3, 4, 5) ~ "Spring",
           month %in% c(6, 7, 8) ~ "Summer"
         ))

# Convert 'season' to a factor with correct order
conus_doc_chem <- conus_doc_chem %>%
  mutate(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")))

# Calculate the mean DOC value by site, season, and domain
mean_doc_by_season <- conus_doc_chem %>%
  group_by(domain, site_code, season) %>%
  summarize(mean_val = mean(val, na.rm = TRUE))

# Calculate the number of sites in each domain
site_counts <- mean_doc_by_season %>%
  group_by(domain) %>%
  summarize(n_sites = n_distinct(site_code))

# Merge the site count information back to the main data
mean_doc_by_season <- mean_doc_by_season %>%
  left_join(site_counts, by = "domain")

# Create one big facet plot for all domains
plot <- ggplot(mean_doc_by_season, aes(x = season, y = mean_val, fill = season)) +
  geom_boxplot() +
  labs(title = "Mean DOC Values by Season Across Domains", 
       x = "Season", y = "Mean DOC (val)") +
  facet_wrap(~ domain, scales = "free_y", ncol = 4) +  # Facet by domain
  geom_text(data = site_counts, aes(x = 1, y = Inf, label = paste("n =", n_sites)),
            hjust = -0.2, vjust = 1.2, size = 3, inherit.aes = FALSE) +  # Add "n=__" label
  scale_fill_manual(values = c("Spring" = "springgreen3", "Summer" = "gold", 
                               "Fall" = "tan3", "Winter" = "lightblue")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))

plot

# Save the facet plot using the here package
ggsave(here("figures/seasonal/boxplot_domain.png"), plot = plot, width = 8, height = 6, dpi = 300)


####################
####### MAPS ########
####################

# grouped domains

# Assign seasons based on the month names in the 'month' column
conus_doc_chem <- conus_doc_chem %>%
  mutate(season = case_when(
    month %in% c("Sep", "Oct", "Nov") ~ "Fall",
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",
    month %in% c("Mar", "Apr", "May") ~ "Spring",
    month %in% c("Jun", "Jul", "Aug") ~ "Summer"
  ))

# Ensure 'season' is a factor with the correct levels
conus_doc_chem <- conus_doc_chem %>%
  mutate(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")))

# Assign groups to the domains
conus_doc_chem <- conus_doc_chem %>%
  mutate(group = case_when(
    domain %in% c("loch_vale", "niwot", "boulder", "east_river") ~ "Group 1",
    domain %in% c("walker_branch", "panola", "neon", "santee") ~ "Group 2",
    domain %in% c("sleepers", "bear", "hbef", "plum") ~ "Group 4",
    domain == "shale_hills" ~ "Group 5",
    domain == "trout_lake" ~ "Group 6",
    domain == "hjandrews" ~ "Group 7",
    TRUE ~ domain  # Keep other domains as is for now
  ))

# Calculate the mean DOC value by group, site, season
mean_doc_by_group <- conus_doc_chem %>%
  group_by(group, site_code, season) %>%
  summarize(mean_val = mean(average_val, na.rm = TRUE))

# Get lat/long for each group by averaging lat/long of domains in the group
site_lat_long_grouped <- site_lat_long %>%
  mutate(group = case_when(
    domain %in% c("loch_vale", "niwot", "boulder", "east_river") ~ "Group 1",
    domain %in% c("walker_branch", "panola", "neon", "santee") ~ "Group 2",
    domain %in% c("sleepers", "bear", "hbef", "plum") ~ "Group 4",
    domain == "shale_hills" ~ "Group 5",
    domain == "trout_lake" ~ "Group 6",
    domain == "hjandrews" ~ "Group 7",
    TRUE ~ domain
  )) %>%
  group_by(group) %>%
  summarize(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE))

# Create a boxplot for a group and return it as a grob without x-axis labels
create_boxplot_grouped <- function(group_name, data) {
  boxplot <- ggplot(data %>% filter(group == group_name), aes(x = factor(season), y = mean_val, fill = season)) +
    geom_boxplot(width = 0.5) +
    scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold")) +
    theme_minimal() +
    theme(legend.position = "none",  # Remove legend from individual boxplots
          text = element_text(size = 8), 
          plot.title = element_text(size = 10),
          axis.text.x = element_blank(),  # Remove x-axis labels
          axis.ticks.x = element_blank(),
          panel.grid = element_blank()) +  # Remove x-axis ticks
    labs(title = NULL, x = NULL, y = NULL)  # Remove x and y labels
  ggplotGrob(boxplot)
}

# Plot the US base map with a dummy layer for the legend
us_map_plot <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  coord_fixed(1.3) +
  # Add a dummy geom to create the fill legend for the seasons
  geom_point(aes(x = -130, y = 50, fill = factor(c("Fall", "Winter", "Spring", "Summer"))), 
             shape = 21, size = 0, show.legend = TRUE) +  # Dummy layer to create the legend
  scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold"),
                    name = "Season") +  # Add a legend for the season
  theme_minimal()+
  guides(fill = guide_legend(override.aes = list(size = 5)))

# Function to place boxplots for each group on the map
add_boxplot_to_map_grouped <- function(base_map, group_name, data, lat_long_data) {
  # Generate boxplot grob for the group
  boxplot_grob <- create_boxplot_grouped(group_name, data)
  
  # Get the lat/long for the group
  group_lat_long <- lat_long_data %>% filter(group == group_name)
  
  # Place the boxplot grob on the map using annotation_custom()
  base_map +
    annotation_custom(grob = boxplot_grob,
                      xmin = group_lat_long$longitude - 5,  # Adjust positioning
                      xmax = group_lat_long$longitude + 5,  # Control width
                      ymin = group_lat_long$latitude - 5,   # Adjust positioning
                      ymax = group_lat_long$latitude + 5) +  # Control height
    geom_rect(aes(xmin = group_lat_long$longitude - 5, xmax = group_lat_long$longitude + 5,
                  ymin = group_lat_long$latitude - 5, ymax = group_lat_long$latitude + 5), 
              fill = NA, color = "black", size = 0.5)  # Add a border around the boxplot
}

# Loop through each group and add its boxplot to the map
final_map <- us_map_plot
for (group in unique(mean_doc_by_group$group)) {
  final_map <- add_boxplot_to_map_grouped(final_map, group, mean_doc_by_group, site_lat_long_grouped)
}

# Print the final map with boxplots and the season legend
print(final_map)

# Save the facet plot using the here package
ggsave(here("figures/seasonal/map_domains_grouped.png"), plot = final_map, width = 8, height = 4, dpi = 300)






######################################
# each domain individually

# Assign seasons based on the month names in the 'month' column
conus_doc_chem <- conus_doc_chem %>%
  mutate(season = case_when(
    month %in% c("Sep", "Oct", "Nov") ~ "Fall",
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",
    month %in% c("Mar", "Apr", "May") ~ "Spring",
    month %in% c("Jun", "Jul", "Aug") ~ "Summer"
  )) %>%
  mutate(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")))

# Calculate the mean DOC value by domain, site, season
mean_doc_by_domain <- conus_doc_chem %>%
  group_by(domain, site_code, season) %>%
  summarize(mean_val = mean(average_val, na.rm = TRUE))

# Get lat/long for each domain
site_lat_long <- site_data %>%
  group_by(domain) %>%
  summarize(latitude = mean(latitude, na.rm = TRUE), 
            longitude = mean(longitude, na.rm = TRUE))

# Create a boxplot for a domain and return it as a grob with smaller size and no x-axis labels
create_boxplot_domain <- function(domain_name, data) {
  plot_data <- data %>% filter(domain == domain_name)
  if (nrow(plot_data) == 0) return(NULL)  # Return NULL if there is no data for the domain
  
  boxplot <- ggplot(plot_data, aes(x = factor(season), y = mean_val, fill = season)) +
    geom_boxplot(width = 0.8) +  # Make the boxplot smaller
   scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold"),
                      limits = c("Fall", "Winter", "Spring", "Summer")) +  # Correct the legend order
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual boxplots
      text = element_text(size = 7),  # Adjust text size
      plot.title = element_text(size = 9),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),  # Remove gridlines from boxplots
      panel.background = element_rect(fill = "white", color = NA)  # **Set white background for the boxplot**
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
  
  # Get the lat/long for the domain and apply an offset to avoid overlap
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
                      xmax = long + 4,  # Control width
                      ymin = lat - 4,   # Adjust positioning (make it smaller)
                      ymax = lat + 4) +  # Control height
    geom_rect(aes(xmin = long - 4, xmax = long + 4,
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
  theme(panel.grid = element_blank()) +  # **Remove gridlines**
  guides(fill = guide_legend(override.aes = list(size = 5)))

# Apply offsets to avoid overlaps and place the boxplots on the map
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

for (domain in unique(mean_doc_by_domain$domain)) {
  offset <- offsets[[domain]] %||% c(0, 0)  # Default to no offset if not specified
  final_map <- add_boxplot_to_map_domain(final_map, domain, mean_doc_by_domain, site_lat_long, offset[1], offset[2])
}

# Print the final map with boxplots and the season legend
print(final_map)


# Save the plot using the here package
ggsave(here("figures/seasonal/map_individual_domains.png"), plot = final_map, width = 12, height = 7, dpi = 300)


