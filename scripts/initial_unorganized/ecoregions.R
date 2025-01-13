#install.packages("ggrepel")
library(macrosheds)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(here)
library(patchwork)
library(ggrepel)

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

##################### Calculate seasonal VWM for each site  ###############

# Step 1: Add a season column based on the month
conus_doc_chem <- conus_doc_chem %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    year = year(date)  # Extract the year for grouping
  )

# Step 2: Calculate seasonal VWM for each year and site
seasonal_vwm_yearly <- conus_doc_chem %>%
  filter(var.x == "DOC", var.y == "discharge") %>%  # Filter for DOC and discharge
  group_by(site_code, year, season) %>%  # Group by site, year, and season
  summarize(
    VWM = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE)  # Calculate seasonal VWM
  ) %>%
  ungroup()

# Step 3: Calculate average VWM for each season across years
average_seasonal_vwm <- seasonal_vwm_yearly %>%
  group_by(site_code, season) %>%
  summarize(
    avg_VWM = mean(VWM, na.rm = TRUE)  # Average VWM across years
  ) %>%
  ungroup()

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

# Step 5: Join with average_seasonal_vwm using the unique site-ecoregion pairs
average_seasonal_vwm_with_ecoregion <- average_seasonal_vwm %>%
  left_join(site_ecoregion, by = "site_code")


########### plot on map ############

# Transform ecoregions_us to EPSG:4326 (longitude-latitude)
ecoregions <- st_transform(ecoregions, crs = 4326)

# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(gridExtra)

# Define ecoregions of interest
pertinent_ecoregions <- c("NORTHWESTERN FORESTED MOUNTAINS", "NORTHERN FORESTS", 
                          "GREAT PLAINS", "TEMPERATE SIERRAS", "EASTERN TEMPERATE FORESTS")

# Filter and simplify the ecoregions dataset
ecoregions_filtered <- ecoregions %>%
  mutate(NA_L1NAME = ifelse(NA_L1NAME %in% pertinent_ecoregions, NA_L1NAME, "Other"))

# Define colors for pertinent ecoregions and grey for others
ecoregion_colors <- c(
  "NORTHWESTERN FORESTED MOUNTAINS" = "#66c2a5",
  "NORTHERN FORESTS" = "#fc8d62",
  "GREAT PLAINS" = "#8da0cb",
  "TEMPERATE SIERRAS" = "#e78ac3",
  "EASTERN TEMPERATE FORESTS" = "#a6d854",
  "Other" = "grey80"
)

# Base map with simplified ecoregion colors
base_map <- ggplot() +
  geom_sf(data = ecoregions_filtered, aes(fill = NA_L1NAME), color = "black") +
  scale_fill_manual(values = ecoregion_colors, name = "Ecoregion") +
  labs(title = "Average Seasonal DOC by Ecoregion", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 8))

# Define coordinates for each ecoregion's boxplot
ecoregion_coords <- data.frame(
  ecoregion = pertinent_ecoregions,
  x = c(-120, -75, -100, -115, -85),  # Approximate longitude values
  y = c(45, 45, 40, 32, 30)          # Approximate latitude values
)

# Function to create boxplots with two-line title and number of sites
create_ecoregion_boxplot <- function(ecoregion_name, data) {
  plot_data <- data %>% filter(NA_L1NAME == ecoregion_name)
  
  # Calculate the number of unique sites for this ecoregion
  num_sites <- plot_data %>% distinct(site_code) %>% nrow()
  
  # Format the title with a line break and the number of sites
  title_text <- paste0(gsub(" ", "\n", ecoregion_name, fixed = TRUE), "\n(n = ", num_sites, " sites)")
  
  # Ensure the 'season' is a factor with the correct order
  plot_data$season <- factor(plot_data$season, levels = c("Fall", "Winter", "Spring", "Summer"))
  
  
  boxplot <- ggplot(plot_data, aes(x = factor(season), y = avg_VWM, fill = season)) +
    geom_boxplot(width = 0.8) +
    scale_fill_manual(values = c("Fall" = "orange", "Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "gold"),
                      limits = c("Fall", "Winter", "Spring", "Summer")) +
    theme_minimal() +
    theme(
      legend.position = "none",   # Remove legend from individual boxplots
      text = element_text(size = 7),
      plot.title = element_text(hjust = 0.5, size = 6),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 4),
      panel.grid = element_blank(),   # Remove gridlines from boxplots
      panel.background = element_rect(fill = "white", color = "black"),  # White background with black border
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = title_text, x = NULL, 
         y = "VWM DOC (mg/L)")  # Ecoregion name and number of sites as title
  
  # Convert to grob
  ggplotGrob(boxplot)
}

# Pre-create boxplot grobs
boxplot_grobs <- lapply(pertinent_ecoregions, function(ecoregion) {
  list(name = ecoregion, grob = create_ecoregion_boxplot(ecoregion, average_seasonal_vwm_with_ecoregion))
})

# Loop to place boxplot grobs on the map at specified coordinates
for (i in seq_len(nrow(ecoregion_coords))) {
  ecoregion_name <- ecoregion_coords$ecoregion[i]
  x_coord <- ecoregion_coords$x[i]
  y_coord <- ecoregion_coords$y[i]
  
  # Retrieve pre-generated boxplot grob
  boxplot_grob <- boxplot_grobs[[which(sapply(boxplot_grobs, function(x) x$name) == ecoregion_name)]]$grob
  
  base_map <- base_map +
    annotation_custom(
      grob = boxplot_grob,
      xmin = x_coord - 5, xmax = x_coord + 5,
      ymin = y_coord - 5, ymax = y_coord + 5
    )
}

# Final adjustments for map focus and display
final_map <- base_map +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
  theme(text = element_text(size = 10))  # Smaller font size for better readability

# Display the final map
print(final_map)
ggsave(here("figures/seasonal/ecoregion.png"), plot = final_map, width = 12, height = 7, dpi = 300)



