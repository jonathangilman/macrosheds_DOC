# Load necessary libraries
library(terra)
library(fs)
library(here)
library(ggplot2)

# Define directories
zip_dir <- here("data/precip_ph/precip_ph_raw")  # Path to ZIP files
output_dir <- here("data/precip_ph")            # Directory for extracted files

# Create the output directory if it doesn't exist
if (!dir_exists(output_dir)) {
  dir_create(output_dir)
}

# Step 1: Extract all ZIP files
zip_files <- dir_ls(zip_dir, glob = "*.zip")  # List all ZIP files
for (zip_file in zip_files) {
  unzip(zip_file, exdir = output_dir)  # Extract each ZIP file
}

# Step 2: Gather all .tif files from extracted folders
tif_files <- dir_ls(output_dir, glob = "*.tif", recurse = TRUE)

# Step 3: Move all .tif files to the main output directory
for (tif_file in tif_files) {
  file_move(tif_file, output_dir)
}

# Step 4: Optional - Clean up empty directories
empty_dirs <- dir_ls(output_dir, type = "directory")
if (length(empty_dirs) > 0) {
  dir_delete(empty_dirs)
}

# Step 5: Read in the TIFF files
files <- list.files(output_dir, pattern = ".tif$", full.names = TRUE)

# Load all the rasters into a list
rasters <- lapply(files, rast)

# Step 6: Check CRS, Resolution, and Extent
reference_raster <- rasters[[1]]  # Use the first raster as the reference

# Reproject and align all rasters to match the reference raster
aligned_rasters <- lapply(rasters, function(r) {
  if (!identical(crs(r), crs(reference_raster))) {  # Compare CRS
    r <- project(r, crs(reference_raster))  # Reproject if needed
  }
  resample(r, reference_raster)  # Align resolution and extent
})

# Step 7: Stack all aligned rasters
raster_stack <- rast(aligned_rasters)

# Step 8: Extract years from filenames
library(stringr)
years <- as.numeric(str_extract(files, "\\d{4}"))

# Check for valid years
if (anyNA(years)) {
  print("Files with invalid year format:")
  print(files[is.na(years)])
  stop("Year extraction failed. Ensure filenames include a 4-digit year.")
}

# Assign names to raster stack layers
names(raster_stack) <- paste0("Year_", years)

# Step 9: Calculate linear trends at each pixel
# Define a function to compute trends
calculate_trend <- function(values) {
  # Ensure there is enough non-NA data to fit a model
  if (sum(!is.na(values)) < 2) {
    return(c(slope = NA, intercept = NA, r_squared = NA))
  }
  # Fit the linear model
  model <- lm(values ~ years)
  c(slope = coef(model)[2], intercept = coef(model)[1], r_squared = summary(model)$r.squared)
}

# Apply the function to the raster stack
trend <- app(raster_stack, calculate_trend)

# Step 10: Separate trend components into individual rasters
slope <- trend[[1]]
intercept <- trend[[2]]
r_squared <- trend[[3]]

# Step 11: Save the trend results
writeRaster(slope, here("data/precip_ph/precip_ph_results/pH_trend_slope.tif"), overwrite = TRUE)
writeRaster(intercept, here("data/precip_ph/precip_ph_results/pH_trend_intercept.tif"), overwrite = TRUE)
writeRaster(r_squared, here("data/precip_ph/precip_ph_results/pH_trend_r_squared.tif"), overwrite = TRUE)

# Step 12: Visualize the slope raster
slope_df <- as.data.frame(slope, xy = TRUE)
ggplot(slope_df, aes(x = x, y = y, fill = slope)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "pH Trend Slope (1985-2023)",
    fill = "Slope"
  )

plot(trend[[1]], main = "Slope")
plot(trend[[2]], main = "Intercept")
plot(trend[[3]], main = "R-squared")

# Convert the slope layer to a data frame for ggplot
slope_df <- as.data.frame(trend[[1]], xy = TRUE, na.rm = TRUE)

# Plot with custom limits for color scale
ggplot(slope_df, aes(x = x, y = y, fill = slope.years)) +
  geom_raster() +
  scale_fill_gradient2(
    name = "Slope",
    low = "purple", mid = "white", high = "yellow",
    midpoint = 0, limits = c(-0.28, 0.34)
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA), # Light gray background
    panel.grid = element_line(color = "white"),                  # White grid lines
    panel.border = element_rect(color = "black", fill = NA),     # Border around plot
    legend.position = "right",                                   # Adjust legend position
    axis.title = element_text(size = 14),                        # Larger axis titles
    axis.text = element_text(size = 12)                          # Larger axis text
  ) +
  labs(
    title = "Slope of pH Trends",
    x = "Longitude",
    y = "Latitude"
  )

r_squared <- trend[[3]]  # Assuming the third layer is r_squared
r_squared_df <- as.data.frame(r_squared, xy = TRUE, na.rm = TRUE)
colnames(r_squared_df)[3] <- "r_squared"  # Rename the column
slope_df <- merge(slope_df, r_squared_df, by = c("x", "y"))

ggplot(slope_df, aes(x = x, y = y, fill = r_squared)) +
  geom_raster() +
  scale_fill_viridis_c(name = expression(R^2), option = "viridis", limits = c(0, 1)) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA), # Light gray background
    panel.grid = element_line(color = "white"),                  # White grid lines
    panel.border = element_rect(color = "black", fill = NA),     # Border around plot
    legend.position = "right",                                   # Adjust legend position
    axis.title = element_text(size = 14),                        # Larger axis titles
    axis.text = element_text(size = 12)                          # Larger axis text
  ) +
  labs(
    title = expression("R"^2 ~ "of pH Trends"),
    x = "Longitude",
    y = "Latitude"
  )


###############################################################
####### coming back to saved data 


# Define file paths
slope_path <- here("data/precip_ph/precip_ph_results/pH_trend_slope.tif")
intercept_path <- here("data/precip_ph/precip_ph_results/pH_trend_intercept.tif")
r_squared_path <- here("data/precip_ph/precip_ph_results/pH_trend_r_squared.tif")

# Load the rasters
slope <- rast(slope_path)
intercept <- rast(intercept_path)
r_squared <- rast(r_squared_path)

# Convert slope raster to data frame
slope_df <- as.data.frame(slope, xy = TRUE, na.rm = TRUE)
colnames(slope_df)[3] <- "slope"

# Convert R-squared raster to data frame
r_squared_df <- as.data.frame(r_squared, xy = TRUE, na.rm = TRUE)
colnames(r_squared_df)[3] <- "r_squared"

# Merge slope and R-squared data frames
trend_df <- merge(slope_df, r_squared_df, by = c("x", "y"))

slope_plot <- ggplot(trend_df, aes(x = x, y = y, fill = slope)) +
  geom_raster() +
  scale_fill_gradient2(
    name = "Slope",
    low = "yellow", mid = "white", high = "purple",
    midpoint = 0, limits = c(-0.28, 0.34)
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    panel.grid = element_line(color = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    title = "Slope of pH Trends",
    x = "Longitude",
    y = "Latitude"
  )
ggsave(here("figures/atmospheric_ph/slope.png"), plot = slope_plot, width = 10, height = 7, dpi = 300)


r2_plot <- ggplot(trend_df, aes(x = x, y = y, fill = r_squared)) +
  geom_raster() +
  scale_fill_viridis_c(
    name = expression(R^2),
    option = "viridis",
    limits = c(0, 1)
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    panel.grid = element_line(color = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    title = expression("R"^2 ~ "of pH Trends"),
    x = "Longitude",
    y = "Latitude"
  )
ggsave(here("figures/atmospheric_ph/r2.png"), plot = r2_plot, width = 10, height = 7, dpi = 300)


############## Add significance

# Load necessary libraries
library(terra)
library(stringr)
library(fs)
library(here)
library(ggplot2)

# Step 1: Define reference extent from the first raster file
files <- dir_ls(here("data/precip_ph"), glob = "*.tif")
reference_raster <- rast(files[1])  # Use the first raster as the reference
reference_extent <- ext(reference_raster)

# Step 2: Align all rasters to the common extent
aligned_rasters <- lapply(files, function(file) {
  r <- rast(file)
  
  # Check if the raster aligns with the reference raster geometry
  if (!compareGeom(r, reference_raster, stopOnError = FALSE)) {
    # Align the raster by extending to match the reference extent
    r <- extend(r, reference_extent)
    # Resample the raster to ensure alignment
    r <- resample(r, reference_raster, method = "bilinear")
  }
  
  return(r)
})

# Step 3: Combine the aligned rasters into a stack
raster_stack <- rast(aligned_rasters)

# Verify the stack
print(raster_stack)

# Step 4: Extract years from filenames
years <- as.numeric(str_extract(files, "\\d{4}"))
print(years)
if (anyNA(years)) {
  stop("Year extraction failed. Ensure filenames include a valid 4-digit year.")
}

# Step 5: Create a mask for valid pixels (at least 5 non-NA values)
valid_data_coverage <- app(raster_stack, function(values) sum(!is.na(values)))
sufficient_data_mask <- valid_data_coverage >= 5
raster_stack <- mask(raster_stack, sufficient_data_mask)

# Updated calculate_p_value function with debugging
calculate_p_value <- function(values, years) {
  # Handle missing data or insufficient observations
  if (all(is.na(values)) || sum(!is.na(values)) < 2) {
    return(NA)
  }
  
  # Filter out NA values and match years
  valid_indices <- !is.na(values)
  valid_values <- values[valid_indices]
  valid_years <- years[seq_along(values)][valid_indices]
  
  # Perform regression
  model <- lm(valid_values ~ valid_years)
  coefficients <- summary(model)$coefficients
  slope <- coefficients[2, 1]  # Slope
  p_value <- coefficients[2, 4]  # P-value
  
  return(p_value)
}

# Step 6: Calculate p-values for the raster stack
p_value_raster <- app(raster_stack, function(values) calculate_p_value(values, years))

# Step 7: Save the p-value raster
writeRaster(p_value_raster, here("data/precip_ph/precip_ph_results/pH_trend_p_values.tif"), overwrite = TRUE)

# Step 8: Create a significance map (binary raster)
significance <- p_value_raster < 0.05
writeRaster(significance, here("data/precip_ph/precip_ph_results/pH_trend_significance.tif"), overwrite = TRUE)

# Step 9: Convert significance raster to a dataframe
significance_df <- as.data.frame(significance, xy = TRUE, na.rm = TRUE)
colnames(significance_df)[3] <- "significance"

# Convert logical to numeric and then to factor
significance_df$significance <- factor(as.numeric(significance_df$significance), levels = c(0, 1))

# Step 10: Visualize the significance map
sig_plot <- ggplot(significance_df, aes(x = x, y = y, fill = significance)) +
  geom_raster() +
  scale_fill_manual(
    name = "Significance",
    values = c("0" = "grey", "1" = "blue"),  # Grey = Not significant, Blue = Significant
    labels = c("Not Significant", "Significant")
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    panel.grid = element_line(color = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    title = "Significance of pH Trends (1985-2023)",
    x = "Longitude",
    y = "Latitude"
  )
ggsave(here("figures/atmospheric_ph/sig_plot.png"), plot = sig_plot, width = 10, height = 7, dpi = 300)


# Step 11: Overlay significance on the slope map
slope <- rast(here("data/precip_ph/precip_ph_results/pH_trend_slope.tif"))
slope_df <- as.data.frame(slope, xy = TRUE, na.rm = TRUE)
colnames(slope_df)[3] <- "slope"
combined_df <- merge(slope_df, significance_df, by = c("x", "y"))

# Plot slope map with significance overlay
ggplot(combined_df, aes(x = x, y = y, fill = slope)) +
  geom_raster(aes(alpha = significance)) +  # Transparency based on significance
  scale_fill_gradient2(
    name = "Slope",
    low = "purple", mid = "white", high = "yellow",
    midpoint = 0
  ) +
  scale_alpha_manual(
    values = c("0" = 0.3, "1" = 1),  # Less opaque for non-significant pixels
    name = "Significance",
    labels = c("Not Significant", "Significant")
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    panel.grid = element_line(color = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    title = "Slope of pH Trends with Significance Overlay",
    x = "Longitude",
    y = "Latitude"
  )
