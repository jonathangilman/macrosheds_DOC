# Explore MacroSheds data
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(here)
library(stringr)
library(purrr)
library(macrosheds)
library(ggplot2)
library(ggrepel)
library(scales)
library(maps)
library(kableExtra)

# Load processed data
processed_data <- readRDS(here("data/Processed/processed_macrosheds_data.rds"))
colnames(processed_data)

# Set custom theme
custom_theme <- theme(
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

# Extract distinct domain metadata for mapping and summarization
domain_data <- processed_data %>%
    distinct(domain, latitude, longitude, domain_fullname, site_code)

# Calculate total DOC observations per domain
doc_obs <- processed_data %>%
    count(domain, name = "count") %>%
    arrange(desc(count))

# Plot total DOC observations per domain
doc_obs_plot <- ggplot(doc_obs, aes(x = reorder(domain, -count), y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(title = "DOC Measurements per Domain", x = "Domain", y = "Number of Measurements") +
    custom_theme +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(here("output/exploration_figs/doc_obs.png"), plot = doc_obs_plot, width = 6, height = 5, dpi = 300)

# Calculate data collection duration per domain
domain_duration <- processed_data %>%
    group_by(domain) %>%
    summarize(
        first_date = min(date, na.rm = TRUE),
        last_date = max(date, na.rm = TRUE),
        duration_years = as.numeric(difftime(last_date, first_date, units = "days")) / 365
    ) %>%
    arrange(desc(duration_years))

# Plot data collection duration per domain
duration_plot <- ggplot(domain_duration, aes(x = reorder(domain, -duration_years), y = duration_years)) +
    geom_bar(stat = "identity", fill = "lightgreen") +
    theme_minimal() +
    labs(title = "Data Collection Duration for DOC per Domain", x = "Domain", y = "Duration (Years)") +
    custom_theme +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(here("output/exploration_figs/duration_plot.png"), plot = duration_plot, width = 6, height = 5, dpi = 300)

# Calculate mean observations per day per domain
domain_stats <- doc_obs %>%
    left_join(domain_duration, by = "domain") %>%
    mutate(mean_obs_per_day = count / (duration_years * 365))

# Plot mean observations per day per domain
mean_obs_plot <- ggplot(domain_stats, aes(x = reorder(domain, -mean_obs_per_day), y = mean_obs_per_day)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    theme_minimal() +
    labs(title = "Mean Observations per Day", x = "Domain", y = "Mean Observations per Day") +
    custom_theme +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(here("output/exploration_figs/mean_obs.png"), plot = mean_obs_plot, width = 8, height = 5, dpi = 300)

# Calculate summary stats for each domain
summary_stats <- processed_data %>%
    group_by(domain) %>%
    summarize(
        min_val = min(val.x, na.rm = TRUE),
        max_val = max(val.x, na.rm = TRUE),
        mean_val = mean(val.x, na.rm = TRUE),
        median_val = median(val.x, na.rm = TRUE),
        sd_val = sd(val.x, na.rm = TRUE),
        count = n()
    )

# Calculate summary stats for each domain and order by count
summary_stats <- processed_data %>%
    group_by(domain) %>%
    summarize(
        count = n(),  # Move 'count' to the first calculation
        min_val = min(val.x, na.rm = TRUE),
        max_val = max(val.x, na.rm = TRUE),
        mean_val = mean(val.x, na.rm = TRUE),
        median_val = median(val.x, na.rm = TRUE),
        sd_val = sd(val.x, na.rm = TRUE)
    ) %>%
    arrange(desc(count))  # Order by count in descending order

# Create a nicely formatted table
summary_table <- summary_stats %>%
    kable(
        format = "html",
        caption = "Summary Statistics for Each Domain (Ordered by Count)",
        col.names = c("Domain", "Count", "Min Value", "Max Value", "Mean Value", "Median Value", "Standard Deviation"),
        digits = 2
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

# Display the table in the RStudio Viewer or knit to an RMarkdown document
summary_table

# Map locations of domains in the continental US
us_map <- map_data("state")

# Ensure each domain has only one unique set of coordinates
continental_us_domains <- processed_data %>%
    group_by(domain) %>%
    summarize(
        latitude = mean(latitude, na.rm = TRUE),
        longitude = mean(longitude, na.rm = TRUE)
    )

# Plot the map with updated domain coordinates
map <- ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = continental_us_domains, aes(x = longitude, y = latitude), color = "red", size = 2) +
    geom_text_repel(data = continental_us_domains, aes(x = longitude, y = latitude, label = domain), size = 3) +
    labs(title = "Locations of Domains in Continental US", x = "Longitude", y = "Latitude") +
    coord_fixed(1.3) +
    theme_minimal()

# Save the map
ggsave(here("output/exploration_figs/map.png"), plot = map, width = 8, height = 6, dpi = 300)


# Calculate the start and end dates, and duration for each site
site_data <- processed_data %>%
    group_by(site_code) %>%
    summarize(
        start_date = min(date, na.rm = TRUE),
        end_date = max(date, na.rm = TRUE),
        duration = as.numeric(difftime(end_date, start_date, units = "days"))
    ) %>%
    arrange(desc(duration))  # Sort by duration, longest on top

# Reorder the site_code factor based on the sorted duration
site_data <- site_data %>%
    mutate(site_code = factor(site_code, levels = rev(site_code)))  # Reverse the order

n_sites <- n_distinct(site_data$site_code)

# Create the plot using ggplot with site names and site count annotation
timeseries <- ggplot(site_data, aes(x = start_date, xend = end_date, y = site_code, yend = site_code)) +
    geom_segment(linewidth = 0.4) +
    labs(title = "MS Site Data: DOC", x = "Year", y = "Site Code") +
    annotate("text", x = min(site_data$start_date), y = 0, label = paste0("n = ", n_sites, " sites"),
             color = "blue", size = 5, hjust = 0, vjust = -1) +  # Add annotation in bottom-left
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8, color = "black"),  # Display site names
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
        axis.ticks.x = element_line(color = "black")
    )
timeseries

# Save the plot
ggsave(here("output/exploration_figs/ribbon_plot.png"), plot = timeseries, width = 8, height = 10, dpi = 300)




