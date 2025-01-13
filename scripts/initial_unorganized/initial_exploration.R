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

# look at DOC data
filter(ms_vars, grepl('carbon, dissolved organic', variable_name, ignore.case = TRUE))

# filter for variable code DOC and chem_category stream_flux
ms_sitevar_catalog <- ms_load_variables(var_set = 'timeseries_by_site')
ms_doc <- ms_sitevar_catalog %>% 
  filter(variable_code == 'DOC',
         chem_category == 'stream_conc'
  ) %>% 
  arrange(desc(observations), desc(mean_obs_per_day))

unique(ms_doc$domain)
unique(ms_sitevar_catalog$domain)
unique(ms_sitevar_catalog$variable_name)

ms_doc_hbef <- ms_doc %>% 
  filter(domain == 'hbef')
sum(ms_doc_hbef$observations)

# calculate total observations per domain
domain_summary <- ms_doc %>%
  group_by(domain) %>%
  summarize(
    total_observations = sum(observations, na.rm = TRUE),
    first_record = min(ymd(first_record)),  # Convert to date format
    last_record = max(ymd(last_record)),    # Convert to date format
    collection_duration = as.numeric(difftime(last_record, first_record, units = "days")),
    mean_obs_per_day = sum(mean_obs_per_day, na.rm = TRUE),  # Sum instead of mean
    unique_site_codes = n_distinct(site_code)  # Count unique site codes for each domain
  ) %>%
  arrange(desc(total_observations))

sum(domain_summary$total_observations)
#### 850,308 observations
min(domain_summary$first_record)
#### 8/21/1975

# plot total observations per domain
tot_obs <- ggplot(domain_summary, aes(x = reorder(domain, -total_observations), y = total_observations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Total Observations per Domain", x = "Domain", y = "Total Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
tot_obs
ggsave(here("figures/initial_exploration/total_observations.png"), plot = tot_obs, width = 6, height = 5, dpi = 300)


# Modify the summary to convert duration to years
domain_summary <- domain_summary %>%
  mutate(collection_duration_years = collection_duration / 365)  # Convert days to years
# Plot the data collection duration in years
duration <- ggplot(domain_summary, aes(x = reorder(domain, -collection_duration_years), y = collection_duration_years)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Data Collection Duration per Domain (in Years)", x = "Domain", y = "Duration (Years)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
duration
ggsave(here("figures/initial_exploration/domain_duration.png"), plot = duration, width = 6, height = 5, dpi = 300)


# display summary table for initial exploration
print(domain_summary)
# print table using datatable
datatable(domain_summary, 
          caption = 'Domain Summary Table',
          options = list(pageLength = 5, autoWidth = TRUE))
# Save the summary table as a CSV file
write.csv(domain_summary, file = here("figures","initial_exploration", "domain_summary.csv"), row.names = FALSE)





#########################################
############# PULL DOWN DATA
#########################################

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

# retrieve chemistry data
doc_chem <- ms_load_product(
  my_ms_dir,
  prodname = 'stream_chemistry',
  filter_vars = 'DOC'
)

nrow(doc_chem)
#### 1,448,327 observations -- different from ms_doc number of observations ??
min(doc_chem$date)
#### 8/21/1975


head(doc_chem)

# Merge domain from ms_doc into doc_chem using site_code
doc_chem_with_domain <- doc_chem %>%
  left_join(ms_doc %>% select(site_code, domain), by = "site_code")

unique(doc_chem_with_domain$ms_interp)
# Filter to keep only rows where ms_interp is equal to 0
filtered_doc_chem_with_domain <- doc_chem_with_domain %>%
  filter(ms_interp == 0)

# TEST : Filter to keep only rows where ms_interp is not equal to 0
interp_doc_chem <- doc_chem_with_domain %>%
  filter(ms_interp != 0)
n_interp0 <- nrow(filtered_doc_chem_with_domain)
#### 85,895 observations
n_interp1 <- nrow(interp_doc_chem_with_domain)
#### 1,362,432
#### SUM : 1,448,327 okay



########### plot DOC values (mg/L) by domain from most observations to least
########### stacked bar plot with interpolated values

# Calculate the number of observations for ms_interp == 0
domain_order <- doc_chem_with_domain %>%
  group_by(domain) %>%
  summarize(
    count = n(),
    median_val = median(val, na.rm = TRUE)
  ) 
str(domain_order)
# Calculate the number of observations for ms_interp != 0
#interp_domain_order <- interp_doc_chem %>%
#  group_by(domain) %>%
#  summarize(
#    count = n(),
#    median_val = median(val, na.rm = TRUE)
#  ) %>%
#  mutate(ms_interp_status = "ms_interp = 1")  # Label for stacking

# Combine both datasets
#combined_order <- bind_rows(domain_order, interp_domain_order)



################  STACKED PLOT 1

# Ensure ms_interp = 0 is on the bottom
combined_order$ms_interp_status <- factor(combined_order$ms_interp_status, levels = c("ms_interp = 1", "ms_interp = 0"))

# Reorder domains based on ms_interp == 0 counts
combined_order <- combined_order %>%
  mutate(domain = factor(domain, levels = domain_order$domain[order(-domain_order$count)]))

# Create the stacked bar plot with reordered domains
doc_obs_plot1 <- ggplot(combined_order, aes(x = domain, y = count, fill = ms_interp_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +  # Custom colors (blue for ms_interp != 0, red for ms_interp == 0)
  theme_minimal() +
  labs(title = "DOC Measurements per Domain", x = "Domain", y = "Number of Measurements", fill = "ms_interp Status") +
  scale_y_continuous(labels = comma) +  # Format y-axis with commas
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

# Display the plot
doc_obs_plot1
#ggsave(here("figures/initial_exploration/doc_stacked1.png"), plot = doc_obs_plot1, width = 8, height = 5, dpi = 300)


################  STACKED PLOT 2

# Now ensure ms_interp = 0 is on the top
combined_order$ms_interp_status <- factor(combined_order$ms_interp_status, levels = c("ms_interp = 1", "ms_interp = 0"))
# Reorder domains based on ms_interp = 1 counts (blue bars)
combined_order <- combined_order %>%
  mutate(domain = factor(domain, levels = interp_domain_order$domain[order(-interp_domain_order$count)]))
# Create the stacked bar plot with reordered domains based on ms_interp = 1
doc_obs_plot2 <- ggplot(combined_order, aes(x = domain, y = count, fill = ms_interp_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +  # Custom colors (blue for ms_interp = 1, red for ms_interp = 0)
  theme_minimal() +
  labs(title = "DOC Measurements per Domain", x = "Domain", y = "Number of Measurements", fill = "ms_interp Status") +
  scale_y_continuous(labels = comma) +  # Format y-axis with commas
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.8),   # Places legend in the upper right (x, y coordinates from 0 to 1)
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: add background and border to legend

# Display the plot
doc_obs_plot2

combined_stacked <- doc_obs_plot2 | doc_obs_plot1
ggsave(here("figures/initial_exploration/doc_stacked.png"), plot = combined_stacked, width = 14, height = 5, dpi = 300)



########### plot DOC values (mg/L) by domain from most observations to least
########### no interpolated values

# Define the domains you want to filter out for CONUS
domains_to_exclude <- c("arctic", "bonanza", "krycklan", "luquillo", "mcmurdo")
# Filter the dataframe to exclude the specified domains
domain_order <- domain_order %>%
  filter(!domain %in% domains_to_exclude)


# plot total DOC observations per domain
doc_obs_plot3 <- ggplot(domain_order, aes(x = reorder(domain, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_y_continuous(labels = scales::comma) +  # Add commas to y-axis labels
  theme_minimal() +
  labs(title = "DOC Measurements per Domain", x = "Domain", y = "Number of Measurements") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    axis.ticks.x = element_line(color = "black"))# Remove all gridlines

# Display the plot
doc_obs_plot3
ggsave(here("figures/initial_exploration/total_doc_observations.png"), plot = doc_obs_plot3, width = 6, height = 5, dpi = 300)


########### DURATION PLOT

# Filter the dataframe to exclude the specified domains
filtered_doc_chem_with_domain <- filtered_doc_chem_with_domain %>%
  filter(!domain %in% domains_to_exclude)

# Check the filtered dataframe
filtered_doc_chem_with_domain
unique(filtered_doc_chem_with_domain$domain)

# Convert the 'date' column to Date format
filtered_doc_chem_with_domain$date <- as.Date(filtered_doc_chem_with_domain$date)
# Calculate the data collection duration for each domain
domain_duration <- filtered_doc_chem_with_domain %>%
  group_by(domain) %>%
  summarize(
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    duration_years = as.numeric(difftime(last_date, first_date, units = "days")) / 365  # Duration in years
  ) %>%
  arrange(desc(duration_years))  # Sort by duration
print(domain_duration)
# Create a barplot for data collection duration
duration_plot <- ggplot(domain_duration, aes(x = reorder(domain, -duration_years), y = duration_years)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Data Collection Duration for DOC per Domain", x = "Domain", y = "Duration (Years)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    axis.ticks.x = element_line(color = "black"))
duration_plot
ggsave(here("figures/initial_exploration/duration_plot.png"), plot = duration_plot, width = 6, height = 5, dpi = 300)


########### MEAN OBSERVATIONS PER DAY PLOT

# Check the structure of domain_order
str(domain_order)

# Check the structure of domain_duration
str(domain_duration)


# Merge domain_order and domain_duration to calculate mean observations per day
domain_stats <- domain_order %>%
  left_join(domain_duration, by = "domain") %>%
  mutate(mean_obs_per_day = count / (duration_years * 365))  # Calculate mean obs per day

# Merge domain_stats with domain_summary to add the unique_site_codes column
domain_stats <- domain_stats %>%
  left_join(domain_summary %>% select(domain, unique_site_codes), by = "domain")

# Create the bar plot for mean observations per day
mean_obs_plot <- ggplot(domain_stats, aes(x = reorder(domain, -mean_obs_per_day), y = mean_obs_per_day)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Mean Observations per Day", x = "Domain", y = "Mean Observations per Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # Add site count above each bar
  geom_text(aes(label = paste("n=",unique_site_codes), y = mean_obs_per_day + 0.02), 
            vjust = 0.01, size = 2.3)

# Display the plot
mean_obs_plot
ggsave(here("figures/initial_exploration/mean_obs_per_day.png"), plot = mean_obs_plot, width = 8, height = 5, dpi = 300)


########### DOC VALUES PLOT

# Reorder domain by the number of observations
domain_order <- domain_stats %>%
  arrange(desc(count)) %>%
  pull(domain)

# Reorder domain factor levels by observation count
filtered_doc_chem_with_domain <- filtered_doc_chem_with_domain %>%
  mutate(domain = factor(domain, levels = domain_order))

# Calculate observation count and maximum val per domain
domain_stats <- filtered_doc_chem_with_domain %>%
  group_by(domain) %>%
  summarize(
    count = n(),
    max_val = max(val, na.rm = TRUE)  # Get the maximum value of val for each domain
  )

# Merge the observation counts and max values with the original data
filtered_doc_chem_with_domain <- filtered_doc_chem_with_domain %>%
  left_join(domain_stats, by = "domain")

# Create the boxplot with observation count labels 5 units above the max value
all_plot <- ggplot(domain_order, aes(x = domain, y = val)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  theme_minimal() +
  labs(title = "DOC Values by Domain", 
       x = "Domain", 
       y = "DOC (mg/L)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    axis.ticks.x = element_line(color = "black"))+
  # Add the observation count 5 units above the maximum value for each domain
  geom_text(data = domain_stats, aes(x = domain, y = max_val + 5, label = paste("n=", scales::comma(count))), 
            inherit.aes = FALSE, vjust = -0.5, size = 3)  # Adjust position and size as needed

# Display the plot
all_plot
ggsave(here("figures/initial_exploration/all_plot.png"), plot = all_plot, width = 10, height = 6, dpi = 300)

  
all_plot_removed_outliers <- ggplot(filtered_doc_chem_with_domain, aes(x = domain, y = val)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  theme_minimal() +
  labs(title = "DOC Values by Domain", 
       x = "Domain", 
       y = "DOC (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 90))+
  # Add the observation count 5 units above the maximum value for each domain
  geom_text(data = domain_stats, aes(x = domain, y = max_val + 5, label = paste("n=", count)), 
            inherit.aes = FALSE, vjust = -0.5, size = 3)  # Adjust position and size as needed

all_plot_removed_outliers

# add # of observations
# split the domains into three groups of 7 domains each
# because 21 was too cluttered to see at once
#domains_group1 <- domain_order$domain[1:7]
#domains_group2 <- domain_order$domain[8:14]
#domains_group3 <- domain_order$domain[15:21]

# Create a function to generate the plots with consistent y-axis limits
#create_boxplot <- function(domains_subset) {
#  ggplot(filtered_doc_chem_with_domain %>% filter(domain %in% domains_subset), aes(x = domain, y = val)) +
#    geom_boxplot(fill = "lightblue", outlier.color = "red") +
#    theme_minimal() +
#    labs(title = "Boxplot of DOC Values by Domain", x = "Domain", y = "DOC (mg/L)") +
#    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#    geom_text(data = domain_order %>% filter(domain %in% domains_subset), 
#              aes(x = domain, y = median_val, label = paste("n =", count)), 
#              inherit.aes = FALSE, vjust = -0.5, size = 2.5, color = "black") +
#    scale_y_continuous(limits = c(0, 75))  # Ensure the same y-axis scale for all plots
#}

# Create plots
#plot_group1 <- create_boxplot(domains_group1)
#plot_group2 <- create_boxplot(domains_group2)
#plot_group3 <- create_boxplot(domains_group3)

#combined_plot <- plot_group1 / plot_group2 / plot_group3
#combined_plot
#ggsave(here("figures/initial_exploration/doc_val_bydomain.png"), plot = combined_plot, width = 6, height = 10, dpi = 300)
#ggsave(here("figures/initial_exploration/doc_val_1.png"), plot = plot_group1, width = 6, height = 4, dpi = 300)
#ggsave(here("figures/initial_exploration/doc_val_2.png"), plot = plot_group2, width = 6, height = 4, dpi = 300)
#ggsave(here("figures/initial_exploration/doc_val_3.png"), plot = plot_group3, width = 6, height = 4, dpi = 300)
ggsave(here("figures/initial_exploration/all_plot.png"), plot = all_plot, width = 10, height = 6, dpi = 300)
ggsave(here("figures/initial_exploration/all_plot_removed_outliers.png"), plot = all_plot_removed_outliers, width = 10, height = 6, dpi = 300)


# Summary stats for each domain
summary_stats <- filtered_doc_chem_with_domain %>%
  group_by(domain) %>%
  summarize(
    min_val = min(val, na.rm = TRUE),
    max_val = max(val, na.rm = TRUE),
    mean_val = mean(val, na.rm = TRUE),
    median_val = median(val, na.rm = TRUE),
    sd_val = sd(val, na.rm = TRUE),
    count = n()
  )

# Print summary statistics table
print(summary_stats)
# print table using datatable
datatable(summary_stats, 
          caption = 'DOC Value by Domain Summary Table',
          options = list(pageLength = 5, autoWidth = TRUE))
# Save the summary table as a CSV file
write.csv(summary_stats, file = here("figures","initial_exploration", "summary_stats.csv"), row.names = FALSE)



#######################  Map  ####################

# Extract the unique domain names from filtered_doc_chem_with_domain
doc_domains <- unique(filtered_doc_chem_with_domain$domain)

# Filter site_data to include only those domains
filtered_site_data <- site_data %>%
  filter(domain %in% doc_domains)

# View the filtered site_data
print(filtered_site_data)

# Reduce to one row per domain
domain_data <- filtered_site_data %>%
  group_by(domain) %>%
  summarize(
    latitude = first(latitude),    # Assuming latitude is consistent within a domain, take the first value
    longitude = first(longitude),  # Same for longitude
  )

# View the resulting dataframe
print(domain_data)


library(maps)

# Filter to keep only domains within the continental US latitude and longitude ranges
continental_us_domains <- domain_data %>%
  filter(longitude >= -125 & longitude <= -66, latitude >= 24 & latitude <= 50)

# Step 1: Get a map of the US
us_map <- map_data("state")  # Load US map data

# Step 2: Plot the US map and overlay the domain locations
map <- ggplot() +
  # Plot the US map
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  # Jitter the domain locations to avoid overlap and add an outline
  geom_jitter(data = continental_us_domains, aes(x = longitude, y = latitude), 
              color = "black", fill = "red", size = 3, shape = 21, stroke = 0.3, 
              width = 0.5, height = 0.5) +  # Stroke adds the outline, shape 21 allows for both fill and outline
  # Add non-overlapping labels for each domain
  geom_text_repel(data = continental_us_domains, aes(x = longitude, y = latitude, label = domain), 
                  size = 3, box.padding = 0.5, point.padding = 0.1,  # Controls distance between point and label
                  segment.color = 'grey50', segment.size = 0.5, 
                  min.segment.length = 0,  # Force lines to appear for every point
                  max.overlaps = Inf, force = 2) +  # Increase force to push labels away
  # Add titles and labels
  labs(title = "Locations of the 16 Domains in Continental US", x = "Longitude", y = "Latitude") +
  # Set the map coordinates
  coord_fixed(1.3) +  # Keeps the aspect ratio of the map correct
  theme_minimal()
map
ggsave(here("figures/initial_exploration/map.png"), plot = map, width = 8, height = 6, dpi = 300)


# Find the domains that were filtered out
filtered_out_domains <- anti_join(domain_data, continental_us_domains, by = "domain")
# View the filtered out domains
print(filtered_out_domains)


###############################################
###############  MCMURDO
###############################################

mcmurdo_info <- ms_sites %>%
  filter(domain== "mcmurdo")
mcmurdo <- filtered_doc_chem_with_domain %>%
  filter(domain == "mcmurdo")
mcmurdo_max <- mcmurdo %>%
  filter(val > 150)
mcmurdo_redbfalls <- mcmurdo %>%
  filter(site_code == "red_bfalls")
mcmurdo_redbfalls_recent <- mcmurdo_redbfalls %>%
  filter(date > "2018-12-22", date < "2020-02-19")



######## mcmurdo plots
# Create the plot
mcmurdo_plot1 <- ggplot(mcmurdo_redbfalls, aes(x = date, y = val)) +
  geom_point(color = "blue") +  # Plot the points
  geom_line(color = "darkblue") +  # Connect the points with lines
  theme_minimal() +
  labs(title = "Red River Blood Falls DOC", x = "Date", y = "DOC (mg/L)")
  # Add labels for values above 150 mg/L
  #geom_text(data = subset(mcmurdo_redbfalls, val > 150), aes(label = round(val, 1)), 
           # vjust = -0.5, color = "red", size = 3)  # Adjust position and size
mcmurdo_plot1

mcmurdo_plot2 <- ggplot(mcmurdo_redbfalls_recent, aes(x = date, y = val)) +
  geom_point(color = "blue") +  # Plot the points
  geom_line(color = "darkblue") +  # Connect the points with lines
  theme_minimal() +
  labs(title = "Red River Blood Falls DOC", x = "Date", y = "DOC (mg/L)")+
  #Add labels for values above 150 mg/L
 geom_text(data = mcmurdo_redbfalls_recent, aes(label = round(val, 1)), 
 vjust = -0.5, color = "red", size = 4)+ # Adjust position and size
  ylim(0,210)+
  scale_x_date(labels = date_format("%d-%b-%Y"))+
  theme(axis.text.x = element_text(angle = -10, vjust = -.75))
mcmurdo_plot2

# Save the plots
ggsave(here("figures/initial_exploration/mcmurdo.png"), plot = mcmurdo_plot1, width = 8, height = 5, dpi = 300)
ggsave(here("figures/initial_exploration/mcmurdo_recent.png"), plot = mcmurdo_plot2, width = 8, height = 5, dpi = 300)




