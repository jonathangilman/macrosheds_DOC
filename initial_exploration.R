library(macrosheds)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(here)
library(patchwork)

help(package = macrosheds)

# load sites
?ms_load_sites
ms_sites <- ms_load_sites()
colnames(ms_sites)

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

# calculate total observations per domain
domain_summary <- ms_doc %>%
  group_by(domain) %>%
  summarize(
    total_observations = sum(observations, na.rm = TRUE),
    first_record = min(ymd(first_record)),  # Convert to date format
    last_record = max(ymd(last_record)),    # Convert to date format
    collection_duration = as.numeric(difftime(last_record, first_record, units = "days")),
    mean_obs_per_day = mean(mean_obs_per_day, na.rm = TRUE),
    unique_site_codes = n_distinct(site_code)  # Count unique site codes for each domain
  ) %>%
  arrange(desc(total_observations))

# plot total observations per domain
tot_obs <- ggplot(domain_summary, aes(x = reorder(domain, -total_observations), y = total_observations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Total Observations per Domain", x = "Domain", y = "Total Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here("figures/initial_exploration/total_observations.png"), plot = tot_obs, width = 6, height = 5, dpi = 300)


# plot data collection duration per domain
duration <- ggplot(domain_summary, aes(x = reorder(domain, -collection_duration), y = collection_duration)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Data Collection Duration per Domain (in days)", x = "Domain", y = "Duration (Days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here("figures/initial_exploration/domain_duration.png"), plot = duration, width = 6, height = 5, dpi = 300)


# display summary table for initial exploration
print(domain_summary)
# print table using datatable
datatable(domain_summary, 
          caption = 'Domain Summary Table',
          options = list(pageLength = 5, autoWidth = TRUE))
# Save the summary table as a CSV file
write.csv(domain_summary, file = here("figures","initial_exploration", "domain_summary.csv"), row.names = FALSE)


##################


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
head(doc_chem)
unique(doc_chem$ms_interp)
# Filter to keep only rows where ms_interp is equal to 0
filtered_doc_chem <- doc_chem %>%
  filter(ms_interp == 0)

# Merge domain from ms_doc into filtered_doc_chem using site_code
filtered_doc_chem_with_domain <- filtered_doc_chem %>%
  left_join(ms_doc %>% select(site_code, domain), by = "site_code")
unique(filtered_doc_chem_with_domain$domain)


###########  plot DOC values (mg/L) by domain from most observations to least

# calculate the number of observations per domain and reorder
domain_order <- filtered_doc_chem_with_domain %>%
  group_by(domain) %>%
  summarize(
    count = n(),
    median_val = median(val, na.rm = TRUE)  # Use median to place labels near the boxplot
  ) %>%
  arrange(desc(count))

# reorder domain factor levels by observation count
filtered_doc_chem_with_domain <- filtered_doc_chem_with_domain %>%
  mutate(domain = factor(domain, levels = domain_order$domain))

all_plot <- ggplot(filtered_doc_chem_with_domain, aes(x = domain, y = val)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  theme_minimal() +
  labs(title = "Boxplot of DOC Values by Domain", 
       x = "Domain", 
       y = "DOC (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
all_plot_removed_outliers <- ggplot(filtered_doc_chem_with_domain, aes(x = domain, y = val)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  theme_minimal() +
  labs(title = "Boxplot of DOC Values by Domain", 
       x = "Domain", 
       y = "DOC (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 75))

all_plot_removed_outliers

# add # of observations
# split the domains into three groups of 7 domains each
# because 21 was too cluttered to see at once
domains_group1 <- domain_order$domain[1:7]
domains_group2 <- domain_order$domain[8:14]
domains_group3 <- domain_order$domain[15:21]

# Create a function to generate the plots with consistent y-axis limits
create_boxplot <- function(domains_subset) {
  ggplot(filtered_doc_chem_with_domain %>% filter(domain %in% domains_subset), aes(x = domain, y = val)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red") +
    theme_minimal() +
    labs(title = "Boxplot of DOC Values by Domain", x = "Domain", y = "DOC (mg/L)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(data = domain_order %>% filter(domain %in% domains_subset), 
              aes(x = domain, y = median_val, label = paste("n =", count)), 
              inherit.aes = FALSE, vjust = -0.5, size = 2, color = "black") +
    scale_y_continuous(limits = c(0, 75))  # Ensure the same y-axis scale for all plots
}

# Create plots
plot_group1 <- create_boxplot(domains_group1)
plot_group2 <- create_boxplot(domains_group2)
plot_group3 <- create_boxplot(domains_group3)

combined_plot <- plot_group1 / plot_group2 / plot_group3
combined_plot
ggsave(here("figures/initial_exploration/doc_val_bydomain.png"), plot = combined_plot, width = 6, height = 10, dpi = 300)
ggsave(here("figures/initial_exploration/doc_val_1.png"), plot = plot_group1, width = 6, height = 4, dpi = 300)
ggsave(here("figures/initial_exploration/doc_val_2.png"), plot = plot_group2, width = 6, height = 4, dpi = 300)
ggsave(here("figures/initial_exploration/doc_val_3.png"), plot = plot_group3, width = 6, height = 4, dpi = 300)
ggsave(here("figures/initial_exploration/all_plot.png"), plot = all_plot, width = 6, height = 6, dpi = 300)
ggsave(here("figures/initial_exploration/all_plot_removed_outliers.png"), plot = all_plot_removed_outliers, width = 6, height = 6, dpi = 300)


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



