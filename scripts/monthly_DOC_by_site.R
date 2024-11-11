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

#########################################################################

# I want to plot monthly DOC data for each site
# I will plot the sites of each domain for organization sake so not too cluttered

# Ensure the date column is in Date format
conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.Date(date)) 

# Extract the month from the date column and group data by domain, site_code, and month
conus_doc_chem2 <- conus_doc_chem %>%
  mutate(month = month(date, label = TRUE)) %>%  # Extract month as a factor with labels (Jan, Feb, ...)
  group_by(domain, site_code, month) %>%
  summarize(average_val = mean(val, na.rm = TRUE))  # Calculate the average DOC for each site and month

# Calculate the number of unique sites per domain
site_counts <- conus_doc_chem2 %>%
  group_by(domain) %>%
  summarize(n_sites = n_distinct(site_code))

# Create the plot
monthly_doc_by_site <- ggplot(conus_doc_chem2, aes(x = month, y = average_val, color = site_code, group = site_code)) +
  geom_line() +
  labs(title = "Average DOC Values by Site and Month", x = "Month", y = "Average DOC (val)") +
  facet_wrap(~ domain, scales = "free_y", ncol = 4) +  # Create 16 separate plots, one for each domain
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend if there are many lines
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data = site_counts, aes(x = -Inf, y = Inf, label = paste("n =", n_sites)), 
            hjust = -0.2, vjust = 1.2, inherit.aes = FALSE, size = 3)  # Add "n=_" label in the upper left corner

ggsave(here("figures/temporal_analyses/monthly.png"), plot = monthly_doc_by_site, width = 8, height = 6, dpi = 300)

# Create the plot
monthly_doc_by_site_bar <- ggplot(conus_doc_chem2, aes(x = month, y = average_val, fill = site_code)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use geom_bar for bar plots
  labs(title = "Average DOC Values by Site and Month", x = "Month", y = "Average DOC (val)") +
  facet_wrap(~ domain, scales = "free_y", ncol = 4) +  # Create 16 separate plots, one for each domain
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend if there are many lines
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = site_counts, aes(x = -Inf, y = Inf, label = paste("n =", n_sites)), 
            hjust = -0.2, vjust = 1.2, inherit.aes = FALSE, size = 3)  # Add "n=_" label in the upper left corner

monthly_doc_by_site_bar
ggsave(here("figures/temporal_analyses/monthly_bar.png"), plot = monthly_doc_by_site_bar, width = 8, height = 6, dpi = 300)


#########################################################################

# I want to plot monthly DOC data for each site
# Now I want to plot each site individually, facet sites of each domain together

# Step 1: Ensure the date column is in Date format
conus_doc_chem <- conus_doc_chem %>%
  mutate(date = as.Date(date))  # Convert 'date' column to Date format

# Step 2: Extract the month from the date column and group data by domain, site_code, and month
conus_doc_chem <- conus_doc_chem %>%
  mutate(month = month(date, label = TRUE)) %>%  # Extract month as a factor with labels (Jan, Feb, ...)
  group_by(domain, site_code, month) %>%
  summarize(average_val = mean(val, na.rm = TRUE))

# Step 3: Define a function to plot for a given domain and save it
plot_and_save_domain <- function(domain_name) {
  # Filter the data for the specific domain
  filtered_domain <- conus_doc_chem %>%
    filter(domain == domain_name)
  
  # Create the plot
  plot <- ggplot(filtered_domain, aes(x = month, y = average_val, group = site_code)) +  # Ensure grouping by site_code
    geom_line(aes(color = site_code), size = 1.2) +  # Line plot for each site
    geom_point(aes(color = site_code), size = 2) +  # Add points in case there's only one observation
    labs(title = paste("Average DOC Values by Site and Month for Domain", domain_name),
         x = "Month", y = "Average DOC (val)") +
    facet_wrap(~ site_code, scales = "free_y", ncol = 4) +  # Facet by site within the domain
    theme_minimal() +
    theme(legend.position = "none",  # Remove the legend
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8))
  
  # Save the plot using the here package
  ggsave(here("figures", "temporal_analyses", paste0("Average_DOC_", domain_name, ".png")),
         plot = plot, width = 8, height = 6, dpi = 300)
}

# Step 4: Get the unique domains
domains <- unique(conus_doc_chem$domain)

# Step 5: Loop through each domain, plot, and save
for (domain in domains) {
  plot_and_save_domain(domain)
}


