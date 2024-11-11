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
library(tidyverse)

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

# retrieve pH data
ph_chem <- ms_load_product(
  my_ms_dir,
  prodname = 'stream_chemistry',
  filter_vars = 'pH'
)

# add domain to ph_chem
ph_chem <- ph_chem %>%
  left_join(site_data, by = "site_code")

# Rename 'val' to 'ph_val' in ph_chem
ph_chem <- ph_chem %>%
  rename(ph_val = val)
# Rename 'val' to 'doc_val' in conus_doc_chem
conus_doc_chem <- conus_doc_chem %>%
  rename(doc_val = val)

#### join pH data the conus_doc_chem
# Keeping all historical pH data but only include domains included in conus_doc_chem

# Perform a full join to combine both doc_val and ph_val, keeping all data
combined_data <- full_join(conus_doc_chem, ph_chem, by = c("date", "domain"))

# Get the list of the 16 unique domains in conus_doc_chem
relevant_domains <- unique(conus_doc_chem$domain)

# Filter combined_data to include only rows from those domains
filtered_combined_data <- combined_data %>%
  filter(domain %in% relevant_domains)

# Perform a full join to retain all rows, allowing NAs for missing doc_val or ph_val
final_combined_data <- full_join(
  conus_doc_chem, 
  ph_chem %>% select(date, domain, ph_val), 
  by = c("date", "domain")
)
# Filter the final combined data to only include the relevant 16 domains
final_combined_data <- final_combined_data %>%
  filter(domain %in% relevant_domains)
unique(final_combined_data$domain)

#########################################################################

# plot DOC and pH for each domain over time

# Step 1: Group by domain and date, and calculate the average values for doc_val and ph_val
avg_data <- final_combined_data %>%
  group_by(domain, date) %>%
  summarize(
    avg_doc = mean(doc_val, na.rm = TRUE),
    avg_ph = mean(ph_val, na.rm = TRUE)
  )

# Step 2: Plot DOC and pH on separate y-axes without scaling
doc_with_ph <- ggplot(avg_data, aes(x = date)) +
  geom_line(aes(y = avg_doc, color = "DOC"), linewidth = 1) +    # Line for DOC (left y-axis)
  geom_line(aes(y = avg_ph, color = "pH"), linewidth = 1) +      # Line for pH (right y-axis)
  facet_wrap(~ domain, scales = "free_y") +                      # Facet by domain
  labs(x = "Date", y = "DOC (mg/L)", title = "DOC and pH over Time by Domain") +
  theme_minimal() +
  scale_color_manual(name = "Variable", values = c("DOC" = "blue", "pH" = "red")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(                                     # Primary y-axis for DOC
    sec.axis = sec_axis(~ ., name = "pH")                 # Secondary y-axis for pH with no scaling
  )
doc_with_ph

######################################################

# water year pH and DOC across time
# order of facet is by # of DOC measurements
# site numbers in domain listed on each plot

# Create a vectorized function to calculate the water year
water_year <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  
  # Use if_else to handle vectorized conditions
  dplyr::if_else(month >= 10, year, year - 1)
}

# Step 1: Add the water year to the dataset
final_combined_data <- final_combined_data %>%
  mutate(water_year = water_year(date))  # Apply the water year function

# Step 2: Count the number of DOC measurements and the number of sites per domain
domain_stats <- final_combined_data %>%
  group_by(domain) %>%
  summarize(
    doc_count = sum(!is.na(doc_val)),  # Count of DOC measurements
    site_count = n_distinct(site_code)  # Number of unique sites
  )

# Step 3: Add domain statistics to the dataset before averaging
final_combined_data <- final_combined_data %>%
  left_join(domain_stats, by = "domain")  # Join the domain statistics

# Step 4: Reorder the domains based on the number of DOC measurements
final_combined_data <- final_combined_data %>%
  mutate(domain = forcats::fct_reorder(domain, doc_count, .desc = TRUE))  # Reorder by doc_count

# Step 5: Group by domain and water_year, and calculate the average values for doc_val and ph_val
avg_data_water_year <- final_combined_data %>%
  group_by(domain, water_year) %>%
  summarize(
    avg_doc = mean(doc_val, na.rm = TRUE),
    avg_ph = mean(ph_val, na.rm = TRUE),
    site_count = first(site_count)  # Retain the number of sites in each domain
  )

# Step 6: Plot DOC and pH on separate y-axes, with "sites=__" in each facet
doc_with_ph_water_year <- ggplot(avg_data_water_year, aes(x = water_year)) +
  geom_line(aes(y = avg_doc, color = "DOC"), linewidth = 0.75) +    # Line for DOC (left y-axis)
  geom_line(aes(y = avg_ph, color = "pH"), linewidth = 0.75) +      # Line for pH (right y-axis)
  facet_wrap(~ domain, scales = "free_y") +                      # Facet by domain
  labs(x = "Water Year", y = "DOC (mg/L)", title = "DOC and pH by Water Year") +
  theme_minimal() +
  scale_color_manual(name = "Variable", values = c("DOC" = "black", "pH" = "red")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(                                     # Primary y-axis for DOC
    sec.axis = sec_axis(~ ., name = "pH")                 # Secondary y-axis for pH with no scaling
  ) +
  theme(panel.grid = element_blank(),  # Remove gridlines 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)  # Add a black border around each facet
  ) +
  # Step 7: Add "sites=__" in the top left of each facet
  geom_text(aes(x = -Inf, y = Inf, label = paste("sites =", site_count)), 
            hjust = -0.1, vjust = 1.5, size = 2.3, color = "black")  # Position text in top left

# Display the plot
doc_with_ph_water_year
# Save the plot using the here package
ggsave(here("figures/temporal_analyses/doc_with_pH_water_year.png"), plot = doc_with_ph_water_year, width = 8, height = 6, dpi = 300)



