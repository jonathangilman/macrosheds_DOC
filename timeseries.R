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
unique(doc_chem$site_code)


######### make big timeseries plot

# Calculate the start and end dates, and duration for each site
site_data <- doc_chem %>%
  group_by(site_code) %>%
  summarize(start_date = min(date), 
            end_date = max(date),
            duration = as.numeric(difftime(end_date, start_date, units = "days"))) %>%
  arrange(desc(duration))  # Sort by duration, longest on top

# Reorder the site_code factor based on the sorted duration
site_data <- site_data %>%
  mutate(site_code = factor(site_code, levels = rev(site_code)))  # Reverse the order

# Create the plot using ggplot
timeseries <- ggplot(site_data, aes(x = start_date, xend = end_date, y = site_code, yend = site_code)) +
  geom_segment(linewidth = 0.25) +
  labs(title = "MS Site Data: DOC", x = "Year", y = "Site Code") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 5))

ggsave(here("figures/temporal_analyses/timeseries.png"), plot = timeseries, width = 8, height = 10, dpi = 300)


##### plot with sites that have >10 yrs of data
# removed 3 sites

# Filter for sites with a duration of 10 years or more
filtered_site_data <- site_data %>%
  filter(duration >= 10) %>%
  arrange(desc(duration))  # Sort by duration, longest first

# Reorder the site_code factor, but reverse the levels to get most data at the top
filtered_site_data <- filtered_site_data %>%
  mutate(site_code = factor(site_code, levels = rev(site_code)))  # Reverse the order

# Create the plot using ggplot
timeseries_10 <- ggplot(filtered_site_data, aes(x = start_date, xend = end_date, y = site_code, yend = site_code)) +
  geom_segment(size = 0.25) +  # Adjust the size of the lines (thickness)
  labs(title = "MS Site Data: DOC (10+ Years)", x = "Year", y = "Site Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5))  # Adjust the y-axis label font size

ggsave(here("figures/temporal_analyses/timeseries_10.png"), plot = timeseries_10, width = 8, height = 10, dpi = 300)







