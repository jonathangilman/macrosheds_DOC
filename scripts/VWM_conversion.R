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