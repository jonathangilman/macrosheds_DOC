# Analyze MacroSheds data
# Load libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(here)
library(grid)
library(maps)

# Load processed data
processed_data <- readRDS(here("data/Processed/processed_macrosheds_data.rds"))

# Calculate daily VWM and clean data
vwm_seasonal <- processed_data %>%
    group_by(domain, date) %>%
    summarize(
        VWM = sum(val.x * val.y, na.rm = TRUE) / sum(val.y, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        Month_Num = month(date),
        season = case_when(
            Month_Num %in% c(9, 10, 11) ~ "Fall",
            Month_Num %in% c(12, 1, 2) ~ "Winter",
            Month_Num %in% c(3, 4, 5) ~ "Spring",
            Month_Num %in% c(6, 7, 8) ~ "Summer",
            TRUE ~ NA_character_
        ),
        season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer"))
    ) %>%
    filter(is.finite(VWM))  # Remove rows with non-finite VWM

# Add placeholder rows for missing levels
placeholder_data <- expand.grid(
    domain = unique(vwm_seasonal$domain),
    season = factor(c("Fall", "Winter", "Spring", "Summer"), levels = c("Fall", "Winter", "Spring", "Summer"))
) %>%
    mutate(
        date = as.Date(NA),  # Placeholder date
        VWM = NA             # Placeholder VWM
    )

vwm_seasonal <- bind_rows(vwm_seasonal, placeholder_data)

# Define custom offsets for each domain
offsets <- list(
    "loch_vale" = c(-8, 12), "niwot" = c(-25, 0), "boulder" = c(-12, -13),
    "east_river" = c(-20, -7), "bear" = c(-3, 7), "hbef" = c(13, 0),
    "hjandrews" = c(-10, 5), "konza" = c(0, -15), "neon" = c(-6, -13),
    "walker_branch" = c(17, -10), "panola" = c(-3, -10), "plum" = c(13, -7),
    "santee" = c(3, -10), "shale_hills" = c(10, -6), "sleepers" = c(-10, 5),
    "trout_lake" = c(-8, 7)
)

# Define boxplot function
create_boxplot_domain <- function(domain_name, data) {
    plot_data <- data %>%
        filter(domain == domain_name) %>%
        complete(season = factor(season, levels = c("Fall", "Winter", "Spring", "Summer")), fill = list(VWM = NA))

    ggplot(plot_data, aes(x = season, y = VWM, fill = season)) +
        geom_boxplot(
            width = 0.8,  # Make boxplots narrower
            outlier.size = 0.2,
            na.rm = TRUE
        ) +
        scale_fill_manual(
            values = c(
                "Fall" = "orange",
                "Winter" = "lightblue",
                "Spring" = "lightgreen",
                "Summer" = "gold"
            )
        ) +
        coord_cartesian(ylim = c(0, quantile(plot_data$VWM, 0.95, na.rm = TRUE))) +
        theme_minimal() +
        theme(
            axis.title.x = element_blank(),  # Remove x-axis title
            axis.text.x = element_blank(),   # Remove x-axis labels
            axis.ticks.x = element_blank(),  # Remove x-axis ticks
            legend.position = "none",
            panel.grid = element_blank(),    # Remove gridlines behind boxplots
            plot.title = element_text(size = 6),  # Smaller title
            axis.title.y = element_text(size = 6),  # Smaller y-axis title
            axis.text.y = element_text(size = 5)  # Smaller y-axis text
        ) +
        labs(title = domain_name, y = "VWM")
}

# Create base map
us_map_plot <- ggplot() +
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    coord_cartesian(xlim = c(-135, -50), ylim = c(18, 58)) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Function to scale down the size of each boxplot
add_boxplot_to_map_domain <- function(base_map, domain_name, data, lat_long_data, x_offset = 0, y_offset = 0) {
    boxplot_grob <- ggplotGrob(create_boxplot_domain(domain_name, data))
    if (is.null(boxplot_grob)) return(base_map)

    domain_coords <- lat_long_data %>% filter(domain == domain_name)
    long <- domain_coords$longitude + x_offset
    lat <- domain_coords$latitude + y_offset

    base_map +
        geom_point(aes(x = domain_coords$longitude, y = domain_coords$latitude), color = "black", fill = "red", shape = 21, size = 3) +
        geom_segment(aes(x = domain_coords$longitude, y = domain_coords$latitude, xend = long, yend = lat), color = "black", linetype = "dashed") +
        annotation_custom(
            grob = boxplot_grob,
            xmin = long - 5, xmax = long + 5.5,  # Scale down grob size
            ymin = lat - 5, ymax = lat + 5
        )
}

# Create a dataframe with latitude and longitude for each domain
site_lat_long <- processed_data %>%
    group_by(domain) %>%
    summarize(
        latitude = mean(latitude, na.rm = TRUE),
        longitude = mean(longitude, na.rm = TRUE),
        .groups = "drop"
    )

# Add boxplots to the map with offsets
final_map <- us_map_plot
for (domain in unique(vwm_seasonal$domain)) {
    offset <- offsets[[domain]] %||% c(0, 0)  # Use custom offsets; default to (0, 0) if not specified
    final_map <- add_boxplot_to_map_domain(final_map, domain, vwm_seasonal, site_lat_long, x_offset = offset[1], y_offset = offset[2])
}

# Save the map
ggsave(here("output/analysis_figs/us_map_plot2.png"), plot = final_map, width = 12, height = 7, dpi = 300)


# Define a simple legend with adjusted title position and outlined points
legend_plot <- ggplot() +
    geom_point(aes(x = 1, y = 4), color = "black", size = 5, shape = 21, stroke = 0.8, fill = "orange") +
    geom_text(aes(x = 1.25, y = 4, label = "Fall"), hjust = 0, size = 5) +
    geom_point(aes(x = 1, y = 3.5), color = "black", size = 5, shape = 21, stroke = 0.8, fill = "lightblue") +
    geom_text(aes(x = 1.25, y = 3.5, label = "Winter"), hjust = 0, size = 5) +
    geom_point(aes(x = 1, y = 3), color = "black", size = 5, shape = 21, stroke = 0.8, fill = "lightgreen") +
    geom_text(aes(x = 1.25, y = 3, label = "Spring"), hjust = 0, size = 5) +
    geom_point(aes(x = 1, y = 2.5), color = "black", size = 5, shape = 21, stroke = 0.8, fill = "gold") +
    geom_text(aes(x = 1.25, y = 2.5, label = "Summer"), hjust = 0, size = 5) +
    xlim(0.5, 3) + ylim(0.5, 4.5) +
    theme_void() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)), # Lower title
        plot.margin = margin(10, 10, 10, 10) # Add space around
    ) +
    ggtitle("Season")

# Save the updated legend
ggsave(here("output/analysis_figs/season_legend.png"), legend_plot, width = 4, height = 6, dpi = 300)


# Load the legend
season_legend <- png::readPNG(here("output/analysis_figs/season_legend.png"))
legend_grob <- grid::rasterGrob(season_legend, interpolate = TRUE)

# Combine map and legend
combined_plot <- ggdraw() +
    draw_plot(final_map) +
    draw_grob(legend_grob, x = 0.85, y = -0.1, width = 0.2, height = 0.4) # Adjust position and size as needed

# Save the combined plot
ggsave(here("output/analysis_figs/us_map_withlegend.png"), combined_plot, width = 12, height = 8, dpi = 300)



##############################################################################################
##############################################################################################

# Create a single plot of boxplots faceted by domain
boxplot_faceted <- vwm_seasonal %>%
    filter(!is.na(VWM)) %>%  # Remove rows with NA values for VWM
    ggplot(aes(x = season, y = VWM, fill = season)) +
    geom_boxplot(
        width = 0.8,  # Adjust boxplot width
        outlier.size = 0.2,
        na.rm = TRUE
    ) +
    scale_fill_manual(
        values = c(
            "Fall" = "orange",
            "Winter" = "lightblue",
            "Spring" = "lightgreen",
            "Summer" = "gold"
        )
    ) +
    facet_wrap(~ domain, scales = "free_y") +  # Create one panel per domain
    theme_minimal() +
    theme(
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        legend.position = "bottom",      # Place the legend below
        panel.grid = element_blank(),    # Remove gridlines
        plot.title = element_text(size = 12, face = "bold"),  # Adjust title size
        axis.title.y = element_text(size = 10),  # Adjust y-axis title size
        axis.text.y = element_text(size = 8)     # Adjust y-axis text size
    ) +
    labs(
        title = "Seasonal VWM DOC by Domain",
        y = "VWM DOC (mg/L)",
        fill = "Season"
    )

# Save the faceted plot
ggsave(here("output/analysis_figs/boxplots_faceted.png"), boxplot_faceted, width = 6, height = 8, dpi = 300)

##############################################################################################

library(dplyr)
library(ggplot2)
library(FSA)  # For Dunn's test
library(rstatix)  # For posthoc tests and compact letter display
library(multcompView)

# Step 1: Remove outliers from the dataset
filtered_vwm_seasonal <- vwm_seasonal %>%
    group_by(domain, season) %>%
    filter(VWM <= quantile(VWM, 0.95, na.rm = TRUE))  # Retain only values below the 95th percentile

# Step 2: Perform pairwise comparisons and create compact letter display
significance_results <- filtered_vwm_seasonal %>%
    filter(!is.na(VWM)) %>%
    group_by(domain) %>%
    rstatix::dunn_test(VWM ~ season, p.adjust.method = "bonferroni") %>%
    mutate(
        comparisons = paste(group1, group2, sep = "-"),  # Use hyphen as separator
        significant = ifelse(p.adj < 0.05, "*", "ns")
    )

compact_letters <- significance_results %>%
    group_by(domain) %>%
    reframe(
        season = unique(c(group1, group2)),  # Extract all unique seasons
        .group = multcompView::multcompLetters(
            setNames(p.adj, comparisons),  # Provide names for p-values
            Letters = letters
        )$Letters
    )

# Step 1: Calculate the letter positions directly above the bars
letter_data <- compact_letters %>%
    left_join(filtered_vwm_seasonal %>%
                  group_by(domain, season) %>%
                  summarize(max_season_y = max(VWM, na.rm = TRUE), .groups = "drop"),
              by = c("domain", "season")
    ) %>%
    mutate(
        y_position = max_season_y + 0.05 * max_season_y  # Add 5% above the bar
    )

# Step 2: Use dynamic y-axis limits without excessive padding
expanded_y_limits <- filtered_vwm_seasonal %>%
    group_by(domain) %>%
    summarize(
        y_limit = max(VWM, na.rm = TRUE) + 0.3 * max(VWM, na.rm = TRUE)  # Add proportional padding
    )

# Step 3: Create the boxplot with proper letter placement
boxplot_adjusted <- ggplot(filtered_vwm_seasonal, aes(x = season, y = VWM, fill = season)) +
    geom_boxplot(
        width = 0.8,
        outlier.size = 0.5,
        na.rm = TRUE
    ) +
    scale_fill_manual(
        values = c(
            "Fall" = "orange",
            "Winter" = "lightblue",
            "Spring" = "lightgreen",
            "Summer" = "gold"
        )
    ) +
    facet_wrap(~ domain, scales = "free_y") +
    geom_text(
        data = letter_data,
        aes(x = season, y = y_position, label = .group),
        inherit.aes = FALSE,
        size = 2,
        vjust = 0  # Place letters just above the bars
    ) +
    theme_minimal() +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8)
    ) +
    labs(
        title = "Seasonal VWM Boxplots by Domain with Statistical Comparisons",
        y = "VWM DOC (mg/L)",
        fill = "Season"
    ) +
    expand_limits(y = 0)  # Ensure y-axis starts at 0

# Save the plot
ggsave(here("output/analysis_figs/boxplot_with_letters.png"), boxplot_adjusted, width = 7, height = 8, dpi = 300)

