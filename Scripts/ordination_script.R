# ====================================================
# Script Name: bayesian_traffic_nonnative_406.R
# Project: BIOL406-2025-Final_Project_github
# Author: Alex Beauchemin
# Date Created: 2025-03-18
# Last Modified: 2025-04-22
# Description: This script runs an NMDS ordination for non-native species assemblage at sites along 250m-long transects along a high-use and a low-use trail in Pacific Spirit Park, BC.
# Dependencies: cleaned_dataframe.csv, R packages: tidyr, dplyr, vegan, ggplot2, lubridate.
# ====================================================


library(tidyr)
library(dplyr)
library(vegan)
library(ggplot2)
library(lubridate)



# Read csv
non_native_data <- read.csv("./data_cleaned/cleaned_dataframe.csv")


### NMDS ORDINATION

# Convert data into percent cover
non_native_data_pct <- non_native_data %>%
  select(!c(Traffic,distance_m,angle_.º.,dominant_tree_species,canopy_cover))

# Convert to matrix (excluding the first column which is 'plot')
non_native_data_matrix <- as.matrix(non_native_data_pct[, -1])
rownames(non_native_data_matrix) <- non_native_data_pct$QuadratID  # Set plot names as rownames

# Run NMDS ordination
set.seed(123)  # For reproducibility
nmds_result <- metaMDS(non_native_data_matrix, distance = "bray", k = 2, trymax = 100)
nmds_result$stress

# Basic NMDS plot
plot(nmds_result, type = "t")  # 't' = text labels

# Add plot points
ordiplot(nmds_result, display = "sites", type = "n")
points(nmds_result$points, col = "blue", pch = 19)
text(nmds_result$points, labels = rownames(nmds_result$points), pos = 3, cex = 0.8)




### LINK SITES ALONG EACH TRAIL

# Extract site scores
site_scores <- as.data.frame(nmds_result$points)
site_scores$Plot <- rownames(site_scores)

# Assign trails based on plot names
site_scores$Trail <- ifelse(grepl("^L", site_scores$Plot), "Low-Use", "High-Use")

# Extract numeric part for correct ordering
site_scores$PlotNum <- as.numeric(sub("[LH]", "", site_scores$Plot))

# Order correctly by trail and numeric plot order
site_scores <- site_scores[order(site_scores$Trail, site_scores$PlotNum), ]

# Define colors for trails
trail_colors <- c("Low-Use" = "#4B0092", "High-Use" = "#B4DD1E")

# Basic NMDS plot setup
ordiplot(nmds_result, display = "sites", type = "n")

# Plot points with colors
points(site_scores[, 1:2], col = trail_colors[site_scores$Trail], pch = 19)

# Add text labels
text(site_scores[, 1:2], labels = site_scores$Plot, pos = 3, cex = 0.8)

# Connect plots sequentially along each trail
for (trail in unique(site_scores$Trail)) {
  trail_points <- site_scores[site_scores$Trail == trail, ]
  lines(trail_points[, 1:2], col = trail_colors[trail], lwd = 2)
}

# Add legend
legend("topright", legend = names(trail_colors), col = trail_colors, pch = 19, bty = "n")

png("./Figures/nmds_plot.png", width = 800, height = 600)

# NMDS Plot
ordiplot(nmds_result, display = "sites", type = "n")

# Plot points with colors
points(site_scores[, 1:2], col = trail_colors[site_scores$Trail], pch = 19)

# Add text labels
text(site_scores[, 1:2], labels = site_scores$Plot, pos = 3, cex = 0.8)

# Connect plots sequentially along each trail
for (trail in unique(site_scores$Trail)) {
  trail_points <- site_scores[site_scores$Trail == trail, ]
  lines(trail_points[, 1:2], col = trail_colors[trail], lwd = 2)
}

# Add legend
legend("topright", legend = names(trail_colors), col = trail_colors, pch = 19, bty = "n")

dev.off()  # Close and save the file
