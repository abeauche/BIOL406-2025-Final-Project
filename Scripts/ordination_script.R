### BIOL406 - FINAL PROJECT ###
### Alex Beauchemin, Lucas Braun, and Denis Kasianiuk ###
### March 18, 2025 ###


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
trail_colors <- c("Low-Use" = "blue", "High-Use" = "red")

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

