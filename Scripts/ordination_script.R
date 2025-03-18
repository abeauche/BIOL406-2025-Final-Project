library(tidyr)
library(dplyr)
library(vegan)
library(ggplot2)
library(lubridate)

# Read csv
non_native_data <- read.csv("./data_cleaned/cleaned_dataframe.csv")

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



