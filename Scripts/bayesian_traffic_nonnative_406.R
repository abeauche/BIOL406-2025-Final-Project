# ====================================================
# Script Name: bayesian_traffic_nonnative_406.R
# Project: BIOL406-2025-Final_Project_github
# Author: Alex Beauchemin
# Date Created: 2025-04-15
# Last Modified: 2025-04-22
# Description: This script runs Bayesian generalized linear mixed-effects models (using the brms package) to evaluate the relationship between (1) non-native species richness and (2) non-native percent cover and distance from trailhead.
# Dependencies: R packages: brms, lmerTest, tidyverse, car, ggeffects, visreg, DHARMa, performance, see.
# ====================================================

# Load required libraries
library(brms)
library(lmerTest)
library(tidyverse)
library(car)
library(ggeffects)
library(visreg)
library(DHARMa)
library(performance)
library(see)

df <- read.csv("./data_cleaned/cleaned_dataframe.csv",stringsAsFactors = TRUE)
species_dispersal <- read.csv("./data_raw/species_dispersal.csv",stringsAsFactors = TRUE)

# Check for distribution
hist((df$richness_non_native))

# Check for overdispersion
m1 <- glm(richness_non_native ~ distance_m + Traffic, family = poisson, data = df)
dispersion <- sum(residuals(m1, type = "pearson")^2) / df.residual(m1)
dispersion

# Bayesian Poisson regression
non_native_rich_poisson_m1 <- brm(
  richness_non_native ~ distance_m + Traffic,
  family = poisson(),
  data = df,
  prior = c(
    prior(normal(0, 1), class = "b"),          # Priors for fixed effects
    prior(normal(0, 1), class = "Intercept")   # Prior for the intercept
  ),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

summary(non_native_rich_poisson_m1)


### NOT ENOUGH DATA --> too many divergent transitions, can't do random slopes here unfortunately
non_native_rich_poisson_m2 <- brm(
  richness_non_native ~ distance_m + (1 + distance_m | Traffic),
  family = poisson(),
  data = df,
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(exponential(1), class = "sd"),
    prior(lkj(2), class = "cor")
  ),
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.999)
)

summary(non_native_rich_poisson_m2)



### Summarize non-native percent cover per plot
df_pct_cover <- df %>%
  rename(canopy_pct = canopy_cover)

# Identify columns that contain species cover values but exclude "canopy_cover"
species_cols <- setdiff(grep("_cover$", names(df_pct_cover), value = TRUE), "canopy_cover")

# Sum percent cover across all species columns
df_pct_cover$total_percent_cover_non_native <- rowSums(df_pct_cover[, species_cols], na.rm = TRUE)

# Check distribution
hist((df_pct_cover$total_percent_cover_non_native))

# Transform percent cover from 0–100 to 0–1 (if not already)
df_pct_cover$pct_cover_prop <- df_pct_cover$total_percent_cover_non_native / 100

# Check distribution
hist((df_pct_cover$pct_cover_prop))

# Fit zero-inflated beta model
zib_model <- brm(
  bf(pct_cover_prop ~ distance_m + Traffic,
     zi ~ distance_m + Traffic),  # zero-inflation component
  family = zero_inflated_beta(),
  data = df_pct_cover,
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 123
)
summary(zib_model)



# Plot richness distance relationship
pred_mu <- ggpredict(non_native_rich_poisson_m1, terms = c("distance_m", "Traffic"))
plot(pred_mu) + ggtitle("Predicted Percent Cover (non-zero part)") + theme_classic() 

# Convert predictions to data.frame for ggplot
pred_mu_df <- as.data.frame(pred_mu)

# Plot with ggplot
brms_distance_richness <- ggplot(pred_mu_df, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_point(data = df_pct_cover, aes(x = distance_m, y = richness_non_native, colour = Traffic)) +
  geom_ribbon(data = pred_mu_df, aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Ribbon for CI
  labs(x = "Distance (m)",
       y = "Non-Native Species Richness") +
  theme_classic() +
  labs(color="Traffic",fill="Traffic") +
  scale_fill_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092")) + scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092"))

ggsave("./figures/brms_richness_distance.png",brms_distance_richness)




