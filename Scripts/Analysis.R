# ====================================================
# Script Name: Analysis.R
# Project: BIOL406-2025-Final_Project_github
# Author: Lucas Braun, Alex Beauchemin
# Date Created: 2025-03-11
# Last Modified: 2025-04-22
# Description: This script runs generalized linear models (GLMs) to test predictions and relationships from our BIOL406 data.
# Dependencies: R packages: brms, lmerTest, tidyverse, car, ggeffects, visreg, DHARMa, performance, see.
# ====================================================


# ===Analysis

#Import

df <- read.csv("./data_cleaned/cleaned_dataframe.csv",stringsAsFactors = TRUE)
species_dispersal <- read.csv("./data_raw/species_dispersal.csv",stringsAsFactors = TRUE)

library(lmerTest)
library(tidyverse)
library(car)
library(ggeffects)
library(visreg)
# install.packages("DHARMa")
library(DHARMa)
# install.packages("performance")
library(performance)
# install.packages("see")
library(see)

# First check histograms of each variable to view distribution

hist(df$lesser_periwinkle_cover)
hist(df$rumex_cover)
hist(df$creeping_buttercup)
hist(df$spanish_bluebell)
hist(df$grass)
hist(df$english_ivy_cover)
hist(df$himalayan_blackberry_cover)
hist(df$english_laurel_cover)
hist(df$nipplewort_cover)
hist(df$english_holly_cover)
hist(df$cutleaf_blackberry_cover)
hist(df$other_cover)
hist(df$richness_non_native)


# Scale variables (mutate(variable_scaled = scale(variable)))

#Check for correlations cor(dataframe) or cor(c("varable_1_scaled","variable_2_scaled",...))

#Make models and analysis of scaled variables (DO NOT SCALE RESPONSE)

m1 <- glm(richness_non_native ~ distance_m + Traffic, family = poisson, data = df)

m2 <- glm(richness_non_native ~ Traffic + distance_m, family = poisson, data = df)

summary(m1)
check_model(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot=F)
plot(simulationOutput)
visreg(m1, xvar = "x", ylim = range(y), rug = 2, scale = "response") 
Anova(m1)

summary(m2)
Anova(m2)      #no difference in the order of variables. m1=m2

#Analysis of Deviance Table (Type II tests)

# Response: richness_non_native
# LR Chisq Df Pr(>Chisq)    
# Traffic     15.4461  1  8.489e-05 ***
  # distance_m   4.1736  1    0.04106 *  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 

# Call:
# glm(formula = richness_non_native ~ distance_m + Traffic, family = poisson, 
    # data = df)

# Coefficients:
  # Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.014285   0.462387  -0.031 0.975354    
# distance_m  -0.004973   0.002481  -2.004 0.045034 *  
  # TrafficLow   1.540445   0.449864   3.424 0.000617 ***

# Thinking a t test between trails as well as an ANCOVA of response over distance for each trail.

# ===Some plots

# Can use gg_effects package to fit lines to predicted values which is better for glm type models. Talk to mia about that package.

predictions <- ggpredict(m1, terms = c("distance_m","Traffic"))

# Lucas: Keep geom_errorbar in mind for my thesis

# ggplot(predictions,aes(x=x,y=predicted,color=group,group=group)) + geom_point() + geom_errorbar(aes(ymin = conf.low,ymax = conf.high)) + geom_hline(yintercept = 1.0,linetype = "dashed", color = "dimgrey") + theme_classic() + labs(x = "Distance (m)",y = "Predicted non_native richness",color="Traffic")

# This is with ribbon. Need to make it prettier.

# Plot predictions of richness according to distance, and save
figure3 <- ggplot() + 
  geom_point(aes(x=distance_m,y=richness_non_native,color=Traffic,group=Traffic,fill = Traffic), data = df) +
  geom_line(aes(x=x, y=predicted, color=group), data = predictions) +
  geom_ribbon(data = predictions, aes(x=x, y=predicted,ymin = conf.low,ymax = conf.high,fill=group),alpha = 0.4) +
  #geom_smooth(method = "lm", aes(color=Traffic), se = TRUE) +
  #geom_hline(yintercept = 1.0,linetype = "dashed", color = "dimgrey") + 
  theme_classic() + 
  labs(x = "Distance (m)",y = "Non-native richness",color="Traffic",fill="Traffic") +
  scale_fill_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092")) + scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092"))
  #ylim(0,8)

print(figure3)

ggsave("./figures/Predicted.PNG",figure2)
ggsave("./figures/distancerichness_brat.PNG",figure3)



### Summarize non-native percent cover per plot
df_pct_cover <- df %>%
  rename(canopy_pct = canopy_cover)

# Identify columns that contain species cover values but exclude "canopy_cover"
species_cols <- setdiff(grep("_cover$", names(df_pct_cover), value = TRUE), "canopy_cover")

# Sum percent cover across all species columns
df_pct_cover$total_percent_cover_non_native <- rowSums(df_pct_cover[, species_cols], na.rm = TRUE)

# Model in GLM for easy visualization
m3 <- glm(total_percent_cover_non_native ~ distance_m + Traffic, family = poisson, data = df_pct_cover)
summary(m3)
predictions_3 <- ggpredict(m3, terms = c("distance_m","Traffic"))

figure4 <- ggplot() + 
  geom_point(aes(x=distance_m,y=total_percent_cover_non_native,color=Traffic,group=Traffic,fill = Traffic), data = df_pct_cover) +
  geom_line(aes(x=x, y=predicted, color=group), data = predictions_3) +
  geom_ribbon(data = predictions_3, aes(x=x, y=predicted,ymin = conf.low,ymax = conf.high,fill=group),alpha = 0.4) +
  #geom_smooth(method = "lm", aes(color=Traffic), se = TRUE) +
  #geom_hline(yintercept = 1.0,linetype = "dashed", color = "dimgrey") + 
  theme_classic() + 
  labs(x = "Distance (m)",y = "Non-native Percent Cover (%)",color="Traffic",fill="Traffic") +
  scale_fill_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092")) + scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092"))
#ylim(0,8)

print(figure4)

ggsave("./figures/distancepctcover_brat.PNG",figure4)



# Pivot_longer_df
df_long <- df %>%
  pivot_longer(
    cols = ends_with("_cover"),  # Selects all species cover columns
    names_to = "species",        # New column for species names
    values_to = "percent_cover"  # New column for percent cover values
  )
df_long <- df_long %>%
  filter(!species == "canopy_cover")

# Left join species_dispersal files
df_long <- df_long %>%
  left_join(species_dispersal, by = "species")

# Summarize dispersal method 
df_summary <- df_long %>%
  filter(percent_cover > 0) %>%  # Join dispersal syndromes
  group_by(QuadratID) %>%  # Group by QuadratID
  summarise(
    wind_dispersed = sum(wind, na.rm = TRUE),
    animal_dispersed = sum(animal, na.rm = TRUE),
    water_dispersed = sum(water, na.rm = TRUE),
    unspecified_dispersed = sum(unspecified, na.rm = TRUE)
  )

# Left join with relevant columns
df_summary <- df_summary %>%
  left_join(df %>% select(QuadratID, distance_m, Traffic, richness_non_native), by = "QuadratID")

# Pivot long for tidy format
df_long_summary <- df_summary %>%
  pivot_longer(
    cols = c(wind_dispersed, animal_dispersed, water_dispersed, unspecified_dispersed),
    names_to = "dispersal_type",
    values_to = "count"
  )

# Exploratory: plot by dispersal syndrome across distance
ggplot(df_long_summary, aes(x = distance_m, y = count, color = Traffic, group = Traffic)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Smoothed trend
  facet_wrap(~dispersal_type, scales = "free_y") +  # Separate plots for dispersal types
  theme_classic() +
  labs(x = "Distance (m)", y = "Count of species", color = "Traffic") +
  scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092"))

# Mutate proportion animal from animal dispersed/richness_non_native
df_summary <- df_summary %>%
  mutate(proportion_animal = animal_dispersed / richness_non_native)

# Filter to avoid redundant columns
df_summary_filtered <- df_summary %>%
  select(QuadratID, proportion_animal, animal_dispersed)

# Join with all rows from df
df_summary3 <- df %>%
  select(QuadratID, distance_m, Traffic, richness_non_native) %>%
  left_join(df_summary_filtered, by = "QuadratID")

# Replace NAs with 0s
df_summary3 <- df_summary3 %>%
  mutate(
    proportion_animal = replace_na(proportion_animal, 0),  # Replace NA with 0
    animal_dispersed = replace_na(animal_dispersed, 0)
  )

# Plot data
ggplot(df_summary3, aes(x = distance_m, y = proportion_animal, color = Traffic, group = Traffic)) +
  geom_point() +  # Scatter plot of points
  geom_smooth(method = "lm", se = TRUE) +  # Smoothed trend line
  theme_classic() +
  labs(x = "Distance (m)", y = "Proportion of Animal-Dispersed Species", color = "Traffic") +
  scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey")   # Adds y = 0 line +
  

# Visualize distribution of data
hist(df_summary3$proportion_animal)
hist(df_summary3$animal_dispersed)

# Fit binomial glm for number of animal dispersed according to total number of non-natives
m_binom <- glm(cbind(animal_dispersed, richness_non_native - animal_dispersed) ~ distance_m + Traffic,
               family = binomial, data = df_summary3)
summary(m_binom)

# Check for overdispersion
summary(m_binom)$dispersion

predictions_4 <- ggpredict(m_binom, terms = c("distance_m","Traffic"))

# Ensure `Traffic` is recognized correctly
predictions_4$Traffic <- predictions_4$group  # Rename `group` to `Traffic` if needed

# Plot
figure5 <- ggplot() + 
  geom_point(aes(x = distance_m, y = proportion_animal, color = Traffic, group = Traffic, fill = Traffic), data = df_summary3) +
  geom_line(aes(x = x, y = predicted, color = Traffic, group = Traffic), data = predictions_4) + 
  geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = Traffic), data = predictions_4, alpha = 0.4) +
  theme_classic() + 
  labs(x = "Distance (m)", y = "Proportion of Animal-Dispersed Non-native Species", color = "Traffic", fill = "Traffic") +
  scale_fill_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092")) + 
  scale_color_manual(values = c("High" = "#B4DD1E", "Low" = "#4B0092"))
print(figure5)

ggsave("./figures/distancepctcover_brat.PNG",figure4)



# ===Wilcoxan Paired Rank Sign test

high_traffic <- df %>%
  filter(Traffic=="High")

low_traffic <- df %>% 
  filter(Traffic=="Low")

result <- wilcox.test(high_traffic$richness_non_native,low_traffic$richness_non_native,paired = TRUE,alternative="less")

print(result) #p=0.004158, low traffic is dignificantly higher in non native species richness than high traffic

