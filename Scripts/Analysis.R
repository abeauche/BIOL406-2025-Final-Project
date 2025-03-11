# ===Analysis

#Import

df <- read.csv("./data_cleaned/cleaned_dataframe.csv",stringsAsFactors = TRUE)

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

# Thinking a t test between trails as well as an ANCOVA of response over distance for each trail.

