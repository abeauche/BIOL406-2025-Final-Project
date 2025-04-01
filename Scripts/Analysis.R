# ===Analysis

#Import

df <- read.csv("./data_cleaned/cleaned_dataframe.csv",stringsAsFactors = TRUE)

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


trail_colors <- c("High" == "brown4", "Low" == "darkolivegreen")

# This is with ribbon. Need to make it prettier.

figure2 <- ggplot(predictions,aes(x=x,y=predicted,color=group,group=group,fill = group)) + 
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin = conf.low,ymax = conf.high,fill=group),alpha = 0.5) + 
  geom_hline(yintercept = 1.0,linetype = "dashed", color = "dimgrey") + 
  theme_classic() + 
  labs(x = "Distance (m)",y = "Predicted non-native richness",color="Traffic",fill="Traffic") +
  scale_fill_manual(values = c("High" = "purple4", "Low" = "aquamarine")) + scale_color_manual(values = c("High" = "purple4", "Low" = "aquamarine"))

print(figure2)


figure3 <- ggplot() + 
  geom_point(aes(x=distance_m,y=richness_non_native,color=Traffic,group=Traffic,fill = Traffic), data = df) +
  geom_line(aes(x=x, y=predicted, color=group), data = predictions) +
  geom_ribbon(data = predictions, aes(x=x, y=predicted,ymin = conf.low,ymax = conf.high,fill=group),alpha = 0.4) +
  #geom_smooth(method = "lm", aes(color=Traffic), se = TRUE) +
  #geom_hline(yintercept = 1.0,linetype = "dashed", color = "dimgrey") + 
  theme_classic() + 
  labs(x = "Distance (m)",y = "Non-native richness",color="Traffic",fill="Traffic") +
  scale_fill_manual(values = c("High" = "purple4", "Low" = "blue")) + scale_color_manual(values = c("High" = "purple4", "Low" = "blue"))
  #ylim(0,8)

print(figure3)

ggsave("./figures/Predicted",figure2)

# ===Wilcoxan Paired Rank Sign test

high_traffic <- df %>%
  filter(Traffic=="High")

low_traffic <- df %>% 
  filter(Traffic=="Low")

result <- wilcox.test(high_traffic$richness_non_native,low_traffic$richness_non_native,paired = TRUE,alternative="less")

print(result) #p=0.004158, low traffic is dignificantly higher in non native species richness than high traffic

