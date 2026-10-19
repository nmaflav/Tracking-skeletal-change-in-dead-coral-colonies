












# Filter those cases where the volume or surface area computed don't make sense 
# (values that are too large, considering that we are working with individual
# colonies)
# there might be a error with the scaling of those models that worth checking,
# but for know I'll run the analysis and visualizations without them
# Here I inserted NA in those values that we need to check later, to see if 
# there is actually an issue with the scaling.

for (i in 1:length(data$`S22 (m2)`)) {
  if (data$`S22 (m2)`[i] > 10) {
    data$`S22 (m2)`[i] <- NA
  }
}

for (i in 1:length(data$`S23 (m2)`)) {
  if (data$`S23 (m2)`[i] > 8) {
    data$`S23 (m2)`[i] <- NA
  }
}

for (i in 1:length(data$`DS (m2)`)) {
  if (is.na(data$`S22 (m2)`[i]) | is.na(data$`S23 (m2)`[i])) {
    data$`DS (m2)`[i] <- NA
  }
}



### Calculate percentage change for volume and surface variables
#Volume
data$PCV <- abs(data$`DV (m3)`)/data$`V22 (m3)`*100

data <- data %>% relocate(PCV, .after = `DV (m3)`)

data$cat_V <- NA

for (i in 1:nrow(data)) {  
  if (data$`V22 (m3)`[i] > data$`V23 (m3)`[i]) {
    data$cat_V[i] <- "erosion"  
  } else {
    data$cat_V[i] <- "deposition"
  }
}

data <- data %>% relocate(cat_V, .after = PCV)

#Surface area
data$PCS <- abs(data$`DS (m2)`)/data$`S22 (m2)`*100

data$cat_S <- NA

for (i in 1:nrow(data)) {  
  if (!is.na(data$`S22 (m2)`[i]) & !is.na(data$`S23 (m2)`[i])) { 
    if (data$`S22 (m2)`[i] > data$`S23 (m2)`[i]) {
      data$cat_S[i] <- "area_reduction"
    } else {
      data$cat_S[i] <- "area_increase"
    }
  } else {
    data$cat_S[i] <- NA  
  }
}

# Barplot: volume and surface area increase/decrease per each morphology
#Volume

summary_data <- data %>% group_by(morphology, cat_V) %>% 
  summarise(meanPCV = mean(PCV), sePCV = sd(PCV)/sqrt(n())) %>% 
  ungroup() 

# Modify the data so that 'erosion' is negative
summary_data <- summary_data %>%
  mutate(meanPCV = ifelse(cat_V == "erosion", -meanPCV, meanPCV))

# Plot the barplot: Volume 
library(ggplot2)

# Create the plot with error bars
ggplot(summary_data, aes(x = morphology, y = meanPCV, fill = cat_V)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, width = 0.5) +  # Adjust width to make bars thinner
  geom_errorbar(aes(ymin = meanPCV - sePCV, ymax = meanPCV + sePCV), width = 0.2, position = position_dodge(0.5)) +  # Add error bars
  theme_minimal() +
  labs(x = "Morphology", y = "Volume Percentage Change") +
  scale_fill_manual(values = c("deposition" = "black", "erosion" = "white")) +  # White bars for erosion, black for deposition
  theme_classic() +
  theme(
      # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(),  # Remove grid lines for a cleaner look
    panel.grid.minor = element_blank(),
    legend.title = element_blank()  # Remove the legend title
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Dashed line at y = 0





#Surface area
summary_surface<-data %>% 
  filter(!is.na(cat_S)) %>%  # Filter out rows where cat_S is NA
  group_by(morphology, cat_S) %>% 
  summarise(
    meanPCS = mean(PCS, na.rm = TRUE), 
    sePCS = sd(PCS, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

# Modify the data so that 'erosion' is negative
summary_surface <- summary_surface %>%
  mutate(meanPCS = ifelse(cat_S== "area_reduction", -meanPCS, meanPCS))

ggplot(summary_surface, aes(x = morphology, y = meanPCS, fill = cat_S)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, width = 0.5) +  # Adjust width to make bars thinner
  geom_errorbar(aes(ymin = meanPCS - sePCS, ymax = meanPCS + sePCS), width = 0.2, position = position_dodge(0.5)) +  # Add error bars
  theme_minimal() +
  labs(x = "Morphology", y = "Surface Area Percentage Change") +
  scale_fill_manual(values = c("area_increase" = "black", "area_reduction" = "white")) +  # White bars for area_reduction, black for area_increase
  theme_classic() +
  theme(
      # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(),  # Remove grid lines for a cleaner look
    panel.grid.minor = element_blank(),
    legend.title = element_blank()  # Remove the legend title
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Dashed line at y = 0

#Analysis
#volume
#normality

# Visual check with density plots
ggplot(data, aes(x = PCV, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()

# Shapiro-Wilk test for normality (for each morphology group)
data %>%
  group_by(morphology) %>%
  summarise(shapiro_p = shapiro.test(PCV)$p.value)
# Not normal data

#homogeneity 
library(car)
leveneTest(PCV ~ morphology, data = data)

# Generalized Linear Model (GLM)
glm_model <- glm(PCV ~ morphology, data = data, family = gaussian())
summary(glm_model) #there is no morphology effect on PCV



#volume
#normality
# Visual check with density plots
ggplot(data, aes(x = PCA, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()

# Shapiro-Wilk test for normality (for each morphology group)
data %>%
  group_by(morphology) %>%
  summarise(shapiro_p = shapiro.test(PCS)$p.value)
# Not normal data

#homogeneity 
library(car)
leveneTest(PCS ~ morphology, data = data)

# Generalized Linear Model (GLM)
glm_model <- glm(PCS ~ morphology, data = data, family = gaussian())
summary(glm_model) #there is a small effect on PCS



################################################################################
# 1) Load ----
## data ----
setwd("C:/Users/nmafl/OneDrive - James Cook University/academia/aims/bioerosion")
data <- read_excel("datasets/data.xlsx")
View(data)

## packages ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(FSA)

# 2) Tidy ----
## variables as factors ----
data <- data %>%
  mutate(
    morphology = factor(morphology),
    reef       = factor(reef),
    site       = factor(site),
    exposure   = factor(exposure),
    depth      = factor(zone)
  )

## sub-massive: not enough observations ----
data <- subset(data, 
        !(morphology %in% c("Small massive/sub-massive")))

## cleaning ----
data <- data[-c(48 ,55, 56), ] # models with poor metashape reconstruction

# 3) exploration ----
## 3D deviations (avet) by taxa ----
ggplot(data, aes(x = morphology, y = avet, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "3D deviations (avet)") +
  theme_minimal() +
  theme(legend.position = "none")

## 3D deviations (avet) by taxa ----
ggplot(data, aes(x = morphology, y = rms, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "3D deviations (rms)") +
  theme_minimal() +
  theme(legend.position = "none")

## change in surface area by taxa ----
ggplot(data, aes(x = morphology, y = PCA, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "Change in Surface Area") +
  theme_minimal() +
  theme(legend.position = "none")

## change in volume by taxa ----
ggplot(data, aes(x = morphology, y = PCV, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "Change in Volume") +
  theme_minimal() +
  theme(legend.position = "none")

## density plot: surface area
ggplot(data, aes(x = PCA, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()+
  theme(legend.position = "none")

## density plot: volume
ggplot(data, aes(x = PCV, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()+
  theme(legend.position = "none")

# 4) Assumptions: ----
## normality ----
shapiro_test <- data %>%
  group_by(morphology) %>%
  summarise(p_value = shapiro.test(avet)$p.value)

print(shapiro_test)

## homogeneity of variance ----
leveneTest(avet ~ morphology, data = data)

# 5) Models ----
## 3D deviations (avet) ----
mod_3d_avet <- lmer(
  avet ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_3d_avet)
anova(mod_3d_avet)

## 3D deviations (rms) ----
mod_3d_rms <- lmer(
  rms ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_3d_rms)
anova(mod_3d_rms)

## Surface Area ----
mod_surf <- lmer(
  PCA ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_surf)
anova(mod_surf)

## Volume ----
mod_vol <- lmer(
  PCV ~ morphology + exposure + depth 
  + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_vol)
anova(mod_vol)

# 6) Residual diagnostics ----
## 3D deviations (avet) ----
par(mfrow = c(1,2))
plot(resid(mod_3d_avet))
qqnorm(resid((mod_3d_avet)))
qqline(resid((mod_3d_avet)))

## 3D deviations (rms) ----
par(mfrow = c(1,2))
plot(resid(mod_3d_rms))
qqnorm(resid((mod_3d_rms)))
qqline(resid((mod_3d_rms)))

## Surface area ----
par(mfrow = c(1,2))
plot(resid(mod_surf))
qqnorm(resid(mod_surf))
qqline(resid(mod_surf))

## Volume ----
par(mfrow = c(1,2))
plot(resid(mod_vol))
qqnorm(resid(mod_vol))
qqline(resid(mod_vol))

# 7) Visualization ----
## 3D deviations (avet) ----


## 3D deviations (rms) ----


## Surface Area ----


## Volume ----

