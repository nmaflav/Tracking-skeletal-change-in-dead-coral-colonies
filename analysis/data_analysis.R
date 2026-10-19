







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
data <- read_excel("C:/Users/nmafl/OneDrive - Australian Institute of Marine Science/Bioerosion 1 Methods/datasets/data.xlsx")
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
### calculate percentages ----
data$PCA_100 <- abs(data$PCA)*100
data <- data[data$PCA_100 <= 100, ]
data$cat_SA <- NA

### assign categories ----
for (i in 1:nrow(data)) {  
  if (!is.na(data$SA_2022[i]) & !is.na(data$SA_2023[i])) { 
    if (data$SA_2022[i] > data$SA_2023[i]) {
      data$cat_SA[i] <- "Area reduction"
    } else {
      data$cat_SA[i] <- "Area increase"
    }
  } else {
    data$cat_SA[i] <- NA  
  }
}

### summary ----
summary_surface <- data %>% 
  filter(!is.na(cat_SA)) %>% 
  group_by(morphology, cat_SA) %>% 
  summarise(
    meanPCA = mean(PCA_100, na.rm = TRUE), 
    sePCA = sd(PCA_100, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

summary_surface <- summary_surface %>%
  mutate(meanPCA = ifelse(cat_SA== "Area reduction", -meanPCA, meanPCA))

#### barplot ----
library(ggplot2)
library(showtext)
library(ggtext)
library(dplyr)

# 1. Setup font
#font_add_google("Lora", "lora")
#showtext_auto()
#showtext_opts(dpi = 300)

summary_surface <- summary_surface %>%
  mutate(morphology = gsub("Acropora", "<i>Acropora</i>", morphology))

summary_surface <- summary_surface %>%
  mutate(morphology = gsub("Acropora", "<i>Acropora</i>", morphology))

ggplot(summary_surface, aes(x = morphology, y = meanPCA, fill = cat_SA)) +
  geom_bar(stat = "identity", position = position_dodge(0.5), 
           color = "black", linewidth = 0.5, width = 0.5) +  
  geom_errorbar(aes(ymin = meanPCA - sePCA, ymax = meanPCA + sePCA), 
                width = 0.2, position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Area increase" = "black", 
                               "Area reduction" = "white")) +  
  theme_classic() +
  labs(
    x = "Morphology", 
    y = "<i>Acropora</i> Surface Area Percentage Change (%)"
  ) +
  theme(
    text = element_text(family = "serif"),
    axis.title.y = element_markdown(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_markdown(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "right" 
  )

## Volume ----
library(ggplot2)
library(ggtext)
library(dplyr)

## Volume ----
### calculate percentages ----
data$PCV_100 <- abs(data$PCV) * 100
data <- data[data$PCV_100 <= 100, ]
data$cat_V <- NA

### assign categories ----
for (i in 1:nrow(data)) {  
  if (data$V_2022[i] > data$V_2023[i]) {
    data$cat_V[i] <- "Erosion"  
  } else {
    data$cat_V[i] <- "Deposition"
  }
}

### summary ----
summary_vol <- data %>% 
  group_by(morphology, cat_V) %>% 
  summarise(meanPCV = mean(PCV_100), sePCV = sd(PCV_100)/sqrt(n())) %>% 
  ungroup() 

summary_vol <- summary_vol %>%
  mutate(meanPCV = ifelse(cat_V == "Erosion", -meanPCV, meanPCV))

# 1. Update data labels with standard HTML italics for Acropora
summary_vol <- summary_vol %>%
  mutate(morphology = gsub("Acropora", "<i>Acropora</i>", morphology))

### barplot ----
ggplot(summary_vol, aes(x = morphology, y = meanPCV, fill = cat_V)) +
  geom_bar(stat = "identity", position = position_dodge(0.5), color = "black", size = 0.5, 
           width = 0.5) + 
  geom_errorbar(aes(ymin = meanPCV - sePCV, ymax = meanPCV + sePCV), 
                width = 0.2, position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Deposition" = "black", "Erosion" = "white")) +  
  theme_classic() +
  labs(
    x = "Morphology", 
    y = "<i>Acropora</i> Volume Percentage Change" # Standard HTML italics here too
  ) +
  theme(
    # Use standard system serif font (Safe, no warnings, won't blow up sizing)
    text = element_text(family = "serif"),
    
    # Render Y axis title with italics
    axis.title.y = element_markdown(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    
    # Render X axis category labels with italics
    axis.text.x = element_markdown(size = 12),
    axis.text.y = element_text(size = 12),          
    
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

### Volume (original version)----
### calculate percentages ----
data$PCV_100 <- abs(data$PCV)*100
data <- data[data$PCV_100 <= 100, ]
data$cat_V <- NA

### assign categories ----
for (i in 1:nrow(data)) {  
  if (data$V_2022[i] > data$V_2023[i]) {
    data$cat_V[i] <- "Erosion"  
  } else {
    data$cat_V[i] <- "Deposition"
  }
}

### summary ----
summary_vol <- data %>% group_by(morphology, cat_V) %>% 
  summarise(meanPCV = mean(PCV_100), sePCV = sd(PCV_100)/sqrt(n())) %>% 
  ungroup() 

summary_vol <- summary_vol %>%
  mutate(meanPCV = ifelse(cat_V == "Erosion", -meanPCV, meanPCV))

### barplot ----
ggplot(summary_vol, aes(x = morphology, y = meanPCV, fill = cat_V)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, 
           width = 0.5) + 
  geom_errorbar(aes(ymin = meanPCV - sePCV, ymax = meanPCV + sePCV), 
                width = 0.2, position = position_dodge(0.5)) + 
  theme_minimal() +
  labs(x = "Morphology", y = "Volume Percentage Change") +
  scale_fill_manual(values = c("Deposition" = "black", "Erosion" = "white")) +  
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),       
    axis.title.y = element_text(size = 14),       
    axis.text = element_text(size = 12),         
    legend.text = element_text(size = 12)         
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

## Surface Area by Exposure ----
### summary ----
summary_surface_exp <- data %>% 
  filter(!is.na(cat_SA)) %>% 
  group_by(exposure, cat_SA) %>% 
  summarise(
    meanPCA = mean(PCA_100, na.rm = TRUE), 
    sePCA = sd(PCA_100, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

# For "Area reduction" assign negative values
summary_surface_exp <- summary_surface_exp %>%
  mutate(meanPCA = ifelse(cat_SA == "Area reduction", -meanPCA, meanPCA))

#### barplot ----
ggplot(summary_surface_exp, aes(x = exposure, y = meanPCA, fill = cat_SA)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, width = 0.5) +
  geom_errorbar(aes(ymin = meanPCA - sePCA, ymax = meanPCA + sePCA),
                width = 0.2, position = position_dodge(0.5)) +
  theme_minimal() +
  labs(x = "Exposure", y = "Surface Area Percentage Change") +
  scale_fill_manual(values = c("Area increase" = "black", 
                               "Area reduction" = "white")) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")


## Volume by Exposure ----
### summary ----
summary_vol_exp <- data %>% 
  filter(!is.na(cat_V)) %>% 
  group_by(exposure, cat_V) %>% 
  summarise(
    meanPCV = mean(PCV_100, na.rm = TRUE), 
    sePCV = sd(PCV_100, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

# For "Erosion" assign negative values
summary_vol_exp <- summary_vol_exp %>%
  mutate(meanPCV = ifelse(cat_V == "Erosion", -meanPCV, meanPCV))

#### barplot ----
ggplot(summary_vol_exp, aes(x = exposure, y = meanPCV, fill = cat_V)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, width = 0.5) +
  geom_errorbar(aes(ymin = meanPCV - sePCV, ymax = meanPCV + sePCV),
                width = 0.2, position = position_dodge(0.5)) +
  theme_minimal() +
  labs(x = "Exposure", y = "Volume Percentage Change") +
  scale_fill_manual(values = c("Deposition" = "black", "Erosion" = "white")) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
