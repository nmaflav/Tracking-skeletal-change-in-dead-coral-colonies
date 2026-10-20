# 3) Data analysis ----
## 3.1) data ----
library(readr)
data <- read_csv("C:/Users/data.csv")
View(data)

## packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(FSA)

## 3.2) Assumptions ----
### 3.2.1) normality ----
shapiro_test <- data %>%
  group_by(morphology) %>%
  summarise(p_value = shapiro.test(avet)$p.value)

print(shapiro_test)

### 3.2.2) homogeneity of variance ----
leveneTest(avet ~ morphology, data = data)

## 3.3) Models ----
### 3.3.1) 3D deviations (avet) ----
mod_3d_avet <- lmer(
  avet ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_3d_avet)
anova(mod_3d_avet)

### 3.3.2) 3D deviations (rms) ----
mod_3d_rms <- lmer(
  rms ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_3d_rms)
anova(mod_3d_rms)

### 3.3.3) Surface Area ----
mod_surf <- lmer(
  PCA ~ morphology + exposure + depth +
    (1 | reef) + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_surf)
anova(mod_surf)

### 3.3.4) Volume ----
mod_vol <- lmer(
  PCV ~ morphology + exposure + depth 
  + (1 | site),
  data = data,
  REML = TRUE
)

summary(mod_vol)
anova(mod_vol)

## 3.4) Residual diagnostics ----
### 3.4.1) 3D deviations (avet) ----
par(mfrow = c(1,2))
plot(resid(mod_3d_avet))
qqnorm(resid((mod_3d_avet)))
qqline(resid((mod_3d_avet)))

### 3.4.2) 3D deviations (rms) ----
par(mfrow = c(1,2))
plot(resid(mod_3d_rms))
qqnorm(resid((mod_3d_rms)))
qqline(resid((mod_3d_rms)))

### 3.4.3) Surface area ----
par(mfrow = c(1,2))
plot(resid(mod_surf))
qqnorm(resid(mod_surf))
qqline(resid(mod_surf))

### 3.4.4) Volume ----
par(mfrow = c(1,2))
plot(resid(mod_vol))
qqnorm(resid(mod_vol))
qqline(resid(mod_vol))

## 3.5) Visualization ----
### 3.5.1) Surface Area ----
#### 3.5.1.1) calculate percentages ----
data$PCA_100 <- abs(data$PCA)*100
data <- data[data$PCA_100 <= 100, ]
data$cat_SA <- NA

#### 3.5.1.2) assign categories ----
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

#### 3.5.1.3) summary ----
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

#### 3.5.1.4) barplot ----
ggplot(summary_surface, aes(x = morphology, y = meanPCA, fill = cat_SA)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5, 
           width = 0.5) +  
  geom_errorbar(aes(ymin = meanPCA - sePCA, ymax = meanPCA + sePCA), 
                width = 0.2, position = position_dodge(0.5)) + 
  theme_minimal() +
  labs(x = "Morphology", y = "Surface Area Percentage Change") +
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

### 3.5.2) Volume ----
#### 3.5.2.1) calculate percentages ----
data$PCV_100 <- abs(data$PCV)*100
data <- data[data$PCV_100 <= 100, ]
data$cat_V <- NA

#### 3.5.2.2) assign categories ----
for (i in 1:nrow(data)) {  
  if (data$V_2022[i] > data$V_2023[i]) {
    data$cat_V[i] <- "Erosion"  
  } else {
    data$cat_V[i] <- "Deposition"
  }
}

#### 3.5.2.3) summary ----
summary_vol <- data %>% group_by(morphology, cat_V) %>% 
  summarise(meanPCV = mean(PCV_100), sePCV = sd(PCV_100)/sqrt(n())) %>% 
  ungroup() 

summary_vol <- summary_vol %>%
  mutate(meanPCV = ifelse(cat_V == "Erosion", -meanPCV, meanPCV))

#### 3.5.2.4) barplot ----
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

## 3.6) Other plots ----
### 3.6.1) Surface Area by Exposure ----
#### 3.6.1.1) summary ----
summary_surface_exp <- data %>% 
  filter(!is.na(cat_SA)) %>% 
  group_by(exposure, cat_SA) %>% 
  summarise(
    meanPCA = mean(PCA_100, na.rm = TRUE), 
    sePCA = sd(PCA_100, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

#### 3.6.1.2) For "Area reduction" assign negative values ----
summary_surface_exp <- summary_surface_exp %>%
  mutate(meanPCA = ifelse(cat_SA == "Area reduction", -meanPCA, meanPCA))

#### 3.6.1.3) barplot ----
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

### 3.6.2) Volume by Exposure ----
#### 3.6.2.1) summary ----
summary_vol_exp <- data %>% 
  filter(!is.na(cat_V)) %>% 
  group_by(exposure, cat_V) %>% 
  summarise(
    meanPCV = mean(PCV_100, na.rm = TRUE), 
    sePCV = sd(PCV_100, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()

#### 3.6.2.2) For "Erosion" assign negative values ----
summary_vol_exp <- summary_vol_exp %>%
  mutate(meanPCV = ifelse(cat_V == "Erosion", -meanPCV, meanPCV))

#### 3.6.2.3) barplot ----
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