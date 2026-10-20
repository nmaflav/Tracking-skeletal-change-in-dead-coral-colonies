# 2) Data cleaning and exploration ----
## 2.1) data ----
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

## 2.2) Tidy ----
### 2.2.1) variables as factors ----
data <- data %>%
  mutate(
    morphology = factor(morphology),
    reef       = factor(reef),
    site       = factor(site),
    exposure   = factor(exposure),
    depth      = factor(zone)
  )

### 2.2.2) Delete sub-massive: not enough observations ----
data <- subset(data, 
               !(morphology %in% c("Small massive/sub-massive")))

### 2.2.3) cleaning ----
data <- data[-c(48 ,55, 56), ] # models with poor Metashape reconstruction

## 2.3) Exploration ----
### 2.3.1) 3D deviations (avet) by taxa ----
ggplot(data, aes(x = morphology, y = avet, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "3D deviations (avet)") +
  theme_minimal() +
  theme(legend.position = "none")

### 2.3.2) 3D deviations (avet) by taxa ----
ggplot(data, aes(x = morphology, y = rms, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "3D deviations (rms)") +
  theme_minimal() +
  theme(legend.position = "none")

### 2.3.3) change in surface area by taxa ----
ggplot(data, aes(x = morphology, y = PCA, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "Change in Surface Area") +
  theme_minimal() +
  theme(legend.position = "none")

### 2.3.4) change in volume by taxa ----
ggplot(data, aes(x = morphology, y = PCV, fill = morphology)) +
  geom_boxplot() +
  labs(title = "Difference in 3D deviations by Morphology",
       x = "Morphology",
       y = "Change in Volume") +
  theme_minimal() +
  theme(legend.position = "none")

### 2.3.5) density plot: surface area ----
ggplot(data, aes(x = PCA, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()+
  theme(legend.position = "none")

### 2.3.5) density plot: volume ----
ggplot(data, aes(x = PCV, fill = morphology)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~morphology) +
  theme_minimal()+
  theme(legend.position = "none")