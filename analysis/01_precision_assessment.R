# 1) Precision Assessment ----
## 1.1) Load data ----
library(readxl)
precision <- read_excel("C:/Users/precision.csv")
View(precision)

## packages ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(FSA)

## 1.2) Summary Statistics -----
library(dplyr)

summary_stats <- precision %>%
  mutate(
    sa_cm2  = sa_plane * 10000,     
    vol_cm3 = vol_plane * 1000000    
  ) %>%
  group_by(morphology, skeleton) %>%
  summarise(
    mean_vol_cm3 = mean(vol_cm3, na.rm = TRUE),
    sd_vol_cm3   = sd(vol_cm3, na.rm = TRUE),
    n_vol        = sum(!is.na(vol_cm3)),
    se_vol_cm3   = sd_vol_cm3 / sqrt(n_vol),
    precision_vol_pct = (se_vol_cm3 / mean_vol_cm3) * 100,
    mean_sa_cm2 = mean(sa_cm2, na.rm = TRUE),
    sd_sa_cm2   = sd(sa_cm2, na.rm = TRUE),
    n_sa        = sum(!is.na(sa_cm2)),
    se_sa_cm2   = sd_sa_cm2 / sqrt(n_sa),
    precision_sa_pct = (se_sa_cm2 / mean_sa_cm2) * 100,
    .groups = "drop"
  )

View(summary_stats)

## 1.3) Graphs ----
library(ggplot2)
library(ggtext)
library(dplyr)
library(showtext)

font_add_google("Lora", "lora")
showtext_auto()

### 1.3.1) Surface Area ----
#### 1.3.1.1) m2 ----
precision <- precision %>%
  mutate(morphology_italic = gsub("Acropora", "<i>Acropora</i>", morphology))

ggplot(precision, aes(x = code, y = sa_plane)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  facet_wrap(~ morphology_italic, scales = "free_x", strip.position = "bottom") +
  theme_classic() +
  labs(
    x = "Morphology",
    y = expression(bold("Surface Area (m"^2*")"))
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 16),
    strip.text = element_markdown(size = 16), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "none"
  )

#### 1.3.1.2) cm2 ----
precision$sa_plane_cm2 <- precision$sa_plane * 10000

ggplot(precision, aes(x = code, y = sa_plane_cm2)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  facet_wrap(~ morphology_italic, scales = "free_x", strip.position = "bottom") +
  theme_classic() +
  labs(
    x = "Morphology",
    y = expression(bold("Surface Area (cm"^2*")"))
  ) +
  theme(
    text = element_text(family = "lora"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 16),
    strip.text = element_markdown(size = 16, family = "lora"), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "none"
  )

### 1.3.2) Volume ----
####  1.3.2.1) m3----
precision$vol_plane <- as.numeric(precision$vol_plane)

precision <- precision %>%
  mutate(morphology_italic = gsub("Acropora", "<i>Acropora</i>", morphology))

ggplot(precision, aes(x = code, y = vol_plane)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  facet_wrap(~ morphology_italic, scales = "free_x", strip.position = "bottom") +
  theme_classic() +
  labs(
    x = "Morphology",
    y = expression(bold("Volume (m"^3*")"))
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 16),
    strip.text = element_markdown(size = 16), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "none"
  )

#### 1.3.2.2) cm3 ----
precision$vol_plane_cm3 <- precision$vol_plane * 1e6

precision <- precision %>%
  mutate(morphology_italic = gsub("Acropora", "<i>Acropora</i>", morphology))

ggplot(precision, aes(x = code, y = vol_plane_cm3)) +
  geom_boxplot(alpha = 0.9) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  facet_wrap(~ morphology_italic, scales = "free_x", strip.position = "bottom") +
  theme_classic() +
  labs(
    x = "Morphology",
    y = expression("Volume (cm"^3*")")
  ) +
  theme(
    text = element_text(family = "lora"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 16),
    strip.text = element_markdown(size = 16, family = "lora"), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "none"
  )

### 1.3.3) 3D Compare ----
#### 1.3.3.1) m ----
precision$rms <- as.numeric(precision$rms)

rms_summary_table_m <- precision %>%
  filter(!is.na(rms)) %>% 
  group_by(morphology, skeleton) %>% 
  summarise(
    n = n(),
    Average_RMS = sqrt(mean(rms^2)), 
    SE = sd(rms) / sqrt(n()),       
    RSE_pct = (SE / mean(rms)) * 100, 
    .groups = "drop"
  )

print(rms_summary_table_m)

#### 1.3.3.2) cm ----
precision$rms_cm <- as.numeric(precision$rms) *100

rms_summary_table_cm <- precision %>%
  filter(!is.na(rms_cm)) %>% 
  group_by(morphology, skeleton) %>% 
  summarise(
    n = n(),
    Average_RMS = sqrt(mean(rms_cm^2)), 
    SE = sd(rms_cm) / sqrt(n()),       
    RSE_pct = (SE / mean(rms_cm)) * 100, 
    .groups = "drop"
  )

print(rms_summary_table_cm)