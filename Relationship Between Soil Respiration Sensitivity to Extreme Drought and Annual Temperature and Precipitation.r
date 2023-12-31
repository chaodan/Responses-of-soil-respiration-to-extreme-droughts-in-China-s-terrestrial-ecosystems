## Relationship Between Soil Respiration Sensitivity to Extreme Drought and Annual Temperature and Precipitation
library(dplyr)
library(ggplot2)
library(plotluck)
library(psych)
library(ggthemes)
library(tidyverse)
library(Rmisc)
library(readr)
library(ggsci)
library(gridExtra)
library(ggpmisc)
library(ggpubr)

RS_sensitivity <- read_csv("E:\\RP\\Soil_Respiration_Sensitivity_to_Extreme_Drought_and_Relationship_with_Precipitation.csv")
glimpse(RS_sensitivity, width = 40)

# Plotting relationship between Soil Respiration Sensitivity and Mean Annual Temperature
p <- ggplot(RS_sensitivity, aes(x = MAT, y = PValue, colour = Veg)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#756bb1", fill = "#cbc9e2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

ggsave("Soil_Respiration_Sensitivity_to_Extreme_Drought_and_Annual_Temperature.tiff", width = 80, height = 80, units = "mm", dpi = 800)
p + stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 4) + stat_cor(label.y = 0.01, size = 4)

# Plotting relationship between Soil Respiration Sensitivity and Mean Annual Precipitation
p <- ggplot(RS_sensitivity, aes(x = MAT, y = PValue)) +
  geom_point(size = 1, alpha = 0.2, color = "#6baed6") +
  geom_smooth(method = "lm", formula = y ~ x, color = "#756bb1", fill = "#cbc9e2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

# Plotting relationship between Soil Respiration Sensitivity and Mean Annual Precipitation
p <- ggplot(RS_sensitivity, aes(x = MAP, y = PValue, colour = Veg)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#756bb1", fill = "#cbc9e2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

# Plotting relationship between Soil Respiration Sensitivity and Mean Annual Precipitation
p <- ggplot(RS_sensitivity, aes(x = MAP, y =
