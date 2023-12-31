## Analyzing Soil Respiration Distribution in China's Terrestrial Ecosystems using SRDB Data
install.packages("ggridges")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("reshape2")

# Load libraries
library(ggridges)
library(ggplot2)
library(viridis)
library(viridisLite)
library(hrbrthemes)
library(readr)
library(ggsci)
library(gridExtra)
library(dplyr)
library(plotluck)
library(psych)
library(ggthemes)
library(tidyverse)
library(Rmisc)
library(ggpubr)
library(reshape2)

# Read data
hztemp <- read.csv("E:\\RP\\SRDB_Manipulation.csv")
glimpse(hztemp, width = 40)

# Filter data for croplands
TRRs_Cropland <- filter(hztemp, Ecosystem_type == "Cropland")
glimpse(TRRs_Cropland, width = 40)

# Plot and save distribution for croplands
ggplot(TRRs_Cropland, aes(Manipulation, Rs_mean, color = Manipulation)) +
  geom_point(data = TRRs_Cropland, mapping = aes(Manipulation, Rs_mean), size = 4) +
  geom_errorbar(aes(ymin = Rs_mean - RS_sd, ymax = Rs_mean + RS_sd, color = Manipulation),
                width = 0, position = position_dodge(width = 0.7), cex = 0.9) +
  labs(y = "", x = "") + theme_bw() + coord_flip() +
  facet_grid(~RS_component, drop = TRUE, space = "free_x") +
  theme(strip.background = element_rect(fill = c("#FFF6E1"))) +
  theme(strip.text = element_text(size = 14, face = 'bold', colour = "#B8733A")) +
  theme(axis.text = element_text(colour = 'black', size = 10))

ggsave("Distribution_of_Soil_Respiration_and_Its_Components_in_Cropland_Ecosystems.tiff", width = 200, height = 340, units = "mm", dpi = 800)

# Similar steps for forests
TRRs_Forest <- filter(hztemp, Ecosystem_type == "Forest")
glimpse(TRRs_Forest, width = 40)

ggplot(TRRs_Forest, aes(Manipulation, Rs_mean, color = Manipulation)) +
  geom_point(data = TRRs_Forest, mapping = aes(Manipulation, Rs_mean), size = 4) +
  geom_errorbar(aes(ymin = Rs_mean - RS_sd, ymax = Rs_mean + RS_sd, color = Manipulation),
                width = 0, position = position_dodge(width = 0.7), cex = 0.9) +
  labs(y = "", x = "") + theme_bw() + coord_flip() +
  facet_grid(~RS_component, drop = TRUE, space = "free_x") +
  theme(strip.background = element_rect(fill = c("#FFF6E1"))) +
  theme(strip.text = element_text(size = 14, face = 'bold', colour = "#B8733A")) +
  theme(axis.text = element_text(colour = 'black', size = 10))

ggsave("Distribution_of_Soil_Respiration_and_Its_Components_in_Forest_Ecosystems.tiff", width = 200, height = 340, units = "mm", dpi = 800)

# Similar steps for grasslands
TRRs_Grassland <- filter(hztemp, Ecosystem_type == "Grassland")
glimpse(TRRs_Grassland, width = 40)

ggplot(TRRs_Grassland, aes(Manipulation, Rs_mean, color = Manipulation)) +
  geom_point(data = TRRs_Grassland, mapping = aes(Manipulation, Rs_mean), size = 4) +
  geom_errorbar(aes(ymin = Rs_mean - RS_sd, ymax = Rs_mean + RS_sd, color = Manipulation),
               
