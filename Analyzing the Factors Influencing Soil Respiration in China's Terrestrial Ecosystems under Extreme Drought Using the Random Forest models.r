## Analyzing the Factors Influencing Soil Respiration in China's Terrestrial Ecosystems under Extreme Drought Using the Random Forest models - Correlation Heatmaps and Contribution Plots

install.packages("patchwork")  # If not installed, install the patchwork package in R
library(psych)
library(reshape2)
library(ggplot2)
library(randomForest)
library(patchwork)
library(readr)
library(dplyr)
library(Matrix)
library(survival)
library(caret)
library(caTools)
library(cvms)
library(RColorBrewer)
library(ggplot2)
library(data.table)

# Read the dataset
RS <- read_csv("E:\\RP\\TFR_Extreme_Conditions_China_Terrestrial_Ecosystems_Soil_Respiration_Analysis_Dataset.csv")
glimpse(RS, width = 40)

# Filter data for croplands and calculate Spearman correlation coefficients
RS_croplands <- RS %>% filter(Vegetation == "croplands" & ED == "1")
spearman <- corr.test(RS_croplands[, 1:20], RS_croplands[, 21], method = 'spearman', adjust = 'none')
spearman  # View correlation matrix

# Similar steps for other vegetation types
RS_forests <- RS %>% filter(Vegetation == "forests" & ED == "1")
spearman <- corr.test(RS_forests[, 1:20], RS_forests[, 21], method = 'spearman', adjust = 'none')
spearman

RS_grasslands <- RS %>% filter(Vegetation == "grasslands" & ED == "1")
spearman <- corr.test(RS_grasslands[, 1:20], RS_grasslands[, 21], method = 'spearman', adjust = 'none')
spearman

RS_savannas <- RS %>% filter(Vegetation == "savannas" & ED == "1")
spearman <- corr.test(RS_savannas[, 1:20], RS_savannas[, 21], method = 'spearman', adjust = 'none')
spearman

RS_shrublands <- RS %>% filter(Vegetation == "shrublands" & ED == "1")
spearman <- corr.test(RS_savannas[, 1:20], RS_savannas[, 21], method = 'spearman', adjust = 'none')
spearman

# Load previously calculated correlation coefficients
spearman_Coefficient <- read_csv("E:\\RP\\Extreme_Conditions_China_Terrestrial_Ecosystems_Soil_Respiration_Correlation_Coefficients.csv")
glimpse(spearman_Coefficient, width = 40)

# Create a correlation heatmap
p1 <- ggplot() +
  geom_tile(data = spearman_Coefficient, aes(x = Vegetation, y = Variable, fill = Coefficient)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = 'black'), axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y = '', x = '', fill = 'Correlation')

# View the correlation heatmap
p1

# Train random forest models for each vegetation type
croplands <- randomForest(RS ~ LON + LAT + SPEI + VPD + AN + AP + BD + PH + SOM + TK + TN + TP + SM + ST + GPP + LAI + MAT + MAP + ET + RH,
                          data = RS_croplands, importance = TRUE, ntree = 1500)
croplands1 <- data.frame(importance(croplands, scale = TRUE), check.names = FALSE)

forests <- randomForest(RS ~ LON + LAT + SPEI + VPD + AN + AP + BD + PH + SOM + TK + TN + TP + SM + ST + GPP + LAI + MAT + MAP + ET + RH,
                       data = RS_forests, importance = TRUE, ntree = 1500)
forests1 <- data.frame(importance(forests, scale = TRUE), check.names = FALSE)

grasslands <- randomForest(RS ~ LON + LAT + SPEI + VPD + AN + AP + BD + PH + SOM + TK + TN + TP + SM + ST + GPP + LAI + MAT + MAP + ET + RH,
                           data = RS_grasslands, importance = TRUE, ntree = 1500)
grasslands
