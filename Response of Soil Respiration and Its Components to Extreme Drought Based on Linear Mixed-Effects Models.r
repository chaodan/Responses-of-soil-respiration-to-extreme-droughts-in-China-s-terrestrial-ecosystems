##Analysis of the Response of Soil Respiration in China's Terrestrial Ecosystems to Extreme Drought Based on the Updated SRDB Dataset
library(RColorBrewer)
library(ggpubr)
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
library(wesanderson)
library(viridis)
library(cowplot)
library(lme4)
library(sjmisc)
library(effects)
library(sjstats)
library(lmerTest)
library(MuMIn)
library(Matrix)



hztemp <- read.csv("E:\\RP\\TFR_SRDB_20230914.csv")
glimpse(hztemp , width = 40)
TRRs <- filter(hztemp,RS_component == "Rs")
glimpse(TRRs, width = 40)

p <- ggplot(TRRs, aes(x = SPEI, y = LogRS)) + geom_point(size=3,aes(color=Ecosystem_type),alpha = 0.3) + geom_smooth(method = 'lm', size = 1.2, alpha = 0.2) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain")) +scale_color_brewer(palette="Set1")
p


m0 <- lmer(LogRS ~ SPEI + (1 | Latitude) + (1 | Longitude) , data = TRRs)
summary(m0)
r.squaredGLMM(m0)

ggsave("The response of Rs in terrestrial ecosystems in China to extreme drought..tiff",width=180, height=160, units="mm", dpi=800) 

# Save as PDF (vector format)
ggsave("TRRs_SPEI.pdf", plot = p, width = 180, height = 160, units = "mm", dpi = 800)





TRRa <- filter(hztemp,RS_component == "Ra")
glimpse(TRRa, width = 40)

p <- ggplot(TRRa, aes(x = SPEI, y = LogRS)) + geom_point(size=3,aes(color=Ecosystem_type),alpha = 0.3) + geom_smooth(method = 'lm', size = 1.2, alpha = 0.2) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain")) +scale_color_brewer(palette="Set1")
p


m0 <- lmer(LogRS ~ SPEI + (1 | Latitude) + (1 | Longitude) , data = TRRa)
summary(m0)
r.squaredGLMM(m0)

ggsave("The response of Ra in terrestrial ecosystems in China to extreme drought..tiff",width=180, height=160, units="mm", dpi=800) 

# Save as PDF (vector format)
ggsave("TRRa_SPEI.pdf", plot = p, width = 180, height = 160, units = "mm", dpi = 800)




TRRh <- filter(hztemp,RS_component == "Rh")
glimpse(TRRh, width = 40)

p <- ggplot(TRRh, aes(x = SPEI, y = LogRS)) + geom_point(size=3,aes(color=Ecosystem_type),alpha = 0.3) + geom_smooth(method = 'lm', size = 1.2, alpha = 0.2) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain")) +scale_color_brewer(palette="Set1")
p


m0 <- lmer(LogRS ~ SPEI + (1 | Latitude) + (1 | Longitude) , data = TRRh)
summary(m0)
r.squaredGLMM(m0)

ggsave("The response of Rh in terrestrial ecosystems in China to extreme drought..tiff",width=180, height=160, units="mm", dpi=800) 

# Save as PDF (vector format)
ggsave("TRRh_SPEI.pdf", plot = p, width = 180, height = 160, units = "mm", dpi = 800)
