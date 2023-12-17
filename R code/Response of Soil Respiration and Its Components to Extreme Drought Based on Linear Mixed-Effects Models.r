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


hztemp <- read.csv("E:\\RP\\TFR_SRDB_20230422.csv")
glimpse(hztemp , width = 40)
TRRs <- filter(hztemp,RS_component == "Rs")
glimpse(TRRs, width = 40)

p <- ggplot(TRRs, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p





TRRs_Natural <- filter(hztemp,RS_component == "Rs" & Ecosystem_state == "Natural")
glimpse(TRRs_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRs_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRs_Managed <- filter(hztemp,RS_component == "Rs" & Ecosystem_state == "Managed")
glimpse(TRRs_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRs_Managed)
summary(m1)
r.squaredGLMM(m1)






TRRs_Cropland <- filter(hztemp,RS_component == "Rs" & Ecosystem_type == "Cropland")
glimpse(TRRs_Cropland, width = 40)
p <- ggplot(TRRs_Cropland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p



m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRs_Cropland)
summary(m1)
r.squaredGLMM(m1)



TRRs_Forest <- filter(hztemp,RS_component == "Rs" & Ecosystem_type == "Forest")
glimpse(TRRs_Forest, width = 40)
p <- ggplot(TRRs_Forest, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRs_Forest_Natural <- filter(TRRs_Forest,RS_component == "Rs" & Ecosystem_state == "Natural")
glimpse(TRRs_Forest_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRs_Forest_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRs_Forest_Managed <- filter(TRRs_Forest,RS_component == "Rs" & Ecosystem_state == "Managed")
glimpse(TRRs_Forest_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRs_Forest_Managed)
summary(m1)
r.squaredGLMM(m1)




TRRs_Grassland <- filter(hztemp,RS_component == "Rs" & Ecosystem_type == "Grassland")
glimpse(TRRs_Grassland, width = 40)
p <- ggplot(TRRs_Grassland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRs_Grassland_Natural <- filter(TRRs_Grassland,RS_component == "Rs" & Ecosystem_state == "Natural")
glimpse(TRRs_Grassland_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRs_Grassland_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRs_Grassland_Managed <- filter(TRRs_Grassland,RS_component == "Rs" & Ecosystem_state == "Managed")
glimpse(TRRs_Grassland_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRs_Grassland_Managed)
summary(m1)
r.squaredGLMM(m1)




TRRa <- filter(hztemp,RS_component == "Ra")
glimpse(TRRa, width = 40)

p <- ggplot(TRRa, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRa_Natural <- filter(TRRa,RS_component == "Ra" & Ecosystem_state == "Natural")
glimpse(TRRa_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRa_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRa_Managed <- filter(TRRa,RS_component == "Ra" & Ecosystem_state == "Managed")
glimpse(TRRa_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRa_Managed)
summary(m1)
r.squaredGLMM(m1)




TRRa_Cropland <- filter(TRRa,RS_component == "Ra" & Ecosystem_type == "Cropland")
glimpse(TRRa_Cropland, width = 40)
p <- ggplot(TRRa_Cropland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p



m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRa_Cropland)
summary(m1)
r.squaredGLMM(m1)




TRRa_Forest <- filter(TRRa,RS_component == "Ra" & Ecosystem_type == "Forest")
glimpse(TRRa_Forest, width = 40)
p <- ggplot(TRRa_Forest, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRa_Forest_Natural <- filter(TRRa_Forest,RS_component == "Ra" & Ecosystem_state == "Natural")
glimpse(TRRa_Forest_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRa_Forest_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRa_Forest_Managed <- filter(TRRa_Forest,RS_component == "Ra" & Ecosystem_state == "Managed")
glimpse(TRRa_Forest_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRa_Forest_Managed)
summary(m1)
r.squaredGLMM(m1)




TRRa_Grassland <- filter(TRRa,RS_component == "Ra" & Ecosystem_type == "Grassland")
glimpse(TRRa_Grassland, width = 40)
p <- ggplot(TRRa_Grassland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p



TRRa_Grassland_Natural <- filter(TRRa_Grassland,RS_component == "Ra" & Ecosystem_state == "Natural")
glimpse(TRRa_Grassland_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRa_Grassland_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRa_Grassland_Managed <- filter(TRRa_Grassland,RS_component == "Ra" & Ecosystem_state == "Managed")
glimpse(TRRa_Grassland_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRa_Grassland_Managed)
summary(m1)
r.squaredGLMM(m1)



TRRh <- filter(hztemp,RS_component == "Rh")
glimpse(TRRh, width = 40)

p <- ggplot(TRRh, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRh_Natural <- filter(TRRh,RS_component == "Rh" & Ecosystem_state == "Natural")
glimpse(TRRh_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRh_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRh_Managed <- filter(TRRh,RS_component == "Rh" & Ecosystem_state == "Managed")
glimpse(TRRh_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRh_Managed)
summary(m1)
r.squaredGLMM(m1)



TRRh_Cropland <- filter(TRRh,RS_component == "Rh" & Ecosystem_type == "Cropland")
glimpse(TRRh_Cropland, width = 40)
p <- ggplot(TRRh_Cropland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRh_Cropland)
summary(m1)
r.squaredGLMM(m1)




TRRh_Forest <- filter(TRRh,RS_component == "Rh" & Ecosystem_type == "Forest")
glimpse(TRRh_Forest, width = 40)
p <- ggplot(TRRh_Forest, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRh_Forest_Natural <- filter(TRRh_Forest,RS_component == "Rh" & Ecosystem_state == "Natural")
glimpse(TRRh_Forest_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRh_Forest_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRh_Forest_Managed <- filter(TRRh_Forest,RS_component == "Rh" & Ecosystem_state == "Managed")
glimpse(TRRh_Forest_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRh_Forest_Managed)
summary(m1)
r.squaredGLMM(m1)





TRRh_Grassland <- filter(TRRh,RS_component == "Rh" & Ecosystem_type == "Grassland")
glimpse(TRRh_Grassland, width = 40)
p <- ggplot(TRRh_Grassland, aes(x = SPEI_min, y = RS)) + geom_point(size=2,aes(color=Ecosystem_state),alpha = 0.3) + geom_smooth(aes(color =Ecosystem_state), method = 'lm', se = TRUE, show.legend = FALSE) + theme_bw() + theme(axis.title.x =element_text(size=17), axis.title.y=element_text(size=17),axis.text=element_text(size=14,family ="Times", face = "plain"))
p




TRRh_Grassland_Natural <- filter(TRRh_Grassland,RS_component == "Rh" & Ecosystem_state == "Natural")
glimpse(TRRh_Grassland_Natural, width = 40)


m0 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude) , data = TRRh_Grassland_Natural)
summary(m0)
r.squaredGLMM(m0)




TRRh_Grassland_Managed <- filter(TRRh_Grassland,RS_component == "Rh" & Ecosystem_state == "Managed")
glimpse(TRRh_Grassland_Managed, width = 40)


m1 <- lmer(RS ~ SPEI_min + (1 | Latitude) + (1 | Longitude), data = TRRh_Grassland_Managed)
summary(m1)
r.squaredGLMM(m1)