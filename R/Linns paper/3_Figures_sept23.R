#Figures
source("R/Linns paper/1_Import_Data.R")
source("R/Linns paper/3_Analysis+Figures_TemperatureData.R")

library(ggplot2)
library(dplyr)
library(broom.mixed)
library(nlme)
library(ggpubr)

SnowmeltSeedMass <- dat_DOY %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Snowmelt_doy, y = log(Seed_mass), fill = Treatment, color = Treatment)) +
  geom_smooth(method = "lm") +
  scale_fill_manual (values =  c("#FC4E07", "#E69F00")) +
  scale_color_manual (values =  c("#FC4E07", "#E69F00")) +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", fill = "Treatment") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(SnowmeltSeedMass, filename = "Figures/SnowmeltSeedMass.jpeg", height = 6, width = 8)
  
SnowmeltSeedMass2 <- dat_DOY %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Snowmelt_doy, y = log(Seed_mass), fill = Year, color = Year)) +
  geom_smooth(method = "lm") +
  scale_fill_manual (values =  c("#CC79A7", "#660066")) +
  scale_color_manual (values =  c("#CC79A7", "#660066")) +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", fill = "Year") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(SnowmeltSeedMass2, filename = "Figures/SnowmeltSeedMass2.jpeg", height = 6, width = 8)

#############


TempSeedMass <- dat_DOY %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Temp_total.cen, y = log(Seed_mass), fill = Treatment, color = Treatment)) +
  geom_smooth(method = "lm") +
  scale_fill_manual (values =  c("#FC4E07", "#E69F00")) +
  scale_color_manual (values =  c("#FC4E07", "#E69F00")) +
  labs(x = "Cumulative temperature", y = "log(Seed mass in g)", fill = "Treatment") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(TempSeedMass, filename = "Figures/TempSeedMass.jpeg", height = 6, width = 8)



#################

FlowerTemp <- dat_DOY %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(y = MeanFlower.cen, x = Temp_total.cen, fill = Year, color = Year)) +
  geom_smooth(method = "loess", span = 1) +
  scale_fill_manual (values =  c("#CC79A7", "#660066")) +
  scale_color_manual (values =  c("#CC79A7", "#660066")) +
  labs(y = "Flower abundance", x = "Cumulative temperature", fill = "Year") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(FlowerTemp, filename = "Figures/FlowerTemp.jpeg", height = 6, width = 8)

FlowerSeedMass <- dat_DOY %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = MeanFlower.cen, y = log(Seed_mass), fill = Year, color = Year)) +
  geom_smooth(method = "loess", span = 1) +
  scale_fill_manual (values =  c("#CC79A7", "#660066")) +
  scale_color_manual (values =  c("#CC79A7", "#660066")) +
  labs(x = "Flower abundance", y = "log(Seed mass in g)", fill = "Year") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(FlowerSeedMass, filename = "Figures/FlowerSeedMass.jpeg", height = 6, width = 8)

#Ikke relevant plot
dat_DOY %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Snowmelt_doy, y = PeakFlower_doy, fill = Year, color = Year)) +
  geom_smooth(method = "loess", span = 1) +
  scale_fill_manual (values =  c("#CC79A7", "#660066")) +
  scale_color_manual (values =  c("#CC79A7", "#660066")) +
  labs(x = "Snowmelt doy", y = "Peak flower doy", fill = "Year") +
  theme_light() +
  facet_wrap(~ Year) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))

#Lage modell med prediktor (?), høy temp - få blomster, høy temp - mange blomster, lav temp - få blomster, høy temp - mange blomster

#


TempSeedNumb <- dat_DOY %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Temp_total.cen, y = Seed_number, fill = Treatment, color = Treatment)) +
  geom_smooth(method = "lm") +
  scale_fill_manual (values =  c("#FC4E07", "#E69F00")) +
  scale_color_manual (values =  c("#FC4E07", "#E69F00")) +
  labs(x = "Cumulative temperature", y = "Seed number", fill = "Treatment") +
  theme_light() +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(TempSeedNumb, filename = "Figures/TempSeedNumb.jpeg", height = 6, width = 8)


SnowSeedNumb <- dat_DOY %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Snowmelt_doy, y = Seed_number, fill = Treatment, color = Treatment)) +
  geom_smooth(method = "lm") +
  scale_fill_manual (values =  c("#FC4E07", "#E69F00")) +
  scale_color_manual (values =  c("#FC4E07", "#E69F00")) +
  labs(x = "Snowmelt doy", y = "Seed number", fill = "Treatment") +
  theme_light() +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none") +
  theme(strip.text = element_text(color = "black"))
ggsave(SnowSeedNumb, filename = "Figures/SnowSeedNumb.jpeg", height = 6, width = 8)
















###### SIMPLE PLOTS

AbundanceDoy <- pollination2 %>% 
  mutate(stage = recode(stage, "F" = "1", "E" = "2", "M" = "3", "L" = "4")) %>%
  ggplot(aes( x = doy, y = flower.mean, color = stage)) +
  geom_point() +
  geom_smooth(aes(fill = stage), span = 0.7) + #add span = 0.5, but line gets less smooth, and several warning messages
  scale_color_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Day of the year (doy)", y = "Mean number of flowers", color = "Snowmelt stages") +
  theme_light() +
  guides(fill = guide_legend(title = "Snowmelt stages", override.aes = list(colour = NULL)), colour = "none") +
  facet_wrap(~ year.poll) +
  theme(strip.text = element_text(color = "black"))
ggsave(AbundanceDoy, filename = "Figures/AbundanceDoy.jpeg", height = 6, width = 8)



## Pollination treatment
plot4 <- dat %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Treatment, y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin() +
  scale_fill_manual (values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Treatment", y = "log(Seed mass in g)", fill = "Snowmelt stages") +
  theme_light() +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = position_dodge(width = 0.9), color = "black") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))
ggsave(plot4, filename = "Figures/TreatmentStagesSeedMass.jpeg", height = 6, width = 8) 



## Plots without .fitted for seed mass

plot1 <- ggplot(dat, aes(x = MeanFlower.cen, y = log(Seed_mass), color = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm, aes(fill = factor(Stage2))) +
  scale_color_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Mean number of flowers", y = "log(Seed mass in g)", color = "Snowmelt stages", tag = "a") +
  theme_light() +
  guides(fill = guide_legend(title = "Snowmelt stages", override.aes = list(colour = NULL)), colour = "none") +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))


plot2 <- ggplot(dat, aes(x = CumTemp_after.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm, aes(fill = factor(Stage2))) +
  scale_color_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Cumulative temperature", y = "log(Seed mass in g)", color = "Snowmelt stages", tag = "b") +
  theme_light() +
  guides(fill = guide_legend(title = "Snowmelt stages", override.aes = list(colour = NULL)), colour = "none") +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))
  
  

plot3 <- dat %>%
  ggplot(aes(x = factor(Stage2), y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin() +
  scale_fill_manual (values =  c("#CC79A7", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Snowmelt stages", y = "log(Seed mass in g)", fill = "Snowmelt stages", tag = "c") +
  theme_light() +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = position_dodge(width = 0.9), color = "black") +
  facet_grid(~ Year) +
  theme(strip.text = element_text(color = "black"))


final_arrangement <- ggarrange(plot1, plot2, plot3, ncol = 1)

ggsave("Figures/SeedMass_fullModel.jpg", final_arrangement, width = 10, height = 12, dpi = 300)


## Plots without .fitted for seed number

plot5 <- ggplot(dat %>% filter(Year == 2016), aes(x = MeanFlower.cen, y = Seed_number, color = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm, aes(fill = factor(Stage2))) +
  scale_color_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Mean number of flowers", y = "Number of seeds", color = "Snowmelt stages", tag = "a") +
  theme_light() +
  guides(fill = guide_legend(title = "Snowmelt stages", override.aes = list(colour = NULL)), colour = "none") +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))


plot6 <- ggplot(dat %>% filter(Year == 2016), aes(x = CumTemp_after.cen, y = Seed_number, colour = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm, aes(fill = factor(Stage2))) +
  scale_color_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Cumulative temperature", y = "Seed number", color = "Snowmelt stages", tag = "b") +
  theme_light() +
  guides(fill = guide_legend(title = "Snowmelt stages", override.aes = list(colour = NULL)), colour = "none") +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))



plot7 <- dat %>%
  filter(Year == 2016) %>% 
  ggplot(aes(x = factor(Stage2), y = Seed_number, fill = factor(Stage2))) +
  geom_violin() +
  scale_fill_manual (values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Snowmelt stages", y = "Seed number", fill = "Snowmelt stages", tag = "c") +
  theme_light() +
  theme(plot.tag.position = c(0.01, 0.95), plot.tag = element_text(size = 26)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = position_dodge(width = 0.9), color = "black") +
  facet_wrap(~ Year) +
  theme(strip.text = element_text(color = "black"))


final_arrangement2 <- ggarrange(plot5, plot6, plot7, ncol = 1)

ggsave("Figures/SeedNumber_fullModel.jpg", final_arrangement2, width = 10, height = 12, dpi = 300)

## Pollination treatment
plot8 <- dat %>% 
  filter(Year == 2016) %>% 
  mutate(Treatment = recode(Treatment, "Pollinated" = "Supplemental pollination", "Control" = "Naturally pollinated")) %>%
  ggplot(aes(x = Treatment, y = Seed_number, fill = factor(Stage2))) +
  geom_violin() +
  scale_fill_manual (values =  c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Treatment", y = "Number of seeds", fill = "Snowmelt stages") +
  theme_light() +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = position_dodge(width = 0.9), color = "black") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Year)
ggsave(plot8, filename = "Figures/TreatmentStagesSeedNumber.jpeg", height = 6, width = 8) 


## Temperature both years + from first flower to last collected seed + peak flowering
TemperatureFinse_comb2 <- ggplot() +
  geom_smooth(data = Temperature_all, aes(x = doy, y = Temp_2016_ALR, color = "2016", linetype = "2016"), alpha = 0, span = 0.8) +
  geom_smooth(data = Temperature_all, aes(x = doy, y = Temp_2017_ALR, color = "2017", linetype = "2017"), alpha = 0, span = 0.8) +
  labs(x = "Day of the year (doy)", y = "Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("2016" = "#000000", "2017" = "#000000")) +
  scale_linetype_manual(values = c("2016" = "solid", "2017" = "dotted"), labels = c("2016", "2017")) +
  theme(legend.position = c(0.02, 0.9), legend.justification = c(0, 1),
        panel.background = element_blank(), text = element_text(size = 14),
        plot.title = element_text(hjust = 0, vjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  geom_rect(aes(xmin=162, xmax=219, ymin=1, ymax=1.2, fill = "1"), alpha=0.5) + 
  geom_rect(aes(xmin=169, xmax=223, ymin=0.8, ymax=1, fill = "2a"), alpha=0.5) + #but two flowers were hand pollinated already 2nd of july, doy= 183
  geom_rect(aes(xmin=175, xmax=228, ymin=0.6, ymax=0.8, fill = "2b"), alpha = 0.5) +
  geom_rect(aes(xmin=186, xmax=228, ymin=0.4, ymax=0.6, fill = "3a"), alpha = 0.5) +
  geom_rect(aes(xmin=189, xmax=232, ymin=0.2, ymax=0.4, fill = "3b"), alpha = 0.5) +
  geom_rect(aes(xmin=197, xmax=247, ymin=0, ymax=0.2, fill = "4"), alpha = 0.5) +
  geom_text(aes(x = 207, y = 1.1, label = "1: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 203, y = 0.9, label = "2: 2016"), color = "brown", size = 3) +
  geom_text(aes(x = 214.5, y = 0.7, label = "2: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 214, y = 0.5, label = "3: 2016"), color = "brown", size = 3) +
  geom_text(aes(x = 218, y = 0.3, label = "3: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 237.5, y = 0.1, label = "4: 2016"), color = "brown", size = 3) +
  geom_point(aes(x = 197, y = 1.1, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 197, y = 0.9, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 206, y = 0.7, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 205, y = 0.5, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 207, y = 0.3, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 209, y = 0.1, label = "none"), shape = 8, size = 2) +
  scale_fill_manual("Stage", values = c("1" = "#CC79A7", "2a" = "#00AFBB", "2b" = "#00AFBB", "3a" = "#E7B800", "3b" = "#E7B800", "4" = "#FC4E07")) +
  guides(linetype = guide_legend(title = "Year", override.aes = list(color = "black")), fill = "none", color = "none")

ggsave(TemperatureFinse_comb2, filename = "Figures/TemperatureFinse_comb2.jpeg", height = 6, width = 8)
