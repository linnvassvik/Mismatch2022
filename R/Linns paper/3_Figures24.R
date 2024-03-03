source("R/Linns paper/2_Analysis24.R")

library(patchwork)
########## FIGURES ###################

## Temperature and Snowmelt affects seed mass
#2016
#Doy on x
# Temp_doy_16a <- expand.grid(Snowmelt_doy = seq(169, 197, length = 100), Temp_total.cen = c(-2, 1.8), Seed_mass = 0)
# 
# 
# TempSnow16a <- make_prediction(Temp_doy_16a, sm_model_16_3)
# 
# #*sd + mean
# 
# TempSnow16a_plot <- make_prettyplot(dat = dat16, 
#                                     Newdata = TempSnow16a, 
#                                     xaxis = Snowmelt_doy, 
#                                     yaxis = log(Seed_mass), 
#                                     prediction = pred, 
#                                     ColorVariable = as.factor(Temp_total.cen),
#                                     SE = SE) +
#   scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
#   scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
#   labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "2016") +
#   theme_minimal()


#Temp on x
Temp_doy_16b <- expand.grid(Snowmelt_doy = mean(dat16$Snowmelt_doy), Temp_total.cen = seq(-1.8, 0.5, length = 100), Seed_mass = 0, Year = 2016)

TempSnow16b <- make_prediction(Temp_doy_16b, sm_model_16_3)%>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

TempSnow16b_plot <- make_prettyplot(dat = dat16, 
                                    Newdata = TempSnow16b, 
                                    xaxis = Temp_total, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = "#666666",
                                    SE = SE,
                                    line_type = "dashed",
                                    shape = as.factor(Year)) +
  scale_color_manual(values = "#666666", name = "doy", label = "mean") +
  scale_fill_manual(values = "#666666", name = "doy", label = "mean") +
  labs(x = "Cumulative temperature above 0 °C", y = "log(Seed mass (g))", title = "2016") +
  theme_minimal() +
  ylim(-12, -2) +
  theme(legend.position = "none")


#2017
#doy on x
# Temp_doy_17a <- expand.grid(Snowmelt_doy.cen = seq(-2, 0.8, length = 100), Temp_total.cen = c(-2,2), Seed_mass = 0)
# 
# TempSnow17a <- make_prediction(Temp_doy_17a, sm_model_17_3)
# 
# TempSnow17a_plot <- make_prettyplot(dat = dat17, 
#                                     Newdata = TempSnow17a, 
#                                     xaxis = Snowmelt_doy.cen, 
#                                     yaxis = log(Seed_mass), 
#                                     prediction = pred, 
#                                     ColorVariable = as.factor(Temp_total.cen),
#                                     SE = SE) +
#   scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
#   scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
#   labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "2017") +
#   theme_minimal()

#Temp on x
Temp_doy_17b <- expand.grid(Snowmelt_doy = mean(dat16$Snowmelt_doy), Temp_total.cen = seq(-0.8, 2.5, length = 100), Seed_mass = 0)

TempSnow17b <- make_prediction(Temp_doy_17b, sm_model_17_3)%>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

TempSnow17b_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = TempSnow17b, 
                                    xaxis = Temp_total, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = "#666666",
                                    SE = SE,
                                    line_type = "dashed") +
  scale_color_manual(values = c("#666666"), name = "doy") +
  scale_fill_manual(values = c("#666666"), name = "doy") +
  labs(x = "Cumulative temperature above 0 °C", y = "", title = "2017") +
  theme_minimal() +
  ylim(-12, -2) +
  theme(legend.position = "none")
  


# Temp_snowmelt <- ggarrange(TempSnow16a_plot, TempSnow17a_plot, common.legend = TRUE) 
# ggsave(Temp_snowmelt, filename = "Figures/Temp_snowmelt.jpeg", height = 6, width = 8)


Temp_snowmelt_2 <- ggarrange(TempSnow16b_plot, TempSnow17b_plot)
ggsave(Temp_snowmelt_2, filename = "Figures/Temp_snowmelt_2.jpeg", height = 6, width = 8)


## Biotic factors affect seed mass

#Treatment
#2016
Newdata16_treatment <- expand.grid(Snowmelt_doy = mean(dat16$Snowmelt_doy), Temp_total.cen = seq(-1.8, 0.4, length = 10), Seed_mass = 0, Treatment = c("Control", "Pollinated"))

Treatment16 <- make_prediction(Newdata16_treatment, sm_model_16_6)%>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Treatment16_plot <- make_prettyplot(dat = dat16,
                                    Newdata = Treatment16, 
                                    xaxis = Temp_total, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = Treatment, 
                                    ColorDot = Treatment, 
                                    SE = SE,
                                    line_type = "solid") +
  scale_color_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Cumulative temperature above 0 °C", y = "log(Seed mass (g))", title = "2016") +
  ylim(-12,-2)

#2017
Newdata17_treatment <- expand.grid(Snowmelt_doy = mean(dat17$Snowmelt_doy), Temp_total.cen = seq(-0.8, 2.5, length = 100), Seed_mass = 0, Treatment = c("Control", "Pollinated"))

Treatment17 <- make_prediction(Newdata17_treatment, sm_model_17_6)%>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Treatment17_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = Treatment17, 
                                    xaxis = Temp_total, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = Treatment, 
                                    ColorDot = Treatment,
                                    SE = SE,
                                    line_type = "solid") +
  scale_color_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Cumulative temperature above 0 °C", y = "log(Seed mass (g))", title = "2017") +
  ylim(-12,-2)

TreatmentPlot <- ggarrange(Treatment16_plot, Treatment17_plot, common.legend = TRUE, legend = "bottom") 
ggsave(TreatmentPlot, filename = "Figures/TreatmentPlot.jpeg", height = 6, width = 8)



#Mean flower only
Newdata16_Flower <- expand.grid(Temp_total.cen = seq(-1.9, 0.4, length = 100), MeanFlower.cen = c(-0.778, 1.92), Seed_mass = 0, Snowmelt_doy = mean(dat16$Snowmelt_doy))

Flower16 <- make_prediction(Newdata16_Flower, sm_model_16_4) %>% 
  mutate(MeanFlowers = (MeanFlower.cen*sd(WeatherAndBiomass$MeanFlowers)+mean(WeatherAndBiomass$MeanFlowers))) %>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Flower16_plot <- make_prettyplot(dat = dat16, 
                                 Newdata = Flower16, 
                                 xaxis = Temp_total, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = as.factor(MeanFlowers),
                                 SE = SE,
                                 line_type = "solid") +
  scale_color_manual(values = c("#6699CC", "#000033"), name = "Mean flowers (count)", labels = c("low (400)", "high (6500)")) +
  scale_fill_manual(values = c("#6699CC", "#000033"), name = "Mean flowers (count)", labels = c("low (400)", "high (6500)")) +
  ylim(-12, 0) +
  labs(x = "Cumulative temperature above 0 °C", y = "log(Seed mass in (g))", title = "2016") +
  theme(legend.position = "bottom")



Newdata17_Flower <- expand.grid(Temp_total.cen = seq(-0.8, 2.5, length = 100), MeanFlower.cen = mean(dat17$MeanFlower.cen), Seed_mass = 0, Snowmelt_doy = mean(dat16$Snowmelt_doy))

Flower17 <- make_prediction(Newdata17_Flower, sm_model_17_4) %>% 
  mutate(MeanFlowers = (MeanFlower.cen*sd(WeatherAndBiomass$MeanFlowers)+mean(WeatherAndBiomass$MeanFlowers))) %>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Flower17_plot <- make_prettyplot(dat = dat17, 
                                 Newdata = Flower17, 
                                 xaxis = Temp_total, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = "#666666",
                                 SE = SE,
                                 line_type = "dashed") +
  scale_color_manual(values = "#666666", name = "Mean flowers (count)") +
  scale_fill_manual(values = "#666666", name = "Mean flowers (count)") +
  ylim(-12, 0) +
  labs(x = "Cumulative temperature above 0 °C", y = "", title = "2017") +
  theme(legend.position = "none")

Flower_Comb <- ggarrange(Flower16_plot, Flower17_plot)
ggsave(Flower_Comb, filename = "Figures/Flower_Comb.jpeg", height = 6, width = 8)

SeedMassPlot <- ggarrange(ggarrange(Treatment16_plot, Treatment17_plot, ncol = 2, labels = "a", common.legend = TRUE, legend = "bottom"),
  ggarrange(Flower16_plot, Flower17_plot, ncol = 2, labels = "b"),
  nrow = 2)
ggsave(SeedMassPlot, filename = "Figures/SeedMassPlot.jpeg", height = 6, width = 8)

#### Seed number
library(rioja)
library(stats)

#Abiotic
SN_Ab <- expand.grid(Temp_total.cen = seq(-1.8, 0.3, length = 100), Seed_potential = 0, Snowmelt_doy.cen = c(0.3, 1.68), siteID = "E 01")


SN16 <- make_glmer_prediction(SN_Ab, sp_model_d) %>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total))) %>% 
  mutate(Snowmelt_doy = (Snowmelt_doy.cen*sd(dat_DOY$Snowmelt_doy)+mean(dat_DOY$Snowmelt_doy)))

SN16_plot <- make_glmer_plot(dat = dat16, 
                xaxis = Temp_total, 
                yaxis = Seed_potential, 
                Newdata = SN16, 
                prediction = pred, 
                ColorVariable = as.factor(Snowmelt_doy), 
                plo = plo, 
                phi = phi) +
  scale_color_manual(values = c("#FFCC66", "#990000"), name = "Timing of snowmelt (DOY)", label = c("early (170)", "late (190)")) +
  scale_fill_manual(values = c("#FFCC66", "#990000"), name = "Timing of snowmelt (DOY)", label = c("early (170)", "late (190)")) +
  labs(x = "Cumulative temperature above 0 (°C)", y = "Seed number/Total ovule number", title = "") +
  theme(legend.position = "bottom")

ggsave(SN16_plot, filename = "Figures/SN16_plot.jpeg", height = 6, width = 8)



#FLower

SN16_Flower <- expand.grid(Temp_total.cen = seq(-1.8, 0.3, length = 100), MeanFlower.cen = c(-0.778, 1.92), Seed_potential = 0, Snowmelt_doy.cen = mean(dat16$Snowmelt_doy.cen), siteID = "E 01")

SNFlower16 <- make_glmer_prediction(SN16_Flower, sp_model3) %>% 
  mutate(MeanFlowers = (MeanFlower.cen*sd(WeatherAndBiomass$MeanFlowers)+mean(WeatherAndBiomass$MeanFlowers))) %>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Flower16_plot <- make_glmer_plot(dat = dat16, 
                                 Newdata = SNFlower16, 
                                 xaxis = Temp_total, 
                                 yaxis = Seed_potential, 
                                 prediction = pred, 
                                 ColorVariable = as.factor(MeanFlowers),
                                 plo = plo, 
                                 phi = phi) +
  scale_color_manual(values = c("#6699CC", "#000033"), name = "Mean flowers (count)", labels = c("low (400)", "high (6500)")) +
  scale_fill_manual(values = c("#6699CC", "#000033"), name = "Mean flowers (count)", labels = c("low (400)", "high (6500)")) +
  labs(x = "Cumulative temperature above °C", y = "Seed number/Total ovule number", title = "") +
  theme(legend.position="bottom")

#Visits
SN16_Visit <- expand.grid(Temp_total.cen = seq(-1.8, 0.3, length = 100), MeanVisit.cen = c(-0.4, 2.45), Seed_potential = 0, Snowmelt_doy.cen = mean(dat16$Snowmelt_doy.cen), siteID = "E 01")

SNVisit16 <- make_glmer_prediction(SN16_Visit, sp_model5) %>% 
  mutate(Temp_total = (Temp_total.cen * sd(dat_DOY$Temp_total) + mean(dat_DOY$Temp_total))) %>% 
  mutate(MeanVisit = (MeanVisit.cen * sd(WeatherAndBiomass$MeanVisit) + mean(WeatherAndBiomass$MeanVisit)))

Visit16_plot <- make_glmer_plot(dat = dat16, 
                                 Newdata = SNVisit16, 
                                 xaxis = Temp_total, 
                                 yaxis = Seed_potential, 
                                 prediction = pred, 
                                 ColorVariable = as.factor(MeanVisit),
                                 plo = plo, 
                                 phi = phi) +
  scale_color_manual(values = c("#FF9999", "#CC3366"), name = "Visitation (rate)", label = c("low (0.005)", "high (0.025)")) +
  scale_fill_manual(values = c("#FF9999", "#CC3366"), name = "Visitation (rate)", label = c("low (0.005)", "high (0.025)")) +
  labs(x = "Cumulative temperature above 0 °C", y = "Seed number/Total ovule number", title = "") +
  theme(legend.position="bottom")

FlVis <- ggarrange(Flower16_plot, Visit16_plot)
ggsave(FlVis, filename = "Figures/FlVis.jpeg", height = 6, width = 8)


#Treatment
SN16_Treat <- expand.grid(Temp_total.cen = seq(-1.8, 0.3, length = 100), Treatment = c("Control", "Pollinated"), Seed_potential = 0, Snowmelt_doy.cen = mean(dat16$Snowmelt_doy.cen), siteID = "E 01")

SNTreat16 <- make_glmer_prediction(SN16_Treat, sp_model4) %>% 
  mutate(Temp_total = (Temp_total.cen*sd(dat_DOY$Temp_total)+mean(dat_DOY$Temp_total)))

Treat16_plot <- make_glmer_plot(dat = dat16, 
                                 Newdata = SNTreat16, 
                                 xaxis = Temp_total, 
                                 yaxis = Seed_potential, 
                                 prediction = pred, 
                                 ColorVariable = Treatment, 
                                 ColorDot = Treatment,
                                 plo = plo, 
                                 phi = phi) +
  scale_color_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#99CC99", "#003300"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Cumulative temperature above 0 °C", y = "Seed number/Total ovule number", title = "") +
  theme(legend.position="bottom")



SN16_Treat2 <- expand.grid(Temp_total.cen = mean(dat16$Temp_total.cen), Treatment = c("Control", "Pollinated"), Seed_potential = 0, Snowmelt_doy.cen = seq(0.1, 2.1, length = 100), siteID = "E 01")

SNTreat16_2 <- make_glmer_prediction(SN16_Treat2, sp_model4) %>% 
  mutate(Snowmelt_doy = (Snowmelt_doy.cen*sd(dat_DOY$Snowmelt_doy)+mean(dat_DOY$Snowmelt_doy)))

Treat16_2_plot <- make_glmer_plot(dat = dat16, 
                                 Newdata = SNTreat16_2, 
                                 xaxis = Snowmelt_doy, 
                                 yaxis = Seed_potential, 
                                 prediction = pred, 
                                 ColorVariable = Treatment,
                                 plo = plo, 
                                 phi = phi) +
  scale_color_manual(values = c("#003300", "#99CC99"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#003300", "#99CC99"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Timing of snowmelt (DOY)", y = "Seed number/Total ovule number", title = "") +
  theme(legend.position="bottom")

TreatmentSN <- ggarrange(Flower16_plot, Flower16_2_plot, common.legend = TRUE, legend="bottom")
ggsave(TreatmentSN, filename = "Figures/TreatmentSN.jpeg", height = 6, width = 8)


SeedNumberPlot <- ggarrange(ggarrange(Treat16_plot, Treat16_2_plot, ncol = 2, labels = "a", common.legend = TRUE, legend = "bottom", label.x = 0.1),
                          ggarrange(Flower16_plot, Visit16_plot, ncol = 2, labels = c("b", "c"), label.x = 0.1),
                          nrow = 2)
ggsave(SeedNumberPlot, filename = "Figures/SeedNumberPlot.jpeg", height = 6, width = 8)



dat16 %>% 
  ggplot(aes(x = Seed_mass, y = Seed_potential, color = Snowmelt_doy)) +
  geom_point()
