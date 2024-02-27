source("R/Linns paper/1_Import_Data.R")
source("R/Linns paper/MyFunctions.R")

library(lme4)
library(nlme)
library(broom.mixed)
library(performance)
library(patchwork)
library(readxl)
library(ggplot2)
library(ggpubr)

### Seed mass 2016
dat16 <- dat_DOY |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat_DOY |> 
  filter(Year == 2017)


#### SEED MASS ###
##
#What determines when plants reach peak flower 
sm_model_16_1 <- glmer(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_Peak.cen * DOY_sinceSM.cen + (1|siteID), family = poisson, data = dat16)
sm_model_16_1b <- glm(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_Peak.cen * DOY_sinceSM.cen, family = poisson, data = dat16)

sm_model_17_1 <- glmer(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_Peak.cen * DOY_sinceSM.cen + (1|siteID), family = poisson, data = dat17)
sm_model_17_1b <- glm(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_Peak.cen * DOY_sinceSM.cen, family = poisson, data = dat17)

#plot peak flower x snowmelt doy and mean(temp), endre temperatur til snowmelt->peakdoy

summary(sm_model_17_1b)
check_model(sm_model_17)

##
#What determines plants seed mass production
#With visit data run this dataset
dat16a <- dat16 %>% 
  filter(!is.na(MeanVisit.cen))

sm_model_16_2 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * (MeanFlower.cen + Treatment + MeanVisit.cen), random =  ~ 1|siteID, data = dat16a) 
sm_model_16_3 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * (MeanFlower.cen + Treatment), random =  ~ 1|siteID, data = dat16) 
sm_model_16_4 <- lme(log(Seed_mass) ~ Temp_total.cen * MeanFlower.cen * Treatment, random =  ~ 1|siteID, data = dat16) 

sm_model_16_5 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen , random =  ~ 1|siteID, data = dat16) 

summary(sm_model_16_3)


sm_model_17_2 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * (MeanFlower.cen + Treatment + MeanVisit.cen), random =  ~ 1|siteID, data = dat17)
sm_model_17_3 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * (MeanFlower.cen + Treatment), random =  ~ 1|siteID, data = dat17)
sm_model_17_4 <- lme(log(Seed_mass) ~ Temp_total.cen * MeanFlower.cen * Treatment, random =  ~ 1|siteID, data = dat17)

sm_model_17_4 <- lme(log(Seed_mass) ~ Temp_total.cen * Snowmelt_doy.cen, random =  ~ 1|siteID, data = dat17)

summary(sm_model_17_2)




### SEED NUMBER ###
#What determines plants seed number production
dat16_2 <- dat16 %>% 
  group_by(BlockID, Year, Plant) %>% 
  mutate(Number_seedovule = (Seed_number + Ovule_number)) %>% 
  ungroup()

#Which model?
sp_model_a <- glmer(Seed_number ~ Snowmelt_doy.cen * Temp_total.cen + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_2)

sp_model_b <- lme(Seed_number ~ Snowmelt_doy.cen * Temp_total.cen, random = ~ 1|siteID, data = dat16_2)

sp_model_c <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy.cen * Temp_total.cen + (1|siteID), family = binomial(link = "logit"), data = dat16_2)

summary(sp_model_c)

#Which biotic factors
sp_model5 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy.cen * Temp_total.cen * (MeanFlower.cen + Treatment) + (1|siteID), family = binomial(link = "logit"), data = dat16_2)

sp_model6 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy.cen * Temp_total.cen * Treatment + (1|siteID), family = binomial(link = "logit"), data = dat16_2)

dat16_2a <- na.omit(dat16_2)
sp_model7 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy.cen * Temp_total.cen * MeanVisit.cen + (1|siteID), family = binomial(link = "logit"), data = dat16_2a)

summary(sp_model7)




########## FIGURES ###################
##Figures Seed Mass
Peak_16a <- expand.grid(Snowmelt_doy.cen = c(-0.6, 2.2), CumTemp_Peak.cen = c(-1.9, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.6), PeakFlower_doy = 0)

Peak16a <- make_prediction(Peak_16a, sm_model_16_1b) 

TempSnow16b_plot <- make_prettyplot(dat = dat16, 
                                    Newdata = Peak16a, 
                                    xaxis = CumTemp_Peak.cen, 
                                    yaxis = PeakFlower_doy, 
                                    prediction = pred, 
                                    ColorVariable = as.factor(Snowmelt_doy.cen),
                                    SE = SE) +
  scale_color_manual(values = c("#E69F00", "#336666"), name = "doy") +
  scale_fill_manual(values = c("#E69F00", "#336666"), name = "doy") +
  labs(x = "Temperature", y = "Peak Flower doy", title = "2016") +
  theme_minimal() 

## Temperature and Snowmelt affects seed mass
#2016
#Doy on x
Temp_doy_16a <- expand.grid(Snowmelt_doy.cen = c(-0.7, -0.6, -0.4, -0.2, 0, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2), Temp_total.cen = c(-2, 2), Seed_mass = 0, MeanVisit.cen = mean(dat16a$MeanVisit.cen), MeanFlower.cen = mean(dat16a$MeanFlower.cen), Treatment = c("Control", "Pollinated"))

TempSnow16a <- make_prediction(Temp_doy_16a, sm_model_16_2) 



TempSnow16a_plot <- make_prettyplot(dat = dat16a, 
                                      Newdata = TempSnow16a, 
                                      xaxis = Snowmelt_doy.cen, 
                                      yaxis = log(Seed_mass), 
                                      prediction = pred, 
                                      ColorVariable = as.factor(Temp_total.cen),
                                      SE = SE) +
  scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "2016") +
  theme_minimal()


#Temp on x
Temp_doy_16b <- expand.grid(Snowmelt_doy.cen = c(-0.6, 2.2), Temp_total.cen = c(-1.8, -1.4, -1.2, -1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.5), Seed_mass = 0, MeanFlower.cen = mean(dat16a$MeanFlower.cen), Treatment = c("Control", "Pollinated"), MeanVisit.cen = mean(dat16a$MeanVisit.cen))

TempSnow16b <- make_prediction(Temp_doy_16b, sm_model_16_2)

TempSnow16b_plot <- make_prettyplot(dat = dat16a, 
                                    Newdata = TempSnow16b, 
                                    xaxis = Temp_total.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = as.factor(Snowmelt_doy.cen),
                                    SE = SE) +
  scale_color_manual(values = c("#E69F00", "#336666"), name = "doy") +
  scale_fill_manual(values = c("#E69F00", "#336666"), name = "doy") +
  labs(x = "Temperature", y = "log(Seed mass in g)", title = "2016") +
  theme_minimal() 

#2017
#doy on x
Temp_doy_17a <- expand.grid(Snowmelt_doy.cen = c(-2.4, -2.2, -2, -1.8, -1.6, -1.4, -1.2, -1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), Temp_total.cen = c(-2,2), Seed_mass = 0)

TempSnow17a <- make_prediction(Temp_doy_17a, sm_model_17_2)

TempSnow17a_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = TempSnow17a, 
                                    xaxis = Snowmelt_doy.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = as.factor(Temp_total.cen),
                                    SE = SE) +
  scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "2017") +
  theme_minimal()

#Temp on x
Temp_doy_17b <- expand.grid(Snowmelt_doy.cen = c(-2.4, -1.4, 0, 0.4, 1.4), Temp_total.cen = c(-0.8, 2.3), Seed_mass = 0, MeanFlower.cen = mean(dat17$MeanFlower.cen), Treatment = "Pollinated", "Control")

TempSnow17b <- make_prediction(Temp_doy_17b, sm_model_17_14)

TempSnow17b_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = TempSnow17b, 
                                    xaxis = Snowmelt_doy.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = as.factor(Temp_total.cen),
                                    SE = SE) +
  scale_color_manual(values = c("#E69F00", "#336666")) +
  scale_fill_manual(values = c("#E69F00", "#336666")) +
  labs(x = "Temperature", y = "log(Seed mass in g)", title = "2017") +
  theme_minimal() +
  theme(legend.position = "none")


Temp_snowmelt <- ggarrange(TempSnow16a_plot, TempSnow17a_plot, common.legend = TRUE) 
ggsave(Temp_snowmelt, filename = "Figures/Temp_snowmelt.jpeg", height = 6, width = 8)

Temp_snowmelt_2 <- ggarrange(TempSnow16b_plot, TempSnow17b_plot) 
ggsave(Temp_snowmelt_2, filename = "Figures/Temp_snowmelt_2.jpeg", height = 6, width = 8)


## Biotic factors affect seed mass

#Treatment
#2016
Newdata16_treatment <- expand.grid(Snowmelt_doy.cen = mean(dat16a$Snowmelt_doy.cen), Temp_total.cen = c(-1.8,-1.5, -1, -0.5, 0, 0.4), Seed_mass = 0, Treatment = c("Control", "Pollinated"), MeanFlower.cen = mean(dat16a$MeanFlower.cen), MeanVisit.cen = mean(dat16a$MeanVisit.cen))

Treatment16 <- make_prediction(Newdata16_treatment, sm_model_16_2)

Treatment16_plot <- make_prettyplot(dat = dat16a,
                                    Newdata = Treatment16, 
                                    xaxis = Temp_total.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = Treatment,
                                    SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Temperature", y = "log(Seed mass in g)", title = "2016")

#2017
Newdata17_treatment <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy.cen), Temp_total.cen = c(-0.8, -0.5, 0, 0.5, 1, 1.5, 2, 2.5), Seed_mass = 0, Treatment = c("Control", "Pollinated"), MeanFlower.cen = mean(dat17$MeanFlower.cen), MeanVisit.cen = mean(dat17$MeanVisit.cen))

Treatment17 <- make_prediction(Newdata17_treatment, sm_model_17_2)

Treatment17_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = Treatment17, 
                                    xaxis = Temp_total.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = Treatment,
                                    SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "", labels = c("Control", "Supplement pollen")) +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "", labels = c("Control", "Supplement pollen")) +
  labs(x = "Temperature", y = "", title = "2017")

TreatmentPlot <- ggarrange(Treatment16_plot, Treatment17_plot, common.legend = TRUE) 
ggsave(TreatmentPlot, filename = "Figures/TreatmentPlot.jpeg", height = 6, width = 8)


#Visit and Flower (a bit weird)
#All 2017 only
Newdata17_VisitFlower <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy.cen), Temp_total.cen = mean(dat17$Temp_total.cen), MeanVisit.cen = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2), Seed_mass = 0, MeanFlower.cen = c(-0.5, 2))

VisitFlower17 <- make_prediction(Newdata17_VisitFlower, sm_model_17_13)

VisitFlower17_plot <- make_prettyplot(dat = dat17, 
                                      Newdata = VisitFlower17, 
                                      xaxis = MeanVisit.cen, 
                                      yaxis = log(Seed_mass), 
                                      prediction = pred, 
                                      ColorVariable = as.factor(MeanFlower.cen),
                                      SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  labs(x = "Mean flower abundance", y = "log(Seed mass in g)", title = "")
ggsave(VisitFlower17_plot, filename = "Figures/VisitFlower17_plot.jpeg", height = 6, width = 8)

#Mean flower only
Newdata17_Flower <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy.cen), Temp_total.cen = mean(dat17$Temp_total.cen), MeanFlower.cen = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), Seed_mass = 0, Treatment = c("Pollinated", "Control"), MeanVisit.cen = mean(dat17$MeanVisit.cen))

Flower17 <- make_prediction(Newdata17_Flower, sm_model_17_2)

Flower17_plot <- make_prettyplot(dat = dat17, 
                                 Newdata = Flower17, 
                                 xaxis = MeanFlower.cen, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = "#336666",
                                 SE = SE) +
  scale_color_manual(values = c("#336666"), name = "") +
  scale_fill_manual(values = c("#336666"), name = "") +
  labs(x = "Mean flower abundance", y = "log(Seed mass) in g", title = "2017") +
  theme(legend.position = "none")
ggsave(Flower17_plot, filename = "Figures/Flower17_plot.jpeg", height = 6, width = 8)


#Mean visit rate only
#2016
Newdata16_Visit <- expand.grid(Snowmelt_doy.cen = c(-0.5, 2), Temp_total.cen = c(-1.8, -1.4, -1.2, -1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), MeanVisit.cen = mean(dat16a$MeanVisit.cen), Seed_mass = 0)

Visit16 <- make_prediction(Newdata16_Visit, sm_model_16_12)

Visit16_plot <- make_prettyplot(dat = dat16a, 
                                Newdata = Visit16, 
                                xaxis = Temp_total.cen, 
                                yaxis = log(Seed_mass), 
                                prediction = pred, 
                                ColorVariable = as.factor(Snowmelt_doy.cen),
                                SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  labs(x = "Temperature", y = "log(Seed mass) in g", title = "2016")


Newdata16b_Visit <- expand.grid(Snowmelt_doy.cen = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2), Temp_total.cen = mean(dat16a$Temp_total.cen), MeanVisit.cen = c(-1, 2.9), Seed_mass = 0)

Visit16b <- make_prediction(Newdata16b_Visit, sm_model_16_12)

Visit16b_plot <- make_prettyplot(dat = dat16a, 
                                Newdata = Visit16b, 
                                xaxis = Snowmelt_doy.cen, 
                                yaxis = log(Seed_mass), 
                                prediction = pred, 
                                ColorVariable = as.factor(MeanVisit.cen),
                                SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  labs(x = "doy", y = "log(Seed mass) in g", title = "2016") 

#2017
Newdata17_Visit <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy.cen), Temp_total.cen = mean(dat17$Temp_total.cen), MeanVisit.cen = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2), Seed_mass = 0)

Visit17 <- make_prediction(Newdata17_Visit, sm_model_17_12)

Visit17_plot <- make_prettyplot(dat = dat17, 
                                 Newdata = Visit17, 
                                 xaxis = MeanVisit.cen, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = "#336666",
                                 SE = SE) +
  scale_color_manual(values = "#336666") +
  scale_fill_manual(values = "#336666") +
  labs(x = "Visitation rate", y = "", title = "Visitation rate 2017") +
  theme(legend.position = "none")

FlowerVisitPlot <- ggarrange(Visit17_plot, Flower17_plot) 
ggsave(FlowerVisitPlot, filename = "Figures/FlowerVisitPlot17.jpeg", height = 6, width = 8)



### SEED NUMBER 2016 only

#Abiotic

#Biotic
