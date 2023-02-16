## Mixed linear models

source("R/Linns paper/1_ImportData.R")
library(lme4)
library(broom.mixed)
library(performance)
library(MuMIn)
library(nlme)

#rescale
d1 <- as_tibble(x = scale(WeatherAndBiomass$CumTemp))
d2 <- as_tibble(x = scale(WeatherAndBiomass$MeanFlowers))

WeatherAndBiomass <- WeatherAndBiomass %>% 
  bind_cols(d1, d2) %>% 
  rename(CumTemp.cen = V1...29, MeanFlower.cen = V1...30)

SeedMass16 <- WeatherAndBiomass %>% 
  filter(Year == 2016)
SeedMass17 <- WeatherAndBiomass %>% 
  filter(Year == 2017)

hist(log(WeatherAndBiomass$Seed_potential), breaks = 20)

#models for dredge
SeedMass_1 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = SeedMass16, REML = FALSE)

SeedMass_2 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = SeedMass17, REML = FALSE)

SeedPotential_1 <- glm(Seed_potential ~ Biomass + Stage + Treatment +  MeanFlower.cen + CumTemp.cen, family = binomial, data = SeedMass16) #removed block as random effect

#includes p-values 
SeedMass_3 <- lme(log(Seed_mass) ~ Biomass + Stage + Treatment +  MeanFlower.cen + CumTemp.cen, data = SeedMass16, random = ~ 1|BlockID)

SeedMass_4 <- lme(log(Seed_mass) ~ Biomass + Stage + Treatment +  MeanFlower.cen + CumTemp.cen, data = SeedMass17, random = ~ 1|BlockID)

summary(SeedMass_1)

#to use in ggplot (not correct)
SeedMass_ny <- lmer(log(Seed_mass) ~ Biomass + Stage + (1|BlockID), data = SeedMass16)
output16 <- augment(SeedMass_ny)

SeedMass_OP17 <- lmer(log(Seed_mass) ~ Biomass + Stage + (1|BlockID), data = SeedMass17)
output17 <- augment(SeedMass_OP17)

#model averaging
options(na.action = "na.fail")

#windows(width = 12, height = 10) #make plot window larger
check_model(SeedMass_2)

model.setSM16 <- dredge(SeedMass_1, rank = "AICc", extra = "R^2")

model.setSM17 <- dredge(SeedMass_2, rank = "AICc", extra = "R^2")


####### 2016 ############

#Make a new dataframe
mmSM16 <- data.frame(model.setSM16)
mmSM16$cumsum <- cumsum(mmSM16$weight)
mmSM16

#Sekect the 95% confidence interval
mmSM1695 <- mmSM16 %>% filter(cumsum < 0.95)
mmSM1695 
#Gives us 17 models

#Model averaging based on AIC values
averaged.modelSM16 <- model.avg(model.setSM16, cumsum(weight) <= 0.95)
averaged.modelSM16

#Getting the table to present
resSM16 <- data.frame(summary(averaged.modelSM16)$coefmat.full)
resSM16

resSM162 <- resSM16 %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Category = plyr::mapvalues(Category, c("Intercept", "Biomass", "StageM", "StageL", "CumTemp.cen", "TreatmentPollinated", "MeanFlower.cen", "CumPrec.cen"), c("Stage E", "Biomass", "Stage: mid", "Stage: late", "Cumulative temperature", "Treatment: hand pollinated", "Phenology", "Cumulative precipitation"))) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "Biomass", "StageM", "StageL", "CumTemp.cen", "TreatmentPollinated", "MeanFlower.cen", "CumPrec.cen"), c("Stage E", "Biomass", "Stage: mid", "Stage: late", "Cumulative temperature", "Treatment: hand pollinated", "Phenology", "Cumulative precipitation"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  #mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI.low, CI.high, Zvalue, Pvalue)
resSM162

########## 2017 ############
#Making a dataframe
mmSM17 <- data.frame(model.setSM17)
mmSM17$cumsum <- cumsum(mmSM17$weight)
mmSM17

#Choosing the 95 % confidence set of model
mmSM1795 <- mmSM17 %>% filter(cumsum < 0.95)
mmSM1795
#Gives us 9 models

#Model averaging
averaged.modelSM17 <- model.avg(model.setSM17, cumsum(weight) <= 0.95)
averaged.modelSM17

#Results to present
resSM17 <- data.frame(summary(averaged.modelSM17)$coefmat.full)
resSM17

resSM173 <- resSM17 %>%
  rownames_to_column(var = "Variable") %>%
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>%
  select(-AdjSE) %>%
  mutate(Category = Variable) %>%
  mutate(Category = plyr::mapvalues(Category, c("Intercept", "Biomass", "CumTemp.cen", "MeanFlower.cen", "StageE", "StageM", "TreatmentPollinated"), c("Stage E", "Biomass", "Cumulative temperature", "Phenology", "Stage: mid", "Stage: late", "Treatment: hand pollinated"))) %>%
  mutate(Variable = plyr::mapvalues(Variable, c("Intercept", "Biomass", "CumTemp.cen", "MeanFlower.cen", "StageE", "StageM", "TreatmentPollinated"), c("Stage E", "Biomass", "Cumulative temperature", "Phenology", "Stage: mid", "Stage: late", "Treatment: hand pollinated"))) %>%
  mutate(CI.low = Estimate - 1.96 * StError) %>%
  mutate(CI.high = Estimate + 1.96 * StError) %>%
  mutate(Estimate = round(Estimate, 2), CI.low = round(CI.low, 2), CI.high = round(CI.high, 2), Zvalue = round(Zvalue, 2), Pvalue = round(Pvalue, 3)) %>%
  #mutate(CI = paste(CI.low, CI.high, sep = " - ")) %>%
  select(Variable, Estimate, CI.low, CI.high, Zvalue, Pvalue)
resSM173





#use results from dredge in paper
#in figure, make ring outside dot that is not significant, color dots that are significant
#figur stage: early, mid late bare 2016 data, mens for 2017 har vi stage f, men ingenting på late. to overlapper, to er ulike (earlier stage, later stage)
