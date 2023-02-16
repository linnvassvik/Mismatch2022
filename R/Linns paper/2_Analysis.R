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
SeedMass_1 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment +  MeanFlower.cen + CumTemp.cen + (1|BlockID), data = SeedMass16)

SeedMass_2 <- lmer(log(Seed_mass) ~ Biomass + Stage + Treatment +  MeanFlower.cen + CumTemp.cen + (1|BlockID), data = SeedMass17)

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

windows(width = 12, height = 10) #make plot window larger
check_model(SeedMass_2)

model.setSM16 <- dredge(SeedMass_1, rank = "AICc", extra = "R^2")

#use results from dredge in paper
#in figure, make ring outside dot that is not significant, color dots that are significant
#figur stage: early, mid late bare 2016 data, mens for 2017 har vi stage f, men ingenting på late. to overlapper, to er ulike (earlier stage, later stage)
