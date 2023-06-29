source("R/Linns paper/1_Import_Data.R")

library(lme4)

### Seed mass 2016
dat16 <- dat |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat |> 
  filter(Year == 2017)

#Run models without biomass

sm_model_16 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16)
summary(sm_model_16)

sm_model_17 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17)
summary(sm_model_17)

#When removing biomass 2016 ends up with stage as sifnificant (more than last time), and 2017 with flower abundance (more than before) and temp (same importance)

#2016 seperate per stage and year
dat16_S2 <- dat |> 
  filter(Stage2 == 2)

sm_model_16_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S2)
summary(sm_model_16_S2)

dat16_S3 <- dat |> 
  filter(Stage2 == 3)

sm_model_16_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S3)
summary(sm_model_16_S3)

dat16_S4 <- dat |> 
  filter(Stage2 == 4)

sm_model_16_S4 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S4)
summary(sm_model_16_S4)

#2017 seperate per stage and year
dat17_S1 <- dat |> 
  filter(Stage2 == 1)

sm_model_17_S1 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S1)
summary(sm_model_17_S1)

dat17_S2 <- dat |> 
  filter(Stage2 == 2)

sm_model_17_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S2)
summary(sm_model_17_S2)

dat17_S3 <- dat |> 
  filter(Stage2 == 3)

sm_model_17_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S3)
summary(sm_model_17_S3)

#Summary: Stage 1: Temp, 2: Flower abundance, 3: Temp + flower abundance, 4 : None (flower abundance = 0.053)


#Make temperature into two different variables. Before and after flowering