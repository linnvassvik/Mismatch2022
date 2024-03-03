source("R/Linns paper/1_Import_Data.R")
source("R/Linns paper/MyFunctions.R")

library(lme4) #lmer and glmer
library(nlme) #lme and glme
library(broom.mixed)
library(performance)
library(patchwork)
library(readxl)
library(ggpubr)
library(DHARMa)

### Seed mass 2016
dat16 <- dat_DOY |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat_DOY |> 
  filter(Year == 2017)


#### SEED MASS ###
#What determines plants seed mass production
#With visit data run this dataset
sm_model_16_3 <- lme(log(Seed_mass) ~ Snowmelt_doy * Temp_total.cen , random =  ~ 1|siteID, data = dat16) 


sm_model_16_4 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanFlower.cen, random =  ~ 1|siteID, data = dat16)
sm_model_16_5 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanVisit.cen, random =  ~ 1|siteID, data = dat16)
sm_model_16_6 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * Treatment, random =  ~ 1|siteID, data = dat16)

summary(sm_model_16_3)



sm_model_17_3 <- lme(log(Seed_mass) ~ Temp_total.cen * Snowmelt_doy, random =  ~ 1|siteID, data = dat17)


sm_model_17_4 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanFlower.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_5 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanVisit.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_6 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * Treatment, random =  ~ 1|siteID, data = dat17)


summary(sm_model_17_6)




### SEED NUMBER ###
#What determines plants seed number production

#Which model?
sp_model_d <- glmer(Seed_potential ~ Snowmelt_doy.cen * Temp_total.cen + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)

summary(sp_model_d)

#Which biotic factors
sp_model3 <- glmer(Seed_potential ~ (Temp_total.cen + Snowmelt_doy.cen) * MeanFlower.cen + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)
#Temp, Flower and Temp * Flower

sp_model4 <- glmer(Seed_potential ~ (Temp_total.cen + Snowmelt_doy.cen) * Treatment + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)
#DOY, Treat, Temp*Treat, DOY*Treat

sp_model5 <- glmer(Seed_potential ~ (Temp_total.cen + Snowmelt_doy.cen) * MeanVisit.cen + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)
#Temp, Snow, Visit, Temp*Visit

summary(sp_model4)





#calculate peak flowering per stage per year
dat_calc <- dat_DOY %>%
  filter(Stage %in% c('E', 'M'))

PeakFl <- dat_calc %>%
  distinct(Stage,Year, PeakFlower_doy) %>% 
  group_by(Year) %>%
  summarize(mean_PeakFlower_doy = mean(PeakFlower_doy))

print(PeakFl)

PeakVis <- pollination2 %>%
  distinct(stage,year.poll, fly) %>% 
  group_by(year.poll) %>%
  summarize(fly_tot = sum(fly))

print(PeakVis)

t_test_result <- t.test(pollination2$fly ~ pollination2$year.poll)
print(t_test_result)

PeakVis2 <- pollination2 %>%
  distinct(stage, year.poll, fly) %>% 
  group_by(year.poll) %>%
  summarize(total_rows = n())

print(PeakVis2)

dat_collect <- dat_DOY %>% 
  distinct(Date1, Date2, Date3, Collected, Year, Stage) %>% 
  filter(!is.na(Date1) & !is.na(Date2) & !is.na(Date3) & !is.na(Collected) & !is.na(Year) & !is.na(Stage)) %>% 
  mutate(Diff = Collected-Date1) %>% 
  group_by(Year) %>%
  summarize(mean_SDDiff = mean(Diff))

print(dat_collect)

dat_mass <- dat_DOY %>% 
  distinct(Seed_mass, Year, Stage) %>% 
  group_by(Year) %>%
  summarize(mean_SM = mean(Seed_mass))

print(dat_mass)

t_test_result <- t.test(dat_DOY$Seed_mass ~ dat_DOY$Year)
print(t_test_result)


#20 butterflies