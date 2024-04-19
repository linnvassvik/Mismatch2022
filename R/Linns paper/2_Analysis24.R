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

sm_model_16_5b <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanFly.cen, random =  ~ 1|siteID, data = dat16)
# sm_model_16_5c <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * TotFly.cen, random =  ~ 1|siteID, data = dat16)

summary(sm_model_16_5b)

sm_model_16_7 <- lme(MeanFly ~ Temp_total.cen + Snowmelt_doy + I(Snowmelt_doy^2) + MeanFlower.cen, random =  ~ 1|siteID, data = dat16)
sm_model_16_7b <- lme(MeanFly ~ Temp_total.cen + Snowmelt_doy.cen + I(Snowmelt_doy.cen^2) + MeanFlower.cen, random =  ~ 1|siteID, data = dat16)
summary(sm_model_16_7b)
# sm_model_16_7 <- glmer(MeanFly ~ Temp_total.cen + Snowmelt_doy + I(Snowmelt_doy^2) + MeanFlower.cen + (1|siteID), family = 'poisson', data = dat16)


# hist(dat16$MeanFly)
# qqnorm(resid(sm_model_16_7))
# qqline(resid(sm_model_16_7))
# 
# 
# # library(DHARMa)
# testDispersion(sm_model_16_7)
# simulationOutput <- simulateResiduals(fittedModel = sm_model_16_7, plot = F)
# residuals(simulationOutput)
# residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
# plot(simulationOutput)
# 
# 





sm_model_17_3 <- lme(log(Seed_mass) ~ Temp_total.cen * Snowmelt_doy, random =  ~ 1|siteID, data = dat17)


sm_model_17_4 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanFlower.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_5 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanVisit.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_5b <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * MeanFly.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_6 <- lme(log(Seed_mass) ~ (Temp_total.cen + Snowmelt_doy) * Treatment, random =  ~ 1|siteID, data = dat17)



summary(sm_model_17_5b)




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
sp_model5b <- glmer(Seed_potential ~ (Temp_total.cen + Snowmelt_doy.cen) * MeanFly.cen + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)
sp_model5c <- glmer(Seed_potential ~ (Temp_total.cen + Snowmelt_doy.cen) * TotFly.cen + (1|siteID), family = binomial(link = "logit"), weights = Tot_Ovule, data = dat16)
#Temp, Snow, Visit, Temp*Visit


sp_model6d <- lme(MeanFly ~ (Temp_total.cen + Snowmelt_doy + I(Snowmelt_doy^2)) * MeanFlower.cen, random =  ~ 1|siteID, data = dat16)


summary(sp_model5b)




# 
# #calculate peak flowering per stage per year
# dat_calc <- dat_DOY %>%
#   filter(Stage %in% c('E', 'M'))
# 
# PeakFl <- dat_calc %>%
#   distinct(Stage,Year, PeakFlower_doy) %>% 
#   group_by(Year) %>%
#   summarize(mean_PeakFlower_doy = mean(PeakFlower_doy))
# 
# print(PeakFl)
# 
# PeakVis <- pollination2 %>%
#   distinct(stage,year.poll, fly) %>%
#   group_by(year.poll) %>%
#   summarize(fly_tot = sum(fly))
# 
# print(PeakVis)
# 
# t_test_result <- t.test(pollination2$fly ~ pollination2$year.poll)
# print(t_test_result)
# 
# PeakVis2 <- pollination2 %>%
#   distinct(stage, year.poll, fly) %>%
#   group_by(year.poll) %>%
#   summarize(total_rows = n())
# 
# print(PeakVis2)
# 
# dat_collect <- dat_DOY %>% 
#   distinct(Date1, Date2, Date3, Collected, Year, Stage) %>% 
#   filter(!is.na(Date1) & !is.na(Date2) & !is.na(Date3) & !is.na(Collected) & !is.na(Year) & !is.na(Stage)) %>% 
#   mutate(Diff = Collected-Date1) %>% 
#   group_by(Year) %>%
#   summarize(mean_SDDiff = mean(Diff))
# 
# print(dat_collect)
# 
# dat_mass <- dat_DOY %>% 
#   distinct(Seed_mass, Year, Stage) %>% 
#   group_by(Year) %>%
#   summarize(mean_SM = mean(Seed_mass))
# 
# print(dat_mass)
# 
# t_test_result <- t.test(dat_DOY$Seed_mass ~ dat_DOY$Year)
# print(t_test_result)
# 
# 
# #20 butterflies