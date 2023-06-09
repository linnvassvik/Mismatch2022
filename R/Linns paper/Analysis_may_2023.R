source("R/Linns paper/1_ImportData.R")

### New analysis
library(lme4)
library(MuMIn)
library(broom.mixed)
library(performance)
library(nlme)
library(ggplot2)

#rescale
d1 <- as_tibble(x = scale(WeatherAndBiomass$CumTemp))
d2 <- as_tibble(x = scale(WeatherAndBiomass$MeanFlowers))

WeatherAndBiomass <- WeatherAndBiomass %>% 
  bind_cols(d1, d2) %>% 
  rename(CumTemp.cen = V1...29, MeanFlower.cen = V1...30)

# prep data
dat <- WeatherAndBiomass |> 
  mutate(Stage2 = case_when(Stage == "F" ~ 1,
                            Stage == "E" ~ 2,
                            Stage == "M" ~ 3,
                            TRUE ~ 4),
         Stage = factor(Stage, levels = c("F", "E", "M", "L")),
         Stage2 = factor(Stage2, levels = c("1", "2", "3", "4"))) |> 
  # make sure that Control Treatment comes first
  mutate(Treatment = factor(Treatment, levels = c("Control", "Pollinated")))


### Seed mass 2016 - 2017
# fit full model
# options(na.action = "na.fail")
# fit <- lmer(log(Seed_mass) ~ Biomass*Year + Stage + MeanFlower.cen + CumTemp.cen + Treatment + (1|BlockID), data = dat, REML = FALSE)
# 
# drop1(fit, test = "Chisq")
# 
# # Likelihood ratio test
# model_selection <- dredge(fit, rank = "AICc", extra = "R^2")
# 
# 
# fit <- lmer(log(Seed_mass) ~ Biomass*Year + Stage + MeanFlower.cen + CumTemp.cen + Treatment + (1|BlockID), data = dat)
# fit1 <- lmer(log(Seed_mass) ~ Biomass+Year + Stage + MeanFlower.cen + CumTemp.cen + Treatment + (1|BlockID), data = dat)
# fit2 <- lmer(log(Seed_mass) ~ Biomass*Year + MeanFlower.cen + CumTemp.cen+ Treatment + (1|BlockID), data = dat)
# fit3 <- lmer(log(Seed_mass) ~ Biomass*Year + Stage + CumTemp.cen + Treatment + (1|BlockID), data = dat)
# fit4 <- lmer(log(Seed_mass) ~ Biomass*Year + Stage + MeanFlower.cen + Treatment + (1|BlockID), data = dat)
# fit5 <- lmer(log(Seed_mass) ~ Biomass*Year + Stage + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = dat)
# fit5 <- lmer(log(Seed_mass) ~ Biomass*Year + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = dat)
# anova(fit, fit5)

sm_model1 <- lmer(log(Seed_mass) ~ Biomass*Year + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment + Treatment + (1|BlockID), data = dat)
sm_model2 <- lme(log(Seed_mass) ~ Biomass*Year + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment, random = ~ 1|BlockID, data = dat)
summary(sm_model2)
check_model(sm_model)
out <- dat |> 
  bind_cols(augment(sm_model) |> 
              select(.fitted))

augment(sm_model) |> 
  filter(Biomass == "0.016")


####################### LINN ##################

dat |>  
  ggplot(aes(y = CumTemp, x = factor(Stage2)), color = factor(Stage2)) +
  geom_boxplot()
  geom_smooth(method = "lm") +
  geom_point()


##########################

dat |>  
  ggplot(aes(x = Biomass, y = log(Seed_mass), colour = factor(Stage2), fill = factor(Stage2), shape = factor(Year), linetype = factor(Year))) +
  geom_point() +
  #geom_smooth(method = "lm") +
  geom_line(data = out, mapping = aes(x = Biomass, y = .fitted, colour = factor(Stage2))) +
  scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_shape_manual(values = c(16, 1), name = "Year") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Year") +
  labs(x = "Biomass in g", y = "log(Seed mass in g)") +
  facet_wrap(~ Year) +
  theme_bw()


dat |>  
  ggplot(aes(x = CumTemp, y = log(Seed_mass), colour = factor(Stage2), shape = factor(Year), linetype = factor(Year))) +
  geom_point() +
  geom_line(data = out, mapping = aes(x = CumTemp, y = .fitted, colour = factor(Stage2))) +
  scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_shape_manual(values = c(16, 0), name = "Year") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Year") +
  labs(x = "Cumulative temperature in °C", y = "log(Seed mass in g)") +
  theme_bw()


dat |> 
  ggplot(aes(x = factor(Stage2), y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin(draw_quantiles = c(0.5)) +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1) +
  facet_wrap(~ Year)  +
  theme_bw()


dat |> 
  filter(Year == 2016) |> 
  ggplot(aes(x = factor(Stage2), y = Seed_potential, fill = factor(Stage2))) +
  geom_violin(draw_quantiles = c(0.5)) +
  scale_fill_viridis_d(option = "inferno", begin = 0.2, end = 0.8, direction = -1) +
  labs(x = "Tiing of snowmelt", y = "Seed:ovule ratio") +
  facet_wrap(~ Year)  +
  theme_bw()




### Seed mass 2016 - 2017
# fit full model
fit <- glmer(Seed_potential ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + (1|BlockID), data = dat |> filter(Year == 2016), family = binomial)
fit <- glm(Seed_potential ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment  , data = dat |> filter(Year == 2016), family = binomial)
summary(fit)

ggplot(dat, aes(x = Treatment, y = Seed_potential, colour = factor(Stage2))) +
  geom_boxplot()

# Likelihood ratio test
dredge(fit, rank = "AICc", extra = "R^2")

sm_model <- lmer(log(Seed_mass) ~ Biomass*Year + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment + (1|BlockID), data = dat)
summary(sm_model)
prediction <- augment(sm_model)

dat |>  
  ggplot(aes(x = Biomass, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_line(data = prediction, mapping = aes(x = Biomass, y = .fitted, colour = factor(Stage2))) +
  scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  labs(x = "Biomass in g", y = "log(Seed mass in g)") +
  facet_wrap(~Year) +
  theme_bw()
