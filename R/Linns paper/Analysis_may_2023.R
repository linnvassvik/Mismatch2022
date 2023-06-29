### New analysis

source("R/Linns paper/1_Import_Data.R")

library(lme4)
library(MuMIn)
library(broom.mixed)
library(performance)
library(patchwork)

# correlation temp and prec
dat |> 
  ggplot(aes(x = CumPrec, y = CumTemp, colour = factor(Year))) +
  geom_point()

# temp vs stage
dat |> 
  ggplot(aes(x = factor(Stage2), y = CumTemp, fill = factor(Year))) +
  geom_boxplot()

### Seed mass 2016
dat16 <- dat |> 
  filter(Year == 2016)
sm_model_16_lrt <- lme(log(Seed_mass) ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment, random =  ~ 1|siteID, data = dat16, method = "ML")

# likelihood ratio test
drop1(sm_model_16_lrt)

# singular fit issues
#sm_model_16 <- lmer(log(Seed_mass) ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment + (1|siteID), data = dat16)




newdata <- crossing(Biomass = 0.1,
                    Stage2 = c(2, 3, 4),
                    MeanFlower.cen = seq(-0.933, 2.27, by = 0.05),
                    CumTemp.cen = 0,
                    Treatment = c("Control", "Pollinated"))

out <- augment(x = sm_model_16, newdata = newdata)
#out <- augment(x = sm_model_16, newdata = newdata, re.form = NA)
ggplot(dat16, aes(x = MeanFlower.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_line(aes(y = .fitted), out)


out16 <- dat16 |> 
  bind_cols(augment(sm_model_16) |> 
              select(.fitted))

### Seed mass 2017
dat17 <- dat |> 
  filter(Year == 2017)
sm_model_17_lrt <- lme(log(Seed_mass) ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment, random = ~ 1|siteID, data = dat17, method = "ML")
# likelihood ratio test
drop1(sm_model_17_lrt)

sm_model_17 <- lme(log(Seed_mass) ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment, random = ~ 1|siteID, data = dat17)
summary(sm_model_17)

bind_rows("2016" = tidy(sm_model_16), 
          "2017" = tidy(sm_model_17),
          .id = "Year") |> 
  filter(effect == "fixed") |> 
  select(Year, term:p.value) |> 
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 3)) |> 
  write_csv("results.csv")


sm_model_17 <- lme(log(Seed_mass) ~ MeanFlower.cen, random = ~ 1|siteID, data = dat17)
dat17 |> ungroup() |> summarise(mean(MeanFlower.cen),
                                min(MeanFlower.cen),
                                max(MeanFlower.cen))
newdata <- crossing(#Biomass = 0.1,
                    Stage2 = c(2, 3, 4),
                    MeanFlower.cen = seq(-0.676, 3.92, by = 0.05)#,
                    # CumTemp.cen = 0,
                    # Treatment = c("Control", "Pollinated")
                    )

out <- augment(x = sm_model_17, newdata = newdata)
ggplot(dat17, aes(x = MeanFlower.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_line(aes(y = .fitted), out)


### FIGURES

flower <- dat |>
  ggplot(aes(x = MeanFlower.cen, y = log(Seed_mass), colour = factor(Stage2), fill = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_shape_manual(values = c(16, 1), name = "Year") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Year") +
  labs(x = "Flower abundance (centered)", y = "log(Seed mass in g)") +
  facet_wrap(~ Year) +
  theme_bw()

temp <- dat |>
  ggplot(aes(x = CumTemp.cen, y = log(Seed_mass), colour = factor(Stage2), fill = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
  scale_shape_manual(values = c(16, 1), name = "Year") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Year") +
  labs(x = "Cumulative temperature (centered)", y = "log(Seed mass in g)") +
  facet_wrap(~ Year) +
  theme_bw()

flower/temp + plot_layout(guides = "collect")

# sm_16 <- lme(log(Seed_mass) ~ Biomass + Stage2, random = ~ 1|BlockID, data = dat16, method = "ML")
# 
# newdat <- expand.grid(
#   Biomass=c(0.00226, 0.0057, 0.0100, 0.0150, 0.0200, 0.0250, 0.0301, 0.0406, 0.0445, 0.0502, 0.0633, 0.097)
#   , Stage2=c("2", "3", "4")
#   , Seed_mass = 0
# )
# newdat$Seed_mass <- predict(sm_16, newdat, re.form=NA)
# mm <- model.matrix(terms(fm1),newdat)
# ## or newdat$distance <- mm %*% fixef(fm1)
# pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
# tvar1 <- pvar1+VarCorr(fm1)$Subject[1]  ## must be adapted for more complex models
# cmult <- 2 ## could use 1.96
# newdat <- data.frame(
#   newdat
#   , plo = newdat$distance-cmult*sqrt(pvar1)
#   , phi = newdat$distance+cmult*sqrt(pvar1)
#   , tlo = newdat$distance-cmult*sqrt(tvar1)
#   , thi = newdat$distance+cmult*sqrt(tvar1)
# )



# out <- bind_rows(out16, out17)
# dat |>
#   ggplot(aes(x = Biomass, y = log(Seed_mass), colour = Stage2, fill = factor(Stage2))) +
#   geom_point() +
#   #geom_smooth(method = "lm") +
#   #geom_line(data = out, mapping = aes(x = Biomass, y = .fitted)) +
#   geom_line(data = out, mapping = aes(x = Biomass, y = .fitted, colour = factor(Stage2))) +
#   scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
#   scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "Timing of snowmelt") +
#   scale_shape_manual(values = c(16, 1), name = "Year") +
#   scale_linetype_manual(values = c("solid", "dashed"), name = "Year") +
#   labs(x = "Biomass in g", y = "log(Seed mass in g)") +
#   facet_wrap(~ Year) +
#   theme_bw()



# factors
biomass <- dat |>
  ggplot(aes(x = factor(Stage2), y = Biomass, fill = factor(Stage2))) +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "") +
  labs(x = "", y = "Biomass in g") +
  facet_wrap(~ Year) +
  theme_bw() +
  theme(legend.position = "none")

stage <- dat |>
  ggplot(aes(x = factor(Stage2), y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "") +
  labs(x = "Timing of snowmelt", y = "log(Seed mass in g)") +
  facet_wrap(~ Year) +
  theme_bw() +
  theme(legend.position = "none")

pollination <- dat |>
  ggplot(aes(x = Treatment, y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "") +
  labs(x = "", y = "log(Seed mass in g)") +
  facet_wrap(~ Year) +
  theme_bw() +
  theme(legend.position = "none")


biomass / stage / pollination

www <- Weather %>% 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  mutate(tempAboveZero = if_else(temperature < 0, 0, temperature),
         CumTemp = cumsum(tempAboveZero))

ggplot(www, aes(x = doy, y = CumTemp)) +
  geom_point() +
  facet_wrap(~ year)



bind_rows(meta16, meta17) |> 
  select(Year, siteID, BlockID, Stage, Plant, Treatment, doy, MinDate, MaxDate, tempAboveZeroAdi) |> 
  group_by(Year, siteID, Stage, BlockID, Plant, Treatment) |> 
  mutate(cumtemp = cumsum(tempAboveZeroAdi),
         Stage2 = case_when(Stage == "F" ~ 1,
                            Stage == "E" ~ 2,
                            Stage == "M" ~ 3,
                            TRUE ~ 4)) |> 
  ggplot(aes(x = doy, y = cumtemp, colour = factor(Stage2), group = BlockID)) +
  geom_line() +
  scale_colour_viridis_d(option = "inferno", begin = 0.15, end = 0.8, direction = -1, name = "") +
  labs(x = "Day of the year", y = "Cumulative temperature") +
  facet_wrap(~ Year) +
  theme_bw()





# Ovule:seed ratio
sp_model <- lme(asin(sqrt(Seed_potential)) ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment, random = ~ 1|siteID, data = dat16)
plot(sp_model)
summary(sp_model)

ggplot(dat16, aes(asin(sqrt(Seed_potential)))) +
  geom_histogram()

# offset
sp_model <- glmer(Seed_potential ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment + (1|siteID), family = beta, data = dat16)

# offset
sp_model <- glmer(Seed_number ~ Biomass + Stage2 + MeanFlower.cen + CumTemp.cen + Treatment + (1|siteID), offset = Ovule_number, family = poisson, data = dat16)
check_model(sp_model)
summary(sp_model)
dat16 |> 
  ggplot(aes(x = factor(Stage2), y = Seed_potential)) +
  geom_jitter() +
  facet_wrap(~ Treatment)


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

dat |> 
  filter(Year == 2016) |> 
  ggplot(aes(x = factor(Stage2), y = Seed_potential, fill = factor(Stage2))) +
  geom_violin(draw_quantiles = c(0.5)) +
  scale_fill_viridis_d(option = "inferno", begin = 0.2, end = 0.8, direction = -1) +
  labs(x = "Tiing of snowmelt", y = "Seed:ovule ratio") +
  facet_wrap(~ Year)  +
  theme_bw()
