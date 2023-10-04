#Figures
source("R/Linns paper/1_Import_Data.R")

library(ggplot2)
library(dplyr)
library(broom.mixed)
library(nlme)

### 2016 

sm_model_16 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16)

newdata <- crossing(Stage2 = c(2, 3, 4),
                    MeanFlower.cen = seq(-0.933, 2.27, by = 0.05),
                    CumTemp_after.cen = 0,
                    Treatment = c("Control", "Pollinated"),
                    siteID = unique(dat16$siteID))

out <- augment(x = sm_model_16, newdata = newdata)

#out <- augment(x = sm_model_16, newdata = newdata, re.form = NA)

ggplot(dat16, aes(x = MeanFlower.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm)
  
  geom_line(aes(y = .fitted), data = out)

ggplot(dat16, aes(x = CumTemp_after.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm)
  
  geom_line(aes(y = .fitted), data = out)


out16 <- dat16 |> 
  bind_cols(augment(sm_model_16) |> 
              select(.fitted))




###### SIMPLE PLOTS

AbundanceDoy <- pollination2 %>% 
  ggplot(aes( x = doy, y = flower.sum, color = stage)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ year.poll)
ggsave(AbundanceDoy, filename = "Figures/AbundanceDoy.jpeg", height = 6, width = 8)



# Create the boxplot
SeedMass_Stage <- dat %>%
  ggplot(aes(x = Stage, y = Seed_mass, fill = Stage)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 0.03)) + #excluded two outliers in 2017
  facet_grid(~ Year)
ggsave(SeedMass_Stage, filename = "Figures/SeedMass_Stage.jpeg", height = 6, width = 8)  


SeedNumber_Stage <- dat %>%
  ggplot(aes(x = Stage, y = Seed_number, fill = Stage)) +
  geom_boxplot()
ggsave(SeedNumber_Stage, filename = "Figures/SeedNumber_Stage.jpeg", height = 6, width = 8)  


## Plots without .fitted

ggplot(dat, aes(x = MeanFlower.cen, y = log(Seed_mass), color = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_manual (values =  c("#FFFF00", "#FF6633", "#660066", "#003366")) +
  labs(x = "Mean number of flowers", y = "log(Seed mass)", fill = "Stage") +
  theme_minimal() +
  facet_wrap(~ Year)


ggplot(dat, aes(x = CumTemp_after.cen, y = log(Seed_mass), colour = factor(Stage2))) +
  geom_point() +
  geom_smooth(method = lm, aes(fill = factor(Stage2))) +
  scale_color_manual(values =  c("#FFFF00", "#FF6633", "#660066", "#003366", name = NULL)) +
  scale_fill_manual(values =  c("#FFFF00", "#FF6633", "#660066", "#003366")) +
  labs(x = "Cumulative temperature", y = "log(Seed mass)", fill = "Stage") +
  theme_minimal() +
  facet_wrap(~ Year)

dat %>%
  ggplot(aes(x = factor(Stage2), y = log(Seed_mass), fill = factor(Stage2))) +
  geom_violin() +
  scale_fill_manual (values =  c("#FFFF00", "#FF6633", "#660066", "#003366")) +
  labs(x = "Stage", y = "log(Seed mass)", fill = "Stage") +
  theme_minimal() +
  facet_grid(~ Year)



