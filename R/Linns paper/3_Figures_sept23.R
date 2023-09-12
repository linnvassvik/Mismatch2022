#Figures
source("R/Linns paper/1_Import_Data.R")

library(ggplot2)
library(dplyr)

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




