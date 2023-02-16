# Figures

source("R/Linns paper/1_ImportData.R")
source("R/Linns paper/2_Analysis.R")
library(ggplot2)
library(broom.mixed)

#Figure on dredge models (som i artikkel)


#Biomass first (need to get a straight line)
SeedMass17 %>% 
  ggplot(aes(y = log(Seed_mass), x = Biomass, color = Stage)) + 
  geom_point() +
  geom_line(data = output17, aes(x = Biomass, y = .fitted))


#Seed_potential mot stage (fjerne seed mass)


#abundance data (bruk de fra modell i analyse skript)
SeedMass1_OP %>% 
  ggplot(aes(y = 'log(Seed_mass)', x = MeanFlower.cen, color = Stage)) +
  geom_point() +
  geom_line(aes(y = .fitted)) 


#Pollen limitation