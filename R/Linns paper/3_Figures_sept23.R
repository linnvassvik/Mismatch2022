#Figures
source("R/Linns paper/1_Import_Data.R")

library(ggplot2)

AbundanceDoy <- pollination2 %>% 
  ggplot(aes( x = doy, y = flower.sum, color = stage)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ year.poll)
ggsave(AbundanceDoy, filename = "Figures/AbundanceDoy.jpeg", height = 6, width = 8)

