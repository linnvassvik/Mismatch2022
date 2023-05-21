# Figures

source("R/Linns paper/1_ImportData.R")
source("R/Linns paper/2_Analysis.R")
library(ggplot2)
library(broom.mixed)
library(lme4)
library(ggpubr)

#Pollen limitation
WeatherAndBiomass %>% 
  ggplot(aes(x = Treatment, y = log(Seed_mass), fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(Stage ~ Year)


#Figure on dredge models (som i artikkel)
#Plot, har ikke endret navn på intercept da jeg ikke vet om det er stage E eller treatment: control




#eller endre farge til symbol, tom sirkel eller hel
Seedmass2016 <- ggplot(resSM162, aes(x = Estimate, y = Variable, color = Pvalue<=0.05)) + 
  geom_errorbarh(aes(xmin = CI.low, xmax = CI.high, height = .0)) +   
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("black", "red")) +
  ggtitle("Seed mass 2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Parameter estimates", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "none") +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey") 
ggsave(Seedmass2016, filename = "Figurer/Seedmass2016.jpeg", height = 6, width = 8)

Seedmass2017 <- ggplot(resSM173, aes(x = Estimate, y = Variable, color = Pvalue<=0.05)) + 
  geom_errorbarh(aes(xmin = CI.low, xmax = CI.high, height = .0)) +   
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("black", "red")) +
  ggtitle("Seed mass 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Parameter estimates", y="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "none") +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey")
ggsave(Seedmass2017, filename = "Figurer/Seedmass2017.jpeg", height = 6, width = 8)

SeedmassCombined <- ggarrange(Seedmass2016, Seedmass2017, ncol = 2, nrow = 1, 
          common.legend = TRUE, legend = "none")
ggsave(SeedmassCombined, filename = "Figures/SeedmassCombined.jpeg", height = 6, width = 8)


######## seed mass
SeedMassStages <- ggplot(WeatherAndBiomass, aes(x = Stage, y = Seed_mass, fill = Stage)) +
  geom_boxplot() +
  scale_x_discrete(labels=c('Stage: early', 'Stage: mid', 'Stage: late', 'Stage: very late')) +
  labs(y = "Seed mass (g)", x = "Snowmelt stages", fill = "") +
  theme_bw() +
  ylim(0.000, 0.035) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_grid(~Year)
ggsave(SeedMassStages, filename = "Figures/SeedMassStages.jpeg", height = 6, width = 8)

SeedMassTreatment <- ggplot(WeatherAndBiomass, aes(x = Treatment, y = Seed_mass, fill = Treatment)) +
  geom_boxplot() +
  ylim(0.000, 0.035) +
  scale_x_discrete(labels=c('Open pollination', 'Supplemental pollination')) +
  scale_color_manual (values = c("#FF9966", "#CCCC99")) +
  scale_fill_manual(values = c("#FF9966", "#CCCC99")) +
  labs(y = "Seed mass (g)", x = "", fill = "") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust=1)) +
  facet_grid(~Year)
ggsave(SeedMassTreatment, filename = "Figures/SeedMassTreatment.jpeg", height = 6, width = 8)

##########
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