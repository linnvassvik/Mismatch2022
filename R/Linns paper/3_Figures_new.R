source("R/Linns paper/1_Import_Data.R")
source("R/Linns paper/Temp.R")

library(ggpubr)

#pollination2 with mean flower
#dat_DOY with doy snowmelt, days since snowmelt and cumtemp

dat_DOYred <- dat_DOY %>% 
  distinct(Year, Stage, siteID, PeakFlower_doy, Snowmelt_doy.cen, Snowmelt_doy, CumTemp_Peak.cen, DOY_sinceSM, DOY_sinceSM.cen, CumTemp_Peak)

MeanFlowerdf <- pollination2 %>% 
  distinct(stage, site, flower.sum, doy) %>% 
  mutate(siteID = paste(stage, site, sep = " ")) %>% 
  mutate(year = year (day.poll))

MeanFlower2 <- MeanFlowerdf %>% 
  group_by(year, siteID) %>% 
  mutate(MeanFlower = mean(flower.sum)) %>% 
  mutate(TotFlower = sum(flower.sum)) %>% 
  filter(!duplicated(siteID)) %>% 
  select(-site, -flower.sum, -day.poll, -doy) %>% 
  rename(Year = year) %>% 
  rename(Stage = stage)

day_flower <- dat_DOYred %>% 
  left_join(MeanFlower2, by = c("Year", "siteID", "Stage")) %>% 
  mutate(shape = case_when(Year == 2016 & Stage == "L" ~ "16 unique",
                           Year == 2016 & Stage %in% c("M", "E") ~ "16 sammen",
                           Year == 2017 & Stage == "F" ~ "17 unique",
                           Year == 2017 & Stage %in% c("M", "E") ~ "17 sammen")) 


plot1 <- pollination2 %>% 
  ggplot(aes( x = doy, y = flower.mean, color = as.factor(year.poll), fill = as.factor(year.poll), shape = as.factor(year.poll))) +
  geom_point(alpha = 0.2) +
  geom_smooth(alpha = 0.2, method = "gam", method.args = list(family = quasipoisson)) +
  theme_minimal() +
  scale_fill_manual (values =  c("#CC9999", "#330000")) +
  scale_color_manual (values =  c("#CC9999", "#330000")) +
  labs(x = "DOY", y = "Floral density", shape = "Year", color = "Year", fill = "Year") 



#  ggplot() +
# geom_jitter(data = Temperature_all, aes(x = doy, y = Temp_2016_ALR, color = "2016"), position = position_jitter(width = 1), size = 0.2) +
#   geom_jitter(data = Temperature_all, aes(x = doy, y = Temp_2017_ALR, color = "2017"), position = position_jitter(width = 1), size = 0.2) +
#   labs(x = "DOY", y = "Average daily temperature (°C)", color = "") +
#   scale_color_manual(values = c("2016" = "#CC9999", "2017" = "#330000")) +
#   theme_minimal()

 plot2 <- ggplot() +
  geom_point(data = Klima, aes(x = DOY, y = Temp16New, color = "2016", shape = "16")) +
  geom_point(data = Klima, aes(x = DOY, y = Temp17New, color = "2017", shape = "17")) +
  labs(x = "DOY", y = "Average daily temperature °C", color = "", shape = "") +
   scale_color_manual(values = c("2016" = "#CC9999", "2017" = "#330000")) +
   scale_shape_manual(values = c("16" = 16, "17" = 17)) +
   guides(color = guide_legend(override.aes = list(shape = c(16, 17)), title = "Year"),
          shape = guide_legend(override.aes = list(color = c("#CC9999", "#330000")), title = "Year")) +
   guides(color = "none") +
   guides(shape = "none") +
   theme_minimal()
 
plot4 <- Klima_Year %>% 
  ggplot(aes(x = Year, y = Temperature, color = Year)) +
  geom_boxplot() + 
  geom_jitter(aes(shape = Year), alpha = 0.2)+
  scale_color_manual(values = c("#CC9999", "#330000")) +
  scale_shape_manual(values = c("2016" = 16, "2017" = 17)) +
  labs(x = "Year", y = "", color = "") +
  guides(color = "none") +
  guides(shape = "none") +
  theme_minimal()


plot3 <- day_flower %>% 
  ggplot(aes(x = DOY_sinceSM, y = CumTemp_Peak, color = Snowmelt_doy, fill = Snowmelt_doy, shape = shape)) +
  geom_point(size = 4) +
  scale_colour_gradient(low = "#FFCC66", high = "#990000", na.value = NA) +
  scale_fill_gradient(low = "#FFCC66", high = "#990000", na.value = NA) +
  scale_shape_manual(values = c(21, 1, 24, 2), guide = "none") +
  theme_minimal() +
  labs(x = "Days since snowmelt", y = "Growing degree days above 0 °C", color = "Timing of snowmelt (DOY)", fill = "Timing of snowmelt (DOY)", shape = "Year") 

#fylte rundinger på 2 år, åpne 1 år
Pheno1 <- ggarrange(plot2, plot4, legend = "bottom", labels = c("a", "b"))
Pheno2 <- ggarrange(plot1, plot3, legend = "bottom", labels = c("c", "d"))

PhenologyPlot <- ggarrange(
  ggarrange(plot2, plot4, legend = "bottom", labels = c("a", "b")),
  ggarrange(plot1, plot3, legend = "bottom", labels = c("c", "d")),
  ncol = 1,
  nrow = 2
)

ggsave(PhenologyPlot, filename = "Figures/PhenologyPlot.jpeg", height = 10, width = 8)




##########
##########
#########
total_flowers_per_year <- pollination2 %>%
  group_by(year.poll) %>%
  summarise(total_flowers = sum(flower.sum))
  
ModelFl <- lm(flower.sum ~ year.poll, data = pollination2)
summary(ModelFl)

total_visits_per_year <- pollination2 %>%
  group_by(year.poll) %>%
  summarise(total_fly = sum(fly))

ModelVis <- lm(fly ~ year.poll, data = pollination2)
summary(ModelVis)
