library(dplyr)
library(readxl)
library(lubridate)

#eklima
eklima16 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016_ny.xlsx") %>% 
  rename(dato = Date) %>% 
  select(-Nedbor) %>% 
  mutate(Date = as.Date(dato)) %>% 
  rename(TempS16 = Temp_station) %>% 
  select(-dato) %>% 
  mutate(DOY = yday(Date)) %>% 
  select(-Date)

eklima17 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Finse_weather.xlsx") %>% 
  rename(TempS17 = temperature) %>% 
  mutate(Date = as.Date(date)) %>% 
  select(-precipitation, -date) %>% 
  mutate(DOY = yday(Date)) %>% 
  select(-Date)

Klima <- eklima17 %>% 
  left_join(eklima16, by = "DOY") %>% 
  na.omit() 
  

#adiabatic temp with meter above sea level per site
ATemp <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_adiabatic_rate.xlsx") %>% 
  rename(ID = siteID) %>% 
  rename(ATemp = Adiabatic.temp) %>% 
  mutate(MASLCorr = MASL - 1222)

# Define the adiabatic lapse rate (6.5°C per 1000 meters)
lapse_rate <- 6.5  # in °C/1000m

Klima <- Klima %>% 
  mutate(Temp16New = (TempS16 - (lapse_rate * 228/1000))) %>% 
  mutate(Temp17New = (TempS17 - (lapse_rate * 228/1000)))

#Klima wihtout DOY longform
Klima_Year <- Klima %>% 
  select(-TempS17, -TempS16) %>% 
  pivot_longer(cols = starts_with("Temp"),
               names_to = "Year",
               values_to = "Temperature") %>%
  mutate(Year = if_else(str_detect(Year, "16"), "2016", "2017"))

#Load temperature dataset from ibutton
iButtonFinse <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_iButton_2016.xlsx") %>% 
  separate(Date, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date))

#average temp per day
iButtonFinse_avrg <- aggregate(iButtonFinse$Temperature, by = list(iButtonFinse$Date, iButtonFinse$ID), FUN = mean) %>% 
  rename(Date = Group.1) %>% 
  rename(ID = Group.2) %>% 
  rename(TempButton = x)

ATemp_16 <- ATemp %>% 
  filter(!ID %in% paste0("f", sprintf("%02d", 1:8))) %>% 
  select(-ATemp, -MASLCorr)

iButtonMASL <- iButtonFinse_avrg %>% 
  left_join(ATemp_16, by = "ID")

iButtonMASL %>% 
  ggplot(aes(y = TempButton, x = MASL)) +
  geom_point() +
  geom_smooth()

ModelTemp <- lm(MASL ~ TempButton, data = iButtonMASL)

summary(ModelTemp)

  







##########
#########
t_test_result <- t.test(Klima$Temp16New, Klima$Temp17New, paired = TRUE)

print(t_test_result) 

# Calculate the differences between Temp16New and Temp17New
Klima_filtered <- Klima %>%
  filter(DOY > 140 & DOY < 197)

differences <- Klima_filtered$Temp16New - Klima_filtered$Temp17New
average_difference <- mean(differences)

# Print the average difference
print(average_difference)

##############
#############
average_seed_proportion <- dat16 %>%
  filter(Snowmelt_doy == 169) %>%
  summarise(average_seed_proportion = mean(Seed_number, na.rm = TRUE))

average_seed_proportion2 <- dat16 %>%
  filter(Snowmelt_doy == 186) %>%
  summarise(average_seed_proportion = mean(Seed_potential, na.rm = TRUE))

dat_fly <- dat %>% 
  group_by(Year) %>% 
  summarise(Visit_rate_fly <- mean(MeanVisit))

model_vis <- lm(MeanVisit ~Year, data = dat)
summary(model_vis)



Seeds <- dat16 %>% 
  filter(Snowmelt_doy == 169) %>% 
  summarise