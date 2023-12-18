source("R/Linns paper/1_Import_Data.R")

library(lme4)
library(nlme)
library(broom.mixed)
library(performance)
library(patchwork)
library(readxl)

### Seed mass 2016
dat16 <- dat |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat |> 
  filter(Year == 2017)

#Run models without biomass
sm_model_16 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16)
summary(sm_model_16)

sm_model_17 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17)
summary(sm_model_17)


#When removing biomass 2016 ends up with stage as sifnificant (more than last time), and 2017 with flower abundance (more than before) and temp (same importance)

#2016 seperate per stage and year
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sm_model_16_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S2)
summary(sm_model_16_S2)

dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sm_model_16_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S3)
summary(sm_model_16_S3)

dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sm_model_16_S4 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S4)
summary(sm_model_16_S4)

#2017 seperate per stage and year
dat17_S1 <- dat17 |> 
  filter(Stage2 == 1)

sm_model_17_S1 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S1)
summary(sm_model_17_S1)

dat17_S2 <- dat17 |> 
  filter(Stage2 == 2)

sm_model_17_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S2)
summary(sm_model_17_S2)

dat17_S3 <- dat17 |> 
  filter(Stage2 == 3)

sm_model_17_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S3)
summary(sm_model_17_S3)


###############
###############
dat16 <- dat16 %>% 
  group_by(BlockID, Year, Plant) %>% 
  mutate(Number_seedovule = (Seed_number + Ovule_number)) %>% 
  ungroup()

### Snumber
sp_model <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16)
summary(sp_model)



#2016 seperate per stage
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sp_model_S2 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S2)
summary(sp_model_S2)


dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sp_model_S3 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S3)
summary(sp_model_S3)


dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sp_model_S4 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S4)
summary(sp_model_S4)



##########################################################################
### Correlation between tested factors


library(reshape2)
library(ggcorrplot)

dat4 <- dat %>% 
  filter(Year != '2017')

# Create a subset of your data with the relevant variables
subset_data <- dat[c("CumTemp_after.cen", "Stage2", "MeanFlower.cen")]

# Calculate the Pearson correlation matrix
cor_matrix <- cor(subset_data, method = "pearson")

# Convert correlation matrix to long format
cor_matrix_long <- melt(cor_matrix)
# Print the correlation matrix
print(cor_matrix)


ggcorrplot::ggcorrplot(cor_matrix, type = "lower", lab = TRUE)

ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

subset_data %>% 
  ggplot(aes(y = Biomass, x = CumTemp_after.cen)) +
  geom_point() +
  geom_smooth(method = lm)




############################################################################################################

##seed mass
dat %>%
  group_by(Year, Stage) %>%
  summarize(total_Seedmass = mean(Seed_mass))

# Independent samples t-test
SeedMassDiff <- t.test(Seed_mass ~ Year, data = dat)

# Print the result
print(SeedMassDiff)


###number of flowers
phenology %>%
  group_by(stage, year) %>%
  summarize(total_flowers = sum(flower.sum))

phenology %>%
  group_by(year) %>%
  summarize(total_flowers = sum(flower.sum))

# Independent samples t-test
phenoDiff <- t.test(flower.sum ~ year, data = phenology)

# Print the result
print(phenoDiff)


###Length of stages
dat_onlyHP <- dat %>% 
  filter(Plant %in% c("HP1", "HP2"))

dat_onlyHP <- dat_onlyHP %>% 
  mutate(DaysDifference = Collected - Date1)

result_stagesYear <- dat_onlyHP %>% 
  group_by(Year, Stage) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

result_year <- dat_onlyHP %>% 
  group_by(Year) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

result_Stages <- dat_onlyHP %>% 
  group_by(Stage2) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

# Assuming you have two groups, one for each year, in your dat_onlyHP dataframe
group_2016 <- dat_onlyHP$DaysDifference[dat_onlyHP$Year == 2016]
group_2017 <- dat_onlyHP$DaysDifference[dat_onlyHP$Year == 2017]

# Perform a t-test
t_test_result <- t.test(group_2016, group_2017)

# Print the results
t_test_result



######### Calculate average seedmass

# Calculate the average seedmass per year
average_seedmass <- aggregate(Seed_mass ~ Year, dat, FUN = mean)

# Calculate the standard deviation of the biomass per year
standard_deviation <- aggregate(Seed_mass ~ Year, dat, FUN = sd)

# Merge the average seedmass and standard deviation into a single data frame
resultSeed <- merge(average_seedmass, standard_deviation, by = "Year")

# Rename the columns
colnames(resultSeed) <- c("Year", "Average_Seedmass", "Standard_Deviation")

# Print the results
print(resultSeed)


# Independent samples t-test
SeedmassDiff <- t.test(Seed_mass ~ Year, data = dat)

# Print the result
print(SeedmassDiff)

####### TEST SIGNIFICANCE

# Select the seedmass for the two years you want to compare
year1_seedmass <- dat$Seed_mass[dat$Year == '2016']
year2_seedmass <- dat$Seed_mass[dat$Year == '2017']

# Perform the independent samples t-test
t_test_result <- t.test(year1_seedmass, year2_seedmass)

# Print the t-test result
print(t_test_result)

###Difference in temperature
# Independent samples t-test
TempDiff <- t.test(Temp_after ~ Year, data = dat3)

# Print the result
print(TempDiff)

# Select the temperature for the two years you want to compare
#year1_temp <- dat$CumTemp_after.cen[dat$Year == '2016']
#year2_temp <- dat$CumTemp_after.cen[dat$Year == '2017']

# Perform the independent samples t-test
#t_test_result_temp <- t.test(year1_temp, year2_temp)

# Print the t-test result
#print(t_test_result_temp)


######## Number of seeds
average_seednumber <- aggregate(Seed_number ~ Year, dat, FUN = mean)
standard_deviation_Seednumber <- aggregate(Seed_number ~ Year, dat, FUN = sd)
resultSeedNumber <- merge(average_seednumber, standard_deviation_Seednumber, by = "Year" )
colnames(resultSeedNumber) <- c("Year", "Average_Seednumber", "Standard_Deviation_SeedNumber")

# Print the results
print(resultSeedNumber)

average_ovulenumber <- aggregate(Ovule_number ~ Year, dat, FUN = mean)
standard_deviation_ovulenumber <- aggregate(Ovule_number ~ Year, dat, FUN = sd)
resultOvuleNumber <- merge(average_ovulenumber, standard_deviation_ovulenumber, by = "Year")
colnames(resultOvuleNumber) <- c("Year", "Average_Ovulenumber", "Standard_Deviation_OvuleNumber")

# Print the results
print(resultOvuleNumber)


average_SeedPotential <- aggregate(Seed_potential ~ Treatment, dat, FUN = mean)
standard_deviation_SeedPotential <- aggregate(Seed_potential ~ Treatment, dat, FUN = sd)
resultSeedPotential <- merge(average_SeedPotential, standard_deviation_SeedPotential, by = "Treatment")
colnames(resultSeedPotential) <- c("Treatment", "Average_SeedPotential", "Standard_Deviation_SeedPotential")

# Print the results
print(resultSeedPotential)

SeedPotentialDiff <- t.test(Seed_potential ~ Treatment, data = dat)
# Print the result
print(SeedPotentialDiff)

######### REMOVE??? #########


###### Find missing temperature at Finse in 2017 by predicting it from temperature from Midtstova
TempFinseMidtstova <- read_excel("Data_plant_pollinator_Finse_2016_2017/TempFinseMidtstova.xlsx") %>% 
  select(name = Navn, date = Tid, temperature = Temp) %>% 
  mutate(date = dmy(date), temperature = as.numeric(temperature))

FinseMidt <- TempFinseMidtstova %>% 
  group_by(date) %>% 
  pivot_wider(names_from = name, values_from = temperature) %>% 
  ungroup()

tPred <- lm(Finsevatn ~ Midtstova, data = FinseMidt)
summary(tPred)

#a = 1.078, R^2 = 0.9392, intercept = -0.79

#TFinse = A * tMId + Intercept

FinseMidtNew <- FinseMidt %>% 
  group_by(date) %>% 
  mutate(FinsevatnNy = (1.07811 * Midtstova + (-0.79)))
