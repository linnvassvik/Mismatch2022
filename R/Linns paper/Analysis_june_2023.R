source("R/Linns paper/1_Import_Data.R")

library(lme4)
library(nlme)
library(MuMIn)
library(broom.mixed)
library(performance)
library(patchwork)

### Seed mass 2016
dat16 <- dat |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat |> 
  filter(Year == 2017)

#Run models without biomass

sm_model_16 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat16)
summary(sm_model_16)

sm_model_17 <- lme(log(Seed_mass) ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat17)
summary(sm_model_17)

#When removing biomass 2016 ends up with stage as sifnificant (more than last time), and 2017 with flower abundance (more than before) and temp (same importance)

#2016 seperate per stage and year
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sm_model_16_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat16_S2)
summary(sm_model_16_S2)

dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sm_model_16_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat16_S3)
summary(sm_model_16_S3)

dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sm_model_16_S4 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat16_S4)
summary(sm_model_16_S4)

#2017 seperate per stage and year
dat17_S1 <- dat17 |> 
  filter(Stage2 == 1)

sm_model_17_S1 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat17_S1)
summary(sm_model_17_S1)

dat17_S2 <- dat17 |> 
  filter(Stage2 == 2)

sm_model_17_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat17_S2)
summary(sm_model_17_S2)

dat17_S3 <- dat17 |> 
  filter(Stage2 == 3)

sm_model_17_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + CumTemp_before + Treatment, random =  ~ 1|siteID, data = dat17_S3)
summary(sm_model_17_S3)


###############
###############
dat16 <- dat16 %>% 
  group_by(BlockID, Year, Plant) %>% 
  mutate(Number_seedovule = (Seed_number + Ovule_number)) %>% 
  ungroup()

### Seed:ovule ratio/seed potential
sp_model <- glmer(Seed_number ~ Stage + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(Number_seedovule) + (1|siteID), family = poisson, data = dat16)
summary(sp_model)


#2016 seperate per stage
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sp_model_S2 <- glmer(Seed_potential ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment + offset(log(Ovule_number)) + (1|siteID), family = binomial, data = dat16_S2)
summary(sp_model_S2)


dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sp_model_S3 <- glmer(Seed_number ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment + (1|siteID), family = poisson, data = dat16_S3)
summary(sp_model_S3)
#Temperature before and after flowering significant (positive)

dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sp_model_S4 <- glmer(Seed_number ~ MeanFlower.cen + CumTemp_before.cen + CumTemp_after.cen + Treatment + (1|siteID), family = poisson, data = dat16_S4)
summary(sp_model_S4)
#singular fit issues, temperature before flowering (positive) and number of flowers (positive) significant

#plot
dat16 %>% 
  ggplot(aes(y = Seed_number, x = MeanFlowers)) +
  geom_smooth() +
  facet_wrap(~Stage2)



###########################
#Look into variables concerning pollination

#Seed mass and pollination
dat16_clean <- dat16[!is.infinite(dat16$MeanVisit), ]

Poll_model_16 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat16_clean)
summary(Poll_model_16)


Poll_model_17 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat17)
summary(Poll_model_17)


#2016 seperate per stage and year
dat16_S2 <- dat16_clean |> 
  filter(Stage2 == 2)

Poll_model_16_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat16_S2)
summary(Poll_model_16_S2)

dat16_S3 <- dat16_clean |> 
  filter(Stage2 == 3)

Poll_model_16_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat16_S3)
summary(Poll_model_16_S3)

dat16_S4 <- dat16_clean |> 
  filter(Stage2 == 4)

Poll_model_16_S4 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat16_S4)
summary(Poll_model_16_S4)

#2017 seperate per stage and year
dat17_S1 <- dat17 |> 
  filter(Stage2 == 1)

Poll_model_17_S1 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat17_S1)
summary(Poll_model_17_S1)

dat17_S2 <- dat17 |> 
  filter(Stage2 == 2)

Poll_model_17_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat17_S2)
summary(Poll_model_17_S2)
#mean flowers significant

dat17_S3 <- dat17 |> 
  filter(Stage2 == 3)

Poll_model_17_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen * MeanVisit, random =  ~ 1|siteID, data = dat17_S3)
summary(Poll_model_17_S3)

#plot
dat %>% 
  ggplot(aes(y = MeanVisit, x = MeanFlowers, color =Stage)) +
  geom_point() +
  geom_smooth(method = lm, expand = TRUE) +
  facet_wrap(~Year, scales ="free_x")


### Seed:ovule ratio/seed potential
sp_model_Poll <- glmer(Seed_number ~ MeanFlower.cen + MeanVisit + (1|siteID), offset = Ovule_number, family = poisson, data = dat16_clean)
summary(sp_model_Poll)
#Snowmeltstage and Hand pollinated treatment significant.

#2016 seperate per stage
dat16_S2 <- dat16_clean |> 
  filter(Stage2 == 2)

sp_model_S2_Poll <- glmer(Seed_number ~ MeanFlower.cen + MeanVisit + Treatment + (1|siteID), family = poisson, data = dat16_S2)
summary(sp_model_S2_Poll)
#Number of flowers (positive) and hand pollinated treatment (negative) significant

dat16_S3 <- dat16_clean |> 
  filter(Stage2 == 3)

sp_model_S3_Poll <- glmer(Seed_number ~ MeanFlower.cen + MeanVisit + Treatment + (1|siteID), family = poisson, data = dat16_S3)
summary(sp_model_S3_Poll)
#Mean flowers significant

dat16_S4 <- dat16_clean |> 
  filter(Stage2 == 4)

sp_model_S4_Poll <- glmer(Seed_number ~ MeanFlower.cen + MeanVisit + Treatment + (1|siteID), family = poisson, data = dat16_S4)
summary(sp_model_S4_Poll)


####################################
### Correlation between tested factors


library(reshape2)
library(ggcorrplot)

# Create a subset of your data with the relevant variables
subset_data <- dat[c("Biomass", "CumTemp_before.cen", "CumTemp_after.cen", "Stage2", "MeanFlower.cen", "MeanVisit")]

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

#correlation between temperature before and after first HP, but very weak correlation on the rest?













######### Calculate average biomass

# Calculate the average biomass per year
average_biomass <- aggregate(Biomass ~ Year, dat, FUN = mean)

# Calculate the standard deviation of the biomass per year
standard_deviation <- aggregate(Biomass ~ Year, dat, FUN = sd)

# Merge the average biomass and standard deviation into a single data frame
result <- merge(average_biomass, standard_deviation, by = "Year")

# Rename the columns
colnames(result) <- c("Year", "Average_Biomass", "Standard_Deviation")

# Print the results
print(result)

######### Calculate average seedmass

# Calculate the average seedmass per year
average_seedmass <- aggregate(Seed_mass ~ Year, dat, FUN = mean)

# Calculate the standard deviation of the biomass per year
standard_deviation <- aggregate(Seed_mass ~ Year, dat, FUN = sd)

# Merge the average biomass and standard deviation into a single data frame
resultSeed <- merge(average_seedmass, standard_deviation, by = "Year")

# Rename the columns
colnames(resultSeed) <- c("Year", "Average_Seedmass", "Standard_Deviation")

# Print the results
print(resultSeed)

####### TEST SIGNIFICANCE

# Select the seedmass for the two years you want to compare
year1_seedmass <- dat$Seed_mass[dat$Year == '2016']
year2_seedmass <- dat$Seed_mass[dat$Year == '2017']

# Perform the independent samples t-test
t_test_result <- t.test(year1_seedmass, year2_seedmass)

# Print the t-test result
print(t_test_result)



# Select the temperature for the two years you want to compare
#year1_temp <- dat$CumTemp_after.cen[dat$Year == '2016']
#year2_temp <- dat$CumTemp_after.cen[dat$Year == '2017']

# Perform the independent samples t-test
#t_test_result_temp <- t.test(year1_temp, year2_temp)

# Print the t-test result
#print(t_test_result_temp)
