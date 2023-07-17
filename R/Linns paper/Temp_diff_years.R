source("R/Linns paper/1_Import_Data.R")

library("tidyverse")
library("dplyr")

# Create the year column
Weather <- Weather %>% mutate(year = year(date))

# Subset the data for the two years and remove NAs
Weather <- Weather %>%
  filter(year %in% c(2016, 2017), !is.na(temperature))

# Filter the data for the specified day of the year ranges
Weather2 <- Weather %>%
  filter((year == 2016 & doy >= 169 & doy <= 247) |
           (year == 2017 & doy >= 145 & doy <= 232))

# Print the filtered data
print(Weather2)



# Subset the data for the two years of interest
subset_data <- Weather2[Weather2$year %in% c(2016, 2017), ]

# Perform an independent samples t-test
test_result <- t.test(subset_data$temperature ~ subset_data$year)

# Print the test result
print(test_result)

# Calculate average temperature and standard deviation for each year
summary_stats <- Weather2 %>%
  group_by(year) %>%
  summarize(avg_temperature = mean(temperature),
            sd_temperature = sd(temperature))

# Print the summary statistics
print(summary_stats)

