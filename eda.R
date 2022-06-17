# Exploratory Data Analysis for Milestone 1
# The data set used provides WFI Commercial Motor Insurance claims data
# at an individual vehicle and accident month level.

# Imported libraries
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")

data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
colnames(data)[1] <- 'accident_month'
dim(data) # Consists of 1,226,044 rows and 13 columns

# Data cleaning
data <- data %>%
  # remove entries where total_claims_cost is greater than the sum_insured
  filter((total_claims_cost <= sum_insured) | is.na(total_claims_cost))
  #filter(!(total_claims_cost > 0) & (is.na(claim_loss_date)))

# The date the first recorded month ended
first_month_ended = min(data$accident_month)

# The date the last recorded month ended
last_month_ended = max(data$accident_month)

# List of all unique policy ids
policy_ids = sort(unique(data$policy_id))

# List of all states where vehicles are located
risk_states = str_sort(unique(data$risk_state_name), numeric=TRUE)

# List of all postcodes
risk_postcodes = sort(unique(data$risk_postcode))

# List of all years of manufacture of vehicles
years_of_manufacture = sort(unique(data$year_of_manufacture))

# List of 15 different categories of vehicle classifications
vehicle_classes = str_sort(unique(data$vehicle_class), numeric=TRUE)

# Investigate claim data per postcode
claims_by_postcode <- data %>%
  group_by(risk_postcode) %>%
  summarise(TotalCarMonth = n(),
            TotalPaid = sum(total_claims_cost, na.rm=TRUE),
            AvgPaidPerCarMonth = TotalPaid/TotalCarMonth,
            AvgYearMade = mean(year_of_manufacture),
            AvgSumInsured = mean(sum_insured))

claims_by_postcode = claims_by_postcode[order(-claims_by_postcode$AvgPaidPerCarMonth, -claims_by_postcode$TotalPaid), ]

# As there are not enough data for every postcode for us to arrive at 
# a reasonable conclusion, I have decided to focus on claim data per LGA
# in Sydney and Melbourne.
# https://postcodez.com.au/postcodes/nsw/sydney
# Want to create a map of Sydney LGAs with colors representing claim behaviors.

# Investigate claim data by quarter
# Compare quarterly claim inflation with quarterly CPI published by ABS
# to investigate the relationship between the two.

# Added a column to include the quarter of the year for each record.
data['quarter_in_year'] <- as.yearqtr(data$accident_month, format="%Y-%m-%d")

# Consider all cars, including those without any claims.
data_by_quarters <- data %>%
  group_by(quarter_in_year) %>%
  summarise(total_car_month = n(),
            total_paid = sum(total_claims_cost, na.rm=TRUE),
            avg_paid_per_car = 3*total_paid/total_car_month)

# Consider only claims with positive total_claims_cost
claims_by_quarters <- data %>%
  filter(total_claims_cost > 0) %>%
  group_by(quarter_in_year) %>%
  summarise(total_claims = n(),
            avg_paid_per_claim = sum(total_claims_cost)/total_claims)

# Merge two tables
claims_by_quarters <- merge(data_by_quarters, claims_by_quarters, by="quarter_in_year")

# Model claim frequency with the average number of claims for a car in a quarter
claims_by_quarters["claims_per_car"] <- c(claims_by_quarters$total_claims/data_by_quarters$total_car_month)

ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_paid_per_car), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_paid_per_car), method = "lm") +
  ggtitle("Quarterly Average Paid per Car from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average paid per car", x="Quarter in Year")

# Average claim size
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_paid_per_claim), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_paid_per_claim), method = "lm") +
  ggtitle("Quarterly Average Claim Size from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claim Size in AU$", x="Quarter in Year")

# Average claim frequency
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=claims_per_car), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=claims_per_car), method = "lm") +
  ggtitle("Quarterly Average Claims Frequency from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claims Frequency", x="Quarter in Year", color = "Legend")

# Work on log changes

# total_claim_cost / sum_insured