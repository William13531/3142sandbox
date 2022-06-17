# Exploratory Data Analysis for Milestone 1
# The data set used provides WFI Commercial Motor Insurance claims data
# at an individual vehicle and accident month level.

# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# -----------------------------------------------------------------------------

# Import and modify data here

data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
# Added a column to include the quarter of the year for each record.
data['quarter_in_year'] <- as.yearqtr(data$accident_month, format="%Y-%m-%d")
# Added a column to include the age of the car
data['car_age'] <- as.numeric(format(as.Date(data$accident_month),format="%Y")) - data$year_of_manufacture
dim(data) # Consists of 1,226,044 rows and 13 columns

# -----------------------------------------------------------------------------

# Data cleaning (David and William)
data <- data %>%
  # remove entries where total_claims_cost is greater than the sum_insured
  # remove entries where the car has a negative age
  filter(((total_claims_cost <= sum_insured) | is.na(total_claims_cost)) & car_age >= 0)

# -----------------------------------------------------------------------------

# Some basic explorations of data

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

# -----------------------------------------------------------------------------

# EDA 1: Investigate claim data by quarter (William)
# Compare quarterly claim inflation with quarterly CPI published by ABS
# to investigate the relationship between the two.

# Consider all cars, including those without any claims.
data_by_quarters <- data %>%
  group_by(quarter_in_year) %>%
  summarise(total_car_month = n(),
            total_paid = sum(total_claims_cost, na.rm=TRUE),
            avg_paid_per_car = specify_decimal(3 * total_paid/total_car_month, 1))

# Consider only claims with positive total_claims_cost
claims_by_quarters <- data %>%
  filter(total_claims_cost > 0) %>%
  group_by(quarter_in_year) %>%
  summarise(total_claims = n(),
            # Model claim size by the average claim size
            avg_claim_size = sum(total_claims_cost)/total_claims,
            # Model severity of claim as a % of sum insured
            avg_claim_severity = sum(total_claims_cost)/sum(sum_insured))

# Merge two tables
claims_by_quarters <- merge(data_by_quarters, claims_by_quarters, by="quarter_in_year")

# Model claim frequency with the average number of claims for a car in a quarter
claims_by_quarters["avg_claim_frequency"] <- c(claims_by_quarters$total_claims/data_by_quarters$total_car_month)

# Plot average paid for each car in a quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_size), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_size), method = "lm") +
  ggtitle("Quarterly Average Paid per Car from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average paid per car", x="Quarter in Year")

# Plot average claim size in a quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_size), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_size), method = "lm") +
  ggtitle("Quarterly Average Claim Size from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claim Size in AU$", x="Quarter in Year")

# Plot average claim frequency for a car in a quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_frequency), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_frequency), method = "lm") +
  ggtitle("Quarterly Average Claims Frequency from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claims Frequency", x="Quarter in Year", color = "Legend")

# Plot average claim severity for each quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_severity), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_severity), method = "lm") +
  ggtitle("Quarterly Average Claim Severity from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claim Severity", x="Quarter in Year", color = "Legend")

# -----------------------------------------------------------------------------

# EDA 2: Investigate claim data by states (Hedy and Danny)
data_by_state <- data %>%
  group_by(risk_state_name) %>%
  summarise(avg_car_age = mean(car_age),
            avg_claims = mean(total_claims_cost,na.rm=TRUE),
            avg_sum_insured = mean(sum_insured))

# Plot total claim costs by states
ggplot(na.omit(data)) +
  geom_boxplot(aes(x = risk_state_name , y = total_claims_cost)) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(y="Total Claims Cost in AU$", x="State", title="Claims Cost per Car by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot average claim cost by states
ggplot(data=data_by_state, aes(x=risk_state_name,y=avg_claims)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Claim Cost in AU$", x="State", title="Average Claim Cost per Car by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot average sum insured by states
ggplot(data=data_by_state, aes(x=risk_state_name,y=avg_sum_insured)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Sum Insured in AU$", x="State", title="Average Sum Insured per Car by State") +
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------------------------------------------------------

# EDA 3: Investigate claim data by car age



# -----------------------------------------------------------------------------

# EDA: Investigate claim data by postcodes (William)
# As there are not enough data for every postcode for us to arrive at 
# a reasonable conclusion, I have decided to focus on claim data per LGA
# in Sydney and Melbourne.
# https://postcodez.com.au/postcodes/nsw/sydney
# Want to create a map of Sydney LGAs with colors representing claim behaviors.

claims_by_postcode <- data %>%
  group_by(risk_postcode) %>%
  summarise(TotalCarMonth = n(),
            TotalPaid = sum(total_claims_cost, na.rm=TRUE),
            AvgPaidPerCarMonth = TotalPaid/TotalCarMonth,
            AvgYearMade = mean(year_of_manufacture),
            AvgSumInsured = mean(sum_insured))

claims_by_postcode = claims_by_postcode[order(-claims_by_postcode$AvgPaidPerCarMonth, -claims_by_postcode$TotalPaid), ]

# total_claim_cost / sum_insured