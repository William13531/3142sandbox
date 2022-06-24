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
library("ozmaps")
library(sf)
library("praise")

# -----------------------------------------------------------------------------

# Helper functions
replicate(5, praise())
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

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
  geom_point(aes(x=quarter_in_year, y=avg_paid_per_car), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_paid_per_car), method = "lm") +
  ggtitle("Quarterly Average Paid per Car from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average paid per car", x="Quarter in Year")

# Plot average claim size in a quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_size), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_size), method = "gam") +
  ggtitle("Quarterly Average Claim Size from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claim Size in AU$", x="Quarter in Year")

# Plot average claim frequency for a car in a quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_frequency), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_frequency), method = "gam") +
  ggtitle("Quarterly Average Claims Frequency from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claims Frequency", x="Quarter in Year", color = "Legend")

# Plot average claim severity for each quarter
ggplot(data=claims_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_severity), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_severity), method = "gam") +
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

claims_by_state <- data %>%
  group_by(risk_state_name) %>%
  filter(total_claims_cost >= 0) %>%
  summarise(no_of_claims = n())

data_by_state = merge(data_by_state, claims_by_state, by="risk_state_name")

# Plot a Australian map with number of claims represented by colours for each state
ozmap()
sf_oz <- ozmap_data("states")
ggplot(data = sf_oz) + geom_sf()

cases <- c(874,1756,683,1293,2726,105,2,14,0)
sf_oz$cases <- cases
ggplot(data = sf_oz, aes(fill = cases)) + 
  geom_sf() +
  labs(title="Number of Claims by States from 16Q3 to 21Q2") +
  scale_fill_gradient(low ="lightblue", high = "purple") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot total claim costs by states
ggplot(na.omit(data)) +
  geom_boxplot(aes(x = risk_state_name, y = total_claims_cost)) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(y="Total Claims Cost in AU$", x="State", title="Total Claims Cost by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot average claim cost by states
ggplot(data=data_by_state, aes(x=risk_state_name, y = avg_claims)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Claim Cost in AU$", x="State", title="Average Claim Size by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot average sum insured by states
ggplot(data=data_by_state, aes(x=risk_state_name, y = avg_sum_insured)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Sum Insured in AU$", x="State", title="Average Sum Insured per Car by State") +
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------------------------------------------------------

# EDA 3: Investigate claim data by car age

data_by_car_age <- data %>%
  group_by(car_age) %>%
  summarise(avg_claim_cost = mean(total_claims_cost, na.rm=TRUE),
            avg_sum_insured = mean(sum_insured),
            count_insured = n())

claims_by_car_age <- data %>%
  group_by(car_age) %>%
  filter(total_claims_cost >= 0) %>%
  summarise(no_of_claims = n())

data_by_car_age = merge(data_by_car_age, claims_by_car_age, by="car_age")
data_by_car_age["avg_claim_frequency"] = c(data_by_car_age$no_of_claims/data_by_car_age$count_insured)

ggplot(data_by_car_age) +
  geom_bar(aes(x=car_age,y=count_insured), stat="identity",width=0.5) +
  geom_smooth(aes(x=car_age,y=count_insured), method="gam") +
  labs(y="Number of Cars Insured", x="Car Age", title="Number of Cars Insured by Car Age") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_by_car_age) +
  geom_bar(aes(x=car_age,y=no_of_claims), stat="identity",width=0.5) +
  geom_smooth(aes(x=car_age,y=no_of_claims), method="gam") +
  labs(y="Number of Claims", x="Car Age", title="Number of Claims by Car Age") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_by_car_age) +
  geom_bar(aes(x=car_age,y=avg_claim_frequency), stat="identity",width=0.5) +
  geom_smooth(aes(x=car_age,y=avg_claim_frequency), method="gam") +
  labs(y="Average Claim Frequency", x="Car Age", title="Average Claim Frequency by Car Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 30)

ggplot(data_by_car_age) +
  geom_bar(aes(x=car_age,y=avg_claim_cost), stat="identity",width=0.5) +
  geom_smooth(aes(x=car_age,y=avg_claim_cost), method="gam") +
  labs(y="Average Claim Cost", x="Car Age", title="Average Claim Cost by Car Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 30)

# -----------------------------------------------------------------------------

# EDA: Investigate claim data by postcodes (William)
# Insufficient data

claims_by_postcode <- data %>%
  group_by(risk_postcode) %>%
  summarise(TotalCarMonth = n(),
            TotalPaid = sum(total_claims_cost, na.rm=TRUE),
            AvgPaidPerCarMonth = TotalPaid/TotalCarMonth,
            AvgYearMade = mean(year_of_manufacture),
            AvgSumInsured = mean(sum_insured))

claims_by_postcode = claims_by_postcode[order(-claims_by_postcode$AvgPaidPerCarMonth, -claims_by_postcode$TotalPaid), ]