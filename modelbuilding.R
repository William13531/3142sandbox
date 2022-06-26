# Task 2: Model Building
# How to model claims inflation using both
#   internal and
#   external
# data for future claims cost prediction?
# This task focuses on the model design, techniques and the
# modelling process.

# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")
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

claim_data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
m1_data <- read.csv("m1monthly.csv")
# Added a column to include the quarter of the year for each record.
claim_data['quarter_in_year'] <- as.yearqtr(claim_data$accident_month, format="%Y-%m-%d")
# Added another column to include the month of the year for each record.
claim_data['month_in_year'] <- as.yearmon(claim_data$accident_month, format="%Y-%m-%d")
# Added a column to include the age of the car
claim_data['car_age'] <- as.numeric(format(as.Date(claim_data$accident_month),format="%Y")) - claim_data$year_of_manufacture

m1_data['month_in_year'] <- as.yearmon(m1_data$DATE, format="%Y-%m-%d")
m1_data = subset(m1_data, select=-c(DATE))
m1_data <- m1_data %>% rename(M1 = MANMM101AUM189S)

# -----------------------------------------------------------------------------

# Data cleaning
claim_data <- claim_data %>%
  # remove entries where total_claims_cost is greater than the sum_insured
  # remove entries where the car has a negative age
  filter(((total_claims_cost <= sum_insured) | is.na(total_claims_cost)) & car_age >= 0)

# -----------------------------------------------------------------------------

# Data by quarters

# Consider all cars, including those without any claims.
data_by_quarters <- claim_data %>%
  group_by(quarter_in_year) %>%
  summarise(total_exposure = sum(exposure),
            total_paid = sum(total_claims_cost, na.rm=TRUE),
            avg_paid_per_car = specify_decimal(total_paid/(4*total_exposure), 1))

# Consider only claims with positive total_claims_cost
claims_by_quarters <- claim_data %>%
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
claims_by_quarters["avg_claim_frequency"] <- c(claims_by_quarters$total_claims/(4*data_by_quarters$total_exposure))

# -----------------------------------------------------------------------------

# Data by months

data_by_months <- claim_data %>%
  group_by(month_in_year) %>%
  summarise(total_exposure = sum(exposure),
            total_paid = sum(total_claims_cost, na.rm=TRUE),
            avg_paid_per_car = specify_decimal(total_paid/(12*total_exposure), 1))

claims_by_months <- claim_data %>%
  filter(total_claims_cost > 0) %>%
  group_by(month_in_year) %>%
  summarise(total_claims = n(),
            # Model claim size by the average claim size
            avg_claim_size = sum(total_claims_cost)/total_claims,
            # Model severity of claim as a % of sum insured
            avg_claim_severity = sum(total_claims_cost)/sum(sum_insured))

# Merge two tables
claims_by_months <- merge(data_by_months, claims_by_months, by="month_in_year")

# Model claim frequency with the average number of claims for a car in a month
claims_by_months["avg_claim_frequency"] <- c(claims_by_months$total_claims/(12*data_by_months$total_exposure))

claims_by_months = merge(claims_by_months, m1_data, by="month_in_year", all.x=TRUE)

claims_by_months['m1_change'] = c(0, diff(log(claims_by_months$"M1"), lag=1))
claims_by_months['avg_claim_size_change'] = c(0, diff(log(claims_by_months$"avg_claim_size"), lag = 1))