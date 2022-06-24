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

# Consider all cars, including those without any claims.
data_by_quarters <- data %>%
  group_by(quarter_in_year) %>%
  summarise(total_exposure = sum(exposure),
            total_paid = sum(total_claims_cost, na.rm=TRUE),
            avg_paid_per_car = specify_decimal(total_paid/(4*total_exposure), 1))

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
claims_by_quarters["avg_claim_frequency"] <- c(claims_by_quarters$total_claims/(4*data_by_quarters$total_exposure))


