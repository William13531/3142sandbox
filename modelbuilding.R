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
#m1_monthly_data <- read.csv("m1monthly.csv")
m1_qtrly_data <- read.csv("m1qtrly.csv")
cpi_qtrly_data <- read.csv("qtrlycpi.csv")
# Added a column to include the quarter of the year for each record.
claim_data['quarter_in_year'] <- as.yearqtr(claim_data$accident_month, format="%Y-%m-%d")
# Added another column to include the month of the year for each record.
claim_data['month_in_year'] <- as.yearmon(claim_data$accident_month, format="%Y-%m-%d")
# Added a column to include the age of the car
claim_data['car_age'] <- as.numeric(format(as.Date(claim_data$accident_month),format="%Y")) - claim_data$year_of_manufacture

m1_qtrly_data['quarter_in_year'] <- as.yearqtr(m1_qtrly_data$DATE, format="%Y-%m-%d")
m1_qtrly_data = subset(m1_qtrly_data, select=-c(DATE))
m1_qtrly_data <- m1_qtrly_data %>% rename(M1 = MANMM101AUM189S)

cpi_qtrly_data['quarter_in_year'] <- as.yearqtr(cpi_qtrly_data$Time, format="%b-%y")
cpi_qtrly_data = subset(cpi_qtrly_data, select=-c(Time))

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

claims_by_quarters = merge(claims_by_quarters, m1_qtrly_data, by="quarter_in_year", all.x=TRUE)

claims_by_quarters['m1_change'] = c(0, diff(log(claims_by_quarters$"M1"), lag=1))
claims_by_quarters['avg_claim_size_change'] = c(0, diff(log(claims_by_quarters$"avg_claim_size"), lag = 1))

claims_by_quarters <- merge(claims_by_quarters, cpi_qtrly_data, by="quarter_in_year")

claims_by_quarters.glm <- glm(avg_claim_size~PriceIndex+M1, weights=total_exposure, family=Gamma, data=claims_by_quarters)
print(summary(claims_by_quarters.glm,corr=T))
print(anova(claims_by_quarters.glm, test="Chi"))
par(mfrow=c(2,2))
plot(claims_by_quarters.glm)

# analysis of residuals
plot(avg_claim_size, fitted(claims_by_quarters.glm), xlab="observed amounts", ylab="fitted values",main="Observed vs Predicted",pch=20)
abline(0,1)
plot(fitted(claims_by_quarters.glm), resid(claims_by_quarters.glm,type="deviance"), xlab="fitted values",ylab="deviance residuals", main="Fitted vs Residuals",pch=20)
abline(0,0)
qqnorm(resid(claims_by_quarters.glm, type="pearson"),xlab="quantiles of Std Normal",ylab="Pearson residuals",pch=20)
qqline(resid(claims_by_quarters.glm))