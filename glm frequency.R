# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")
library("ozmaps")
library(sf)
library(mgcv)

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

# -----------------------------------------------------------------------------

# Import and modify data here

data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
# Added a column to include the quarter of the year for each record.
data['quarter_in_year'] <- as.yearqtr(data$嚜瘸ccident_month, format="%Y-%m-%d")
# Added a column to include the age of the car
data['car_age'] <- as.numeric(format(as.Date(data$嚜瘸ccident_month),format="%Y")) - data$year_of_manufacture

# Data cleaning 
data <- data %>%
  # remove entries where the car has a negative age
  filter(car_age >= 0)

# Consider all cars, including those without any claims.
data_by_quarters <- data %>%
  group_by(quarter_in_year) %>%
  summarise(total_car_month = n(),
            avg_car_age = mean(car_age))

# Consider only claims with positive total_claims_cost
claims_by_quarters <- data %>%
  filter(total_claims_cost > 0) %>%
  group_by(quarter_in_year) %>%
  summarise(total_claims = n())

# Merge two tables
frequency_by_quarters <- merge(data_by_quarters, claims_by_quarters, by="quarter_in_year")

# Model claim frequency with the average number of claims for a car in a quarter
frequency_by_quarters["avg_claim_frequency"] <- c(frequency_by_quarters$total_claims/frequency_by_quarters$total_car_month)

# Plot average claim frequency for a car in a quarter
ggplot(data=frequency_by_quarters) +
  geom_point(aes(x=quarter_in_year, y=avg_claim_frequency), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=quarter_in_year, y=avg_claim_frequency), method = "gam") +
  ggtitle("Quarterly Average Claims Frequency from 2016Q3 to 2021Q2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Average Claims Frequency", x="Quarter in Year", color = "Legend")

# ------------------------------------------------------------------------------
# Milestone 2 
# Claim frequency

# Import and modify data
fuel_price <- read.csv("Automotive fuel quarterly movement.csv")
fuel_price$嚜熹uarter <- as.yearqtr(fuel_price$嚜熹uarter)

GDP <- read.csv("Gross domestic product.csv")
GDP$嚜熹uarter <- as.yearqtr(GDP$嚜熹uarter)

frequency_by_quarters["fuel_price"] <- fuel_price$Index
frequency_by_quarters["GDP"] <- GDP$Index

# Regression
# Check for normal distribution
par(mfrow=c(1,1))
hist(frequency_by_quarters$avg_claim_frequency) 
hist(frequency_by_quarters$total_claims)

# Check for linearity
plot(frequency_by_quarters$avg_claim_frequency ~ frequency_by_quarters$fuel_price)
plot(frequency_by_quarters$total_claims ~ frequency_by_quarters$fuel_price)

# Check for correlation
cor(frequency_by_quarters$fuel_price, frequency_by_quarters$GDP) 

# Model claim frequency
claim_frequency_data = subset(frequency_by_quarters)

# All 20 observations used for training
claim_frequency_glm <- glm(formula = total_claims ~ fuel_price + GDP + avg_car_age, family = 'binomial', data = frequency_by_quarters)
summary(claim_frequency_glm)

claims_glm <- glm(formula = total_claims ~ fuel_price + GDP + avg_car_age, family = poisson, data = frequency_by_quarters)
summary(claims_glm)
print(anova(claims_glm, test="Chi"))

par(mfrow=c(2,2))
plot(claims_glm)

# Randomly allocate 70% of data into the training set and 30% of the data into the testing set
random_split <- runif(nrow(claim_frequency_data))
split = rep(0, nrow(claim_frequency_data))
counter = 1
for (num in random_split) {
  if (num >= 0.3) split[counter] = 0 else split[counter] = 1
  counter = counter + 1
}
claim_frequency_train_data <- claim_frequency_data[split==0, ]
claim_frequency_test_data <- claim_frequency_data[split==1, ]

claims.glm <- glm(formula = total_claims ~ fuel_price + GDP + avg_car_age, family = poisson, data = claim_frequency_train_data)
summary(claims.glm)
print(anova(claims.glm, test="Chi"))

par(mfrow=c(2,2))
plot(claims.glm)

claim_frequency.predict <- exp(predict.glm(claims.glm, data.frame(fuel_price = claim_frequency_test_data$fuel_price, 
                                                                  GDP = claim_frequency_test_data$GDP,
                                                                  avg_car_age = claim_frequency_test_data$avg_car_age)))
# Check similarity between prediction and true value
summary(claim_frequency_test_data$total_claims)
summary(claim_frequency.predict)

# Find the Mean Squared Error and the Root Mean Squared Error of the Model
MSE = mean((claim_frequency_test_data$total_claims-claim_frequency.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)
plot(claim_frequency.predict, claim_frequency_test_data$total_claims)
