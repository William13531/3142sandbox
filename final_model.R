# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")
library(sf)
# library("recipes")
library(caret)
library(boot)
library(splines)
library(mgcv)
library(leaps)
library(glmnet)

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

# specify the cross-validation method
# ctrl <- trainControl(method = "cv", number = 5)

# -----------------------------------------------------------------------------

# Import and modify data here

claim_data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
m1_mthly_data <- read.csv("m1mthly.csv")
cpi_qtrly_data <- read.csv("qtrlycpi.csv")
exchange_rate_mthly_data <- read.csv("exchange_rate.csv")
fuel_qtrly_data <- read.csv("fuel.csv")
gdp_qtrly_data <- read.csv("gdp.csv")

# Added a column to include the quarter of the year for each record.
claim_data['quarter_in_year'] <- as.yearqtr(claim_data$accident_month, format="%Y-%m-%d")
# Added another column to include the month of the year for each record.
claim_data['month_in_year'] <- as.yearmon(claim_data$accident_month, format="%Y-%m-%d")
# Added a column to include the age of the car
claim_data['car_age'] <- as.numeric(format(as.Date(claim_data$accident_month),format="%Y")) - claim_data$year_of_manufacture

m1_mthly_data['month_in_year'] <- as.yearmon(m1_mthly_data$DATE, format="%Y-%m-%d")
m1_mthly_data = subset(m1_mthly_data, select=-c(DATE))
m1_mthly_data <- m1_mthly_data %>% rename(M1 = MANMM101AUM189S)

cpi_qtrly_data['quarter_in_year'] <- as.yearqtr(cpi_qtrly_data$Time, format="%b-%y")
cpi_qtrly_data = subset(cpi_qtrly_data, select=-c(Time,quarterly_change,yearly_change))

exchange_rate_mthly_data['month_in_year'] <- as.yearmon(exchange_rate_mthly_data$Series.ID, format="%d-%b-%y")
exchange_rate_mthly_data = subset(exchange_rate_mthly_data, select=c(month_in_year, FXRTWI, FXRJY, FXREUR)) # consider jpy, euro?

fuel_qtrly_data['quarter_in_year'] <- as.yearqtr(fuel_qtrly_data$time, format="%Y Q%q")
fuel_qtrly_data = subset(fuel_qtrly_data, select=c(quarter_in_year, fuel_price_index))

gdp_qtrly_data['quarter_in_year'] <- as.yearqtr(gdp_qtrly_data$Time, format="%b-%y")
gdp_qtrly_data = subset(gdp_qtrly_data, select=-c(Time, Quarterly_GDP_Growth))

claim_data <- merge(claim_data, cpi_qtrly_data, by="quarter_in_year", all.x=TRUE)
claim_data <- merge(claim_data, m1_mthly_data, by="month_in_year", all.x=TRUE)
claim_data <- merge(claim_data, exchange_rate_mthly_data, by="month_in_year", all.x=TRUE)
claim_data <- merge(claim_data, fuel_qtrly_data, by="quarter_in_year", all.x=TRUE)
claim_data <- merge(claim_data, gdp_qtrly_data, by="quarter_in_year", all.x=TRUE)

# -----------------------------------------------------------------------------

# Data cleaning 
claim_data <- claim_data %>%
  # remove entries where the car has a negative age
  filter(car_age >= 0)

# -----------------------------------------------------------------------------

# We first model the claim size with GLM.
claim_size_data = subset(claim_data, total_claims_cost != "NA" & total_claims_cost > 0)

claim_size_data_choose_predictors = subset(claim_data, total_claims_cost != "NA" & total_claims_cost > 0, 
                         select = -c(quarter_in_year, month_in_year, accident_month, term_start_date, term_expiry_date, policy_id, claim_loss_date, 
                                     vehicle_class, risk_state_name, risk_postcode))

# Use best-subset selection to choose predictors.
subset <- regsubsets(total_claims_cost~., claim_size_data_choose_predictors, nvmax = 10, method = "exhaustive")
total_claims_cost_subset <- summary(subset)
par(mfrow = c(2,2))
plot(seq(1,10,1), total_claims_cost_subset$rss, main="Residual Sum of Squares")
plot(seq(1,10,1), total_claims_cost_subset$adjr2, main="Adjusted R^2")
plot(seq(1,10,1), total_claims_cost_subset$cp, main="Cp")
plot(seq(1,10,1), total_claims_cost_subset$bic, main="Bayesian Information Criterion")

# Hence we choose 4 predictors for total_claims_cost which are policy_tenure, sum_insured,
# year_of_manufacture and price_index.

# Training/Testing Sets for Validation
# Randomly allocate 70% of data into the training set and 30% of the data into the testing set
random_split <- runif(nrow(claim_size_data))
split = rep(0, nrow(claim_size_data))
counter = 1
for (num in random_split) {
  if (num >= 0.3) split[counter] = 0 else split[counter] = 1
  counter = counter + 1
}
claim_size_train_data <- claim_size_data[split==0, ]
claim_size_test_data <- claim_size_data[split==1, ]

# Run a GLM with the four predictors and a poisson error with a log link function.
# Also tried others (Gamma error with log/inverse link)

claim_size.glm <- glm(total_claims_cost ~ policy_tenure+sum_insured+year_of_manufacture+price_index,
                      data=claim_size_train_data,
                      family=poisson(link="log"))
summary(claim_size.glm)

# 10-fold cross validation
(cv.err <- cv.glm(claim_size_train_data, claim_size.glm, K = 10)$delta[1])

print(summary(claim_size.glm, corr=T))
print(anova(claim_size.glm, test="Chi"))
par(mfrow=c(2,2))
plot(claim_size.glm)

# analysis of residuals
par(mfrow=c(2,2))
plot(fitted(claim_size.glm),xlab="observed amounts",ylab="fitted values",main="Observed vs Predicted",pch=20)
abline(0,1,col='red')
plot(fitted(claim_size.glm),resid(claim_size.glm,type="deviance"),xlab="fitted values",ylab="deviance residuals",main="Fitted vs Residuals",pch=20)
abline(0,0,col='red')
qqnorm(resid(claim_size.glm,type="pearson"),xlab="quantiles of Std Normal",ylab="Pearson residuals",pch=20)
qqline(resid(claim_size.glm),col='red')

# Predict total_claims_cost for the testing set
claim_size.predict <- exp((predict.glm(claim_size.glm, data.frame(price_index = claim_size_test_data$price_index,
                                                                 year_of_manufacture = claim_size_test_data$year_of_manufacture,
                                                                 policy_tenure = claim_size_test_data$policy_tenure,
                                                                 sum_insured = claim_size_test_data$sum_insured))))
summary(claim_size_test_data$total_claims_cost)
summary(claim_size.predict)

# Find the Mean Squared Error, Root Mean Squared Error and Mean Absolute Error of the Model
MSE = mean((claim_size_test_data$total_claims_cost-claim_size.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)
MAE = mean(abs(claim_size_test_data$total_claims_cost-claim_size.predict))
print(MAE)
plot(claim_size.predict, claim_size_test_data$total_claims_cost)
abline(0,1,col="red")

# Adding predicted total_claims_cost to the claim_size_test_data
claim_size_test_data["predicted_claims_cost"] = claim_size.predict

# by quarter
claim_size_test_by_quarters <- claim_size_test_data %>%
  group_by(quarter_in_year) %>%
  summarise(total_claims = n(),
            actual_avg_claim_cost = mean(total_claims_cost), 
            predicted_avg_claim_cost = mean(predicted_claims_cost),
            percent_error = abs((actual_avg_claim_cost-predicted_avg_claim_cost)/actual_avg_claim_cost))

#show(claim_size_test_by_quarters)
mean(claim_size_test_by_quarters$percent_error)
cor(claim_size_test_by_quarters$actual_avg_claim_cost, claim_size_test_by_quarters$predicted_avg_claim_cost)
MSE_quarters = mean((claim_size_test_by_quarters$actual_avg_claim_cost - claim_size_test_by_quarters$predicted_avg_claim_cost)^2)
RMSE_quarters = sqrt(MSE_quarters)
MAE_quarters = mean(abs(claim_size_test_by_quarters$actual_avg_claim_cost-claim_size_test_by_quarters$predicted_avg_claim_cost))
print(MSE_quarters)
print(RMSE_quarters)
print(MAE_quarters)

# Plot the predicted quarterly total claim costs against recorded.
plot(claim_size_test_by_quarters$quarter_in_year, claim_size_test_by_quarters$actual_avg_claim_cost, type="l", col="red",
     xlab="Quarter in Year", ylab="Average Claim Cost ($)", main="Predicted and Actual Average Claim Costs for the Test Set by Quarters")
lines(claim_size_test_by_quarters$quarter_in_year, claim_size_test_by_quarters$predicted_avg_claim_cost, col="blue")
legend(2016.5, 10500, legend=c("Actual", "Predicted"),
       col=c("red", "blue"), lty=1:1, cex=0.85, bty = "n")

# by month
claim_size_test_by_months <- claim_size_test_data %>%
  group_by(month_in_year) %>%
  summarise(total_claims = n(),
            actual_avg_claim_cost = mean(total_claims_cost), 
            predicted_avg_claim_cost = mean(predicted_claims_cost),
            percent_error = abs((actual_avg_claim_cost-predicted_avg_claim_cost)/actual_avg_claim_cost))

#show(claim_size_test_by_months)
mean(claim_size_test_by_months$percent_error)
cor(claim_size_test_by_months$actual_avg_claim_cost, claim_size_test_by_months$predicted_avg_claim_cost)
MSE_months = mean((claim_size_test_by_months$actual_avg_claim_cost - claim_size_test_by_months$predicted_avg_claim_cost)^2)
RMSE_months = sqrt(MSE_months)
MAE_months = mean(abs(claim_size_test_by_months$actual_avg_claim_cost-claim_size_test_by_months$predicted_avg_claim_cost))
print(MSE_months)
print(RMSE_months)
print(MAE_months)

# Plot the predicted monthly total claim costs against recorded.
plot(claim_size_test_by_months$month_in_year, claim_size_test_by_months$actual_avg_claim_cost, type="l", col="red",
     xlab="Month in Year", ylab="Average Claim Cost ($)", main="Predicted and Actual Average Claim Costs for the Test Set by Months")
lines(claim_size_test_by_months$month_in_year, claim_size_test_by_months$predicted_avg_claim_cost, col="blue")
legend(2016.5, 14500, legend=c("Actual", "Predicted"),
       col=c("red", "blue"), lty=1:1, cex=0.85, bty = "n")
