# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")
library(sf)
#library(caret)
library(boot)
library(splines)

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

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

# We first model the claim size with the Gamma distribution.
claim_size_data = subset(claim_data, total_claims_cost != "NA" & total_claims_cost > 0)

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

# claim_size.glm = glm(total_claims_cost ~ price_index+FXRTWI+car_age+sum_insured,
#                     data=claim_size_train_data,
#                     family=gaussian(link="log"))

claim_size.glm <- glm(total_claims_cost ~ price_index+policy_tenure,
                     data=claim_size_train_data,
                     family=Gamma(link="inverse"))

# TODO: Try subset selection to find the best subsets of explanatory variables.

# Checking for higher degree of polynomials
# cv.error <- rep(0,25)
# for (i in 1:5) {
#   for (j in 1:5) {
#     fiti <- glm(total_claims_cost ~ poly(price_index, i, raw = TRUE, simple = T) 
#                 + poly(policy_tenure, j, raw = TRUE, simple = T), 
#                 data=claim_size_data,
#                 family=Gamma(link="inverse"))
#     cv.error[5 * i + j - 5] <- cv.glm(claim_size_data, fiti, K=10)$delta[1]
#   }
# }
# plot(seq(1,25),cv.error)
# min(cv.error)

# Trying spline regression
# rss <- rep(0,10)
# colors <- c("red", "blue", "green", "brown", "orange", "purple",
#              "pink", "yellow", "violet", "magenta")
# for (i in 4:13) {
#   fiti <- glm(total_claims_cost ~ bs(price_index+policy_tenure, df=i), 
#                   data=claim_size_data,
#                   family=Gamma(link="inverse"))
#   rss[i-3] <- sum(fiti$residuals^2)
# }

# m3 <- glm(total_claims_cost ~ ns(price_index+policy_tenure, 3),
#                       data=claim_size_data,
#                       family=Gamma(link="inverse"))
# m3pred <- predict(m3, type = "response")
# claim_size.cv.error <- cv.glm(claim_size_data, m3, K=10)$delta[1]
# plot(claim_size_data$total_claims_cost, m3pred)

# (cv.err.10 <- cv.glm(claim_size_train_data, claim_size.glm, K = 10)$delta)
# 
# claim_size.glm = glm(total_claims_cost ~ price_index+policy_tenure+fuel_price_index+GDP+sum_insured,
#                     data=claim_size_train_data,
#                     family=Gamma(link="log"))

print(summary(claim_size.glm, corr=T))
print(anova(claim_size.glm, test="Chi"))
par(mfrow=c(2,2))
plot(claim_size.glm)

# analysis of residuals
par(mfrow=c(2,2))
plot(fitted(claim_size.glm),xlab="observed amounts",ylab="fitted values",main="Observed vs Predicted",pch=20)
abline(0,1,col='steelblue')
plot(fitted(claim_size.glm),resid(claim_size.glm,type="deviance"),xlab="fitted values",ylab="deviance residuals",main="Fitted vs Residuals",pch=20)
abline(0,0,col='steelblue')
qqnorm(resid(claim_size.glm,type="pearson"),xlab="quantiles of Std Normal",ylab="Pearson residuals",pch=20)
qqline(resid(claim_size.glm))

# predicting total_claims_cost for the test set
claim_size.predict <- 1/predict.glm(claim_size.glm, data.frame(price_index = claim_size_test_data$price_index,
                                                               car_age = claim_size_test_data$car_age,
                                                               M1 = claim_size_test_data$M1,
                                                               GDP = claim_size_test_data$GDP,
                                                               policy_tenure = claim_size_test_data$policy_tenure,
                                                               FXRTWI = claim_size_test_data$FXRTWI,
                                                               fuel_price_index = claim_size_test_data$fuel_price_index,
                                                               sum_insured = claim_size_test_data$sum_insured,
                                                               FXRJY = claim_size_test_data$FXRJY,
                                                               FXREUR = claim_size_test_data$FXREUR))

# claim_size.predict <- exp(predict.glm(claim_size.glm, data.frame(price_index = claim_size_test_data$price_index, 
#                                                                car_age = claim_size_test_data$car_age, 
#                                                                M1 = claim_size_test_data$M1, 
#                                                                GDP = claim_size_test_data$GDP,
#                                                                policy_tenure = claim_size_test_data$policy_tenure,
#                                                                FXRTWI = claim_size_test_data$FXRTWI,
#                                                                fuel_price_index = claim_size_test_data$fuel_price_index,
#                                                                sum_insured = claim_size_test_data$sum_insured,
#                                                                FXRJY = claim_size_test_data$FXRJY,
#                                                                FXREUR = claim_size_test_data$FXREUR)))

#print(claim_size.predict)

summary(claim_size_test_data$total_claims_cost)
summary(claim_size.predict)

# Find the Mean Squared Error and the Root Mean Squared Error of the Model
MSE = mean((claim_size_test_data$total_claims_cost-claim_size.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)
plot(claim_size.predict, claim_size_test_data$total_claims_cost)

(size.cv.err <- cv.glm(claim_size_train_data, claim_size.glm, K=5)$delta)
size.cv.rmse = sqrt(size.cv.err)

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
print(MSE_quarters)
print(RMSE_quarters)

plot(claim_size_test_by_quarters$quarter_in_year, claim_size_test_by_quarters$actual_avg_claim_cost, type="l", col="red",
     xlab="Quarter in Year", ylab="Average Claim Cost ($)", main="Predicted and Actual Average Claim Costs for the Test Set by Quarters")
lines(claim_size_test_by_quarters$quarter_in_year, claim_size_test_by_quarters$predicted_avg_claim_cost, col="blue")
legend(2016.5, 11500, legend=c("Actual", "Predicted"),
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
print(MSE_months)
print(RMSE_months)

plot(claim_size_test_by_months$month_in_year, claim_size_test_by_months$actual_avg_claim_cost, type="l", col="red",
     xlab="Month in Year", ylab="Average Claim Cost ($)", main="Predicted and Actual Average Claim Costs for the Test Set by Months")
lines(claim_size_test_by_months$month_in_year, claim_size_test_by_months$predicted_avg_claim_cost, col="blue")
legend(2016.5, 14500, legend=c("Actual", "Predicted"),
       col=c("red", "blue"), lty=1:1, cex=0.85, bty = "n")
# -----------------------------------------------------------------------------

# We then model claim frequency with a GLM with the Poisson distribution.

# Consider all cars, including those without any claims.
data_by_quarters <- claim_data %>%
  group_by(quarter_in_year) %>%
  summarise(total_car_month = n(),
            avg_car_age = mean(car_age))

# Consider only claims with positive total_claims_cost
claims_by_quarters <- claim_data %>%
  filter(total_claims_cost > 0) %>%
  group_by(quarter_in_year) %>%
  summarise(total_claims = n())

# Merge two tables
frequency_by_quarters <- merge(data_by_quarters, claims_by_quarters, by="quarter_in_year")

# Model claim frequency with the average number of claims for a car in a quarter
frequency_by_quarters["avg_claim_frequency"] <- c(frequency_by_quarters$total_claims/frequency_by_quarters$total_car_month)

frequency_by_quarters["fuel_price"] <- fuel_qtrly_data$fuel_price_index
frequency_by_quarters <- merge(frequency_by_quarters, gdp_qtrly_data, by="quarter_in_year", all.x=TRUE)
  #gdp_qtrly_data$GDP

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
(freq.cv.err <- cv.glm(claim_frequency_train_data, claims.glm, K=5)$delta)

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


