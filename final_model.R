# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(lubridate)
library(ggplot2)
library(tidyverse)
library("zoo")
library(sf)
library("recipes")
library(caret)
library(boot)
library(splines)
library(mgcv)
library(leaps)
library(glmnet)
library(MASS)
library(randomForest)
library(gbm)
library("formattable")

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

# specify the cross-validation method
# ctrl <- trainControl(method = "cv", number = 5)

# Helper function to solve for positive integer roots.
solve_eq <- function(num) {
  for (i in 1:highest_degree) {
    for (j in 1:highest_degree) {
      for (k in 1:highest_degree) {
        for (l in 1:highest_degree) {
          if ((i-1)*highest_degree^3 + (j-1)*highest_degree^2 + (k-1)*highest_degree + l == num) {
            return (c(i,j,k,l))
          }
        }
      }
    }
  }  
}

# -----------------------------------------------------------------------------

# Declare static variables here
lockdown_start = as.yearmon("2020-03-01", format="%Y-%m")
highest_degree = 3

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
# Added a column to include the month only.
# claim_data['month'] <- month.name[month(claim_data$accident_month)]
# Added a date for any accidents
# claim_data['accident_date'] <- date(ymd_hms(claim_data$claim_loss_date, tz="NZ"))

claim_data <- claim_data %>%
  # 1 denotes lockdown, 0 denotes not in lockdown.
  # mutate(lockdown = if_else(claim_data$month_in_year - lockdown_start >= 0, "yes", 'no')) %>%
  mutate(lockdown = if_else(claim_data$month_in_year - lockdown_start >= 0, 1, 0)) %>%
  # 1 denotes accident, 0 denotes no accident.
  # mutate(accident = if_else(is.na(claim_data$claim_loss_date), "no", "yes"))
  mutate(accident = if_else(is.na(claim_data$claim_loss_date), 0, 1))

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
                         select = -c(quarter_in_year, month_in_year, accident_month, term_start_date, term_expiry_date, 
                                     policy_id, claim_loss_date, vehicle_class, exposure, risk_postcode, risk_state_name, accident, car_age))

# Use best-subset selection to choose predictors.
total_claims_cost_subset <- summary(regsubsets(total_claims_cost~., claim_size_data_choose_predictors, nvmax = 10, method = "exhaustive"))
par(mfrow = c(2,2))
plot(seq(1,10,1), total_claims_cost_subset$rss, main="Residual Sum of Squares")
plot(seq(1,10,1), total_claims_cost_subset$adjr2, main="Adjusted R^2")
plot(seq(1,10,1), total_claims_cost_subset$cp, main="Cp")
plot(seq(1,10,1), total_claims_cost_subset$bic, main="Bayesian Information Criterion")

# Hence we choose 4 predictors for total_claims_cost which are policy_tenure, sum_insured,
# year_of_manufacture and price_index.

# boost <- gbm(total_claims_cost ~ ., distribution = "gaussian", data = claim_size_data_choose_predictors, n.trees = 1000, shrinkage = 0.55)
# summary(boost)

# Choose the degree of polynomial for the predictors.
cv.error <- rep(0,highest_degree^4)
for (i in 1:highest_degree) {
  for (j in 1:highest_degree) {
    for (k in 1:highest_degree) {
      for (l in 1:highest_degree) {
        fiti <- glm(total_claims_cost ~
                      poly(policy_tenure, i, raw = TRUE, simple = T) +
                      poly(sum_insured, j, raw = TRUE, simple = T) +
                      poly(year_of_manufacture, k, raw = TRUE, simple = T) +
                      poly(price_index, l, raw = TRUE, simple = T),
                    data=claim_size_data,
                    family=Gamma(link="log"))
        # Added a penalty term to avoid unnecessarily high degrees of polynomials
        cv.error[(i-1)*highest_degree^3 + (j-1)*highest_degree^2 + (k-1)*highest_degree + l] <- cv.glm(claim_size_data, fiti, K=10)$delta[1] + (i+j+k+l) * 1000000
      }
    }
  }
}
plot(seq(1,highest_degree^4),cv.error)
match(min(cv.error), cv.error)

(degs <- solve_eq(match(min(cv.error), cv.error)))
(pct_error_reduction_poly = percent(min(cv.error)/cv.error[1] - 1))
# The degrees of polynomials chosen leading to the lowest cross-validation prediction error are
# 2 for policy_tenure, 2 for sum_insured, 2 for year_of_manufacture and 1 for price_index. 
# This reduces the prediction error by more than 90% comparing with all degree-one polynomials for Gamma.
# This reduces the prediction error by more than 15% comparing with all degree-one polynomials for Poisson.

# Trying spline regression
rss <- rep(0,15)
# colors <- c("red", "blue", "green", "brown", "orange", "purple",
            # "pink", "yellow", "violet", "magenta")
for (i in 4:18) {
  fiti <- glm(total_claims_cost ~ bs(policy_tenure + sum_insured + year_of_manufacture + price_index, df=i),
              data=claim_size_data,
              family=Gamma(link="log"))
              # family=Gamma(link="log"))
  rss[i-3] <- sum(fiti$residuals^2)
}
plot(seq(1,15), rss)
(pct_rss_reduction_spline = percent(min(rss)/rss[1] - 1))
# Only trivial improvement with the introduction of spline regression

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

# ridge_reg = glmnet(as.matrix(claim_size_train_data), claim_size_train_data$total_claims_cost, nlambda = 25, alpha = 0, family = 'gaussian', lambda = 0.001)

# Trial with random forest
bag <- randomForest(total_claims_cost ~ ., data = claim_size_data_choose_predictors[split==0, ], importance = TRUE)
pred.bag = predict(bag, newdata = claim_size_data_choose_predictors[split==1, ])
(pred.bag.mse <- mean((claim_size_data_choose_predictors[split==1, ]$total_claims_cost - pred.bag)^2))
(pred.bag.rmse = sqrt(pred.bag.mse))
summary(claim_size_data_choose_predictors[split==1, ]$total_claims_cost)
summary(pred.bag)

importance(bag)

# Run a GLM with the four predictors and a gamma error with a log link function.
# Also tried others (Gamma error with log/inverse link)

# 10-fold cross validation with GLM
(gamma_claim_size_cv.err <- cv.glm(claim_size_data, glm(total_claims_cost ~
                                                            poly(policy_tenure, degs[1], raw = TRUE, simple = T) +
                                                            poly(sum_insured, degs[2], raw = TRUE, simple = T) +
                                                            poly(year_of_manufacture, degs[3], raw = TRUE, simple = T) +
                                                            poly(price_index, degs[4], raw = TRUE, simple = T),
                                                          data=claim_size_data,
                                                          family=Gamma(link="log")), K = 10)$delta[1])

# Training / testing set
claim_size.glm <- glm(total_claims_cost ~ #vehicle_class + month + lockdown +
                        poly(policy_tenure, degs[1], raw = TRUE, simple = T) +
                        poly(sum_insured, degs[2], raw = TRUE, simple = T) +
                        poly(year_of_manufacture, degs[3], raw = TRUE, simple = T) +
                        poly(price_index, degs[4], raw = TRUE, simple = T),
                      data=claim_size_train_data,
                      family=Gamma(link="log"))

summary(claim_size.glm)

# nb.glm <- glm.nb(total_claims_cost ~
#                         poly(policy_tenure, 2, raw = TRUE, simple = T) +
#                         poly(sum_insured, 2, raw = TRUE, simple = T) +
#                         poly(year_of_manufacture, 1, raw = TRUE, simple = T) +
#                         poly(price_index, 1, raw = TRUE, simple = T),
#                       data=claim_size_train_data, link = "log")
# summary(nb.glm)

# print(summary(claim_size.glm, corr=T))
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

# par(mfrow=c(2,2))
# plot(fitted(nb.glm),xlab="observed amounts",ylab="fitted values",main="Observed vs Predicted",pch=20)
# abline(0,1,col='red')
# plot(fitted(nb.glm),resid(claim_size.glm,type="deviance"),xlab="fitted values",ylab="deviance residuals",main="Fitted vs Residuals",pch=20)
# abline(0,0,col='red')
# qqnorm(resid(nb.glm,type="pearson"),xlab="quantiles of Std Normal",ylab="Pearson residuals",pch=20)
# qqline(resid(nb.glm),col='red')

# Predict total_claims_cost for the testing set
# nb.predict <- exp(predict(nb.glm,data.frame(price_index = claim_size_test_data$price_index,
#                             lockdown = claim_size_test_data$lockdown,
#                             vehicle_class = claim_size_test_data$vehicle_class,
#                             month = claim_size_test_data$month,
#                             year_of_manufacture = claim_size_test_data$year_of_manufacture,
#                             # car_age = claim_size_test_data$car_age,
#                             policy_tenure = claim_size_test_data$policy_tenure,
#                             sum_insured = claim_size_test_data$sum_insured)))

claim_size.predict <- exp((predict.glm(claim_size.glm, data.frame(price_index = claim_size_test_data$price_index,
                                                                  # lockdown = claim_size_test_data$lockdown,
                                                                  # vehicle_class = claim_size_test_data$vehicle_class,
                                                                  # month = claim_size_test_data$month,
                                                                  year_of_manufacture = claim_size_test_data$year_of_manufacture,
                                                                  # car_age = claim_size_test_data$car_age,
                                                                  policy_tenure = claim_size_test_data$policy_tenure,
                                                                  sum_insured = claim_size_test_data$sum_insured))))
summary(claim_size_test_data$total_claims_cost)
summary(claim_size.predict)
# summary(nb.predict)

MSE = mean((claim_size_test_data$total_claims_cost-claim_size.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)
MAE = mean(abs(claim_size_test_data$total_claims_cost-claim_size.predict))
print(MAE)
plot(claim_size.predict, claim_size_test_data$total_claims_cost, main="Predicted vs Actual")
abline(0,1,col="red")

# Adding predicted total_claims_cost to the claim_size_test_data
claim_size_test_data["predicted_claims_cost"] = claim_size.predict
# claim_size_test_data["predicted_claims_cost"] = nb.predict

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
plot(claim_size_test_by_quarters$quarter_in_year, 
     claim_size_test_by_quarters$actual_avg_claim_cost, 
     type="l", col="red",
     xlab="Quarter in Year", ylab="Average Claim Cost ($)", 
     main="Predicted and Actual Average Claim Costs for the Test Set by Quarters",
     cex.lab=1.3, cex.main=1.7)
lines(claim_size_test_by_quarters$quarter_in_year, claim_size_test_by_quarters$predicted_avg_claim_cost, col="blue")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "blue"), lty=1:1, cex=1.1, bty = "n")

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
plot(claim_size_test_by_months$month_in_year, 
     claim_size_test_by_months$actual_avg_claim_cost, 
     type="l", col="red",
     xlab="Month in Year", ylab="Average Claim Cost ($)", 
     main="Predicted and Actual Average Claim Costs for the Test Set by Months",
     cex.lab=1.3, cex.main=1.7)
lines(claim_size_test_by_months$month_in_year, claim_size_test_by_months$predicted_avg_claim_cost, col="blue")
legend("topleft",legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=1.1, bty = "n")

# -----------------------------------------------------------------------------

# We then model the claim frequency with GLM.

# Consider all cars, including those without any claims.
data_by_months <- claim_data %>%
  group_by(month_in_year) %>%
  summarise(total_car = n(),
            avg_claim_costs = mean(total_claims_cost, na.rm=TRUE),
            avg_car_age = mean(car_age),
            total_exposure = sum(exposure),
            avg_policy_tenure = mean(policy_tenure))

# Consider only claims with positive total_claims_cost
claims_by_months <- claim_data %>%
  filter(total_claims_cost > 0) %>%
  group_by(month_in_year) %>%
  summarise(total_claims = n())

# Merge two tables
frequency_by_months <- merge(data_by_months, claims_by_months, by="month_in_year")

# Model claim frequency with the average annual number of claims for a car in that quarter
frequency_by_months["avg_frequency"] <- c(frequency_by_months$total_claims/frequency_by_months$total_exposure)

frequency_by_months['quarter_in_year'] <- as.yearqtr(frequency_by_months$month_in_year, format="%m-%Y")

frequency_by_months["fuel_price"] <- fuel_qtrly_data$fuel_price_index
frequency_by_months <- merge(frequency_by_months, gdp_qtrly_data, by="quarter_in_year", all.x=TRUE)

frequency_by_months <- frequency_by_months %>%
  mutate(lockdown = if_else(frequency_by_months$month_in_year - lockdown_start >= 0, 1, 0))

# Regression
# Check for normal distribution
par(mfrow=c(1,1))
hist(frequency_by_months$avg_frequency)
hist(frequency_by_months$total_claims)

# Check for linearity
plot(frequency_by_months$avg_frequency ~ frequency_by_months$fuel_price)
plot(frequency_by_months$total_claims ~ frequency_by_months$fuel_price)

# Check for correlation
cor(frequency_by_months$fuel_price, frequency_by_months$GDP) 

# Model claim frequency
claim_frequency_data = subset(frequency_by_months)
# claim_frequency_data_choose_predictors = subset(frequency_by_months, select = -c(quarter_in_year, month_in_year, total_exposure, total_claims, total_car))
claim_frequency_data_choose_predictors = subset(frequency_by_months, select = -c(quarter_in_year, month_in_year, avg_frequency, avg_claim_costs))

# Best subset selection
# Use best-subset selection to choose predictors.
claims_frequency_subset <- summary(regsubsets(total_claims~., claim_frequency_data_choose_predictors, nvmax = 5, method = "exhaustive"))

par(mfrow = c(2,2))
plot(seq(1,5,1), claims_frequency_subset$rss, main="Residual Sum of Squares")
plot(seq(1,5,1), claims_frequency_subset$adjr2, main="Adjusted R^2")
plot(seq(1,5,1), claims_frequency_subset$cp, main="Cp")
plot(seq(1,5,1), claims_frequency_subset$bic, main="Bayesian Information Criterion")

freq.cv.err <- rep(0,10)
for (i in 1:100) {
  freq.cv.err[i] <- cv.glm(frequency_by_months, glm(total_claims ~ total_car + avg_car_age,
                                                 data = frequency_by_months, family= "poisson"), K = 10)$delta[1] 
}
mean(freq.cv.err)

random_split <- runif(nrow(claim_frequency_data))
split = rep(0, nrow(claim_frequency_data))
counter = 1
for (num in random_split) {
  if (num >= 0.3) split[counter] = 0 else split[counter] = 1
  counter = counter + 1
}

claim_frequency_train_data <- claim_frequency_data[split==0, ]
claim_frequency_test_data <- claim_frequency_data[split==1, ]

# freq.glm <- glm.nb(formula = total_claims ~ total_car + avg_car_age, data = claim_frequency_train_data)
freq.glm <- glm(formula = total_claims ~ total_car + avg_car_age, family="poisson"(link="log"), data = claim_frequency_train_data)
# freq.glm <- glm(formula = total_claims ~ total_car + avg_car_age, family="quasipoisson"(link="log"), data = claim_frequency_train_data)

summary(freq.glm)
print(anova(freq.glm, test="Chi"))

claim_frequency.predict <- exp(predict.glm(freq.glm, data.frame(total_car = claim_frequency_test_data$total_car,
                                                                avg_car_age = claim_frequency_test_data$avg_car_age,
                                                                fuel_price = claim_frequency_test_data$fuel_price,
                                                                GDP = claim_frequency_test_data$GDP)))

summary(claim_frequency.predict)
summary(claim_frequency_test_data$total_claims)

MSE = mean((claim_frequency_test_data$total_claims-claim_frequency.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)
MAE = mean(abs(claim_frequency_test_data$total_claims-claim_frequency.predict))
print(MAE)

(claim_freq_pct_error = mean(abs((claim_frequency.predict-claim_frequency_test_data$total_claims)/claim_frequency_test_data$total_claims)))

boxplot(claim_frequency_test_data$total_claims, claim_frequency.predict, 
        main = "Boxplots for Comparison between Actual and Predicted Claims in a Month",
        names = c("Actual", "Predicted"),
        col = c("lightblue","lightblue4"),
        horizontal = TRUE,
        ylim = c(80,160),
        cex.main=1.5, cex.lab=1.3,
        xlab = "Number of Claims in a Month")

ggplot(data=frequency_by_months) +
  geom_point(aes(x=avg_claim_costs, y=total_claims), size = 1.5, color="red", group = 1) +
  geom_smooth(aes(x=avg_claim_costs, y=total_claims), method="gam") +
  ggtitle("Monthly Average Claim Cost and Number of Claims") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Monthly Average Claim Cost", y="Monthly Number of Claims")

cor(frequency_by_months$avg_claim_costs, frequency_by_months$total_claims)
claim_cost_freq <- lm(total_claims ~ avg_claim_costs, data=frequency_by_months)
print(anova(claim_cost_freq, test="Chi"))
summary(claim_cost_freq)

# Result of model for claim cost of a car
predict_claim_cost_for_a_car <- function(pt, si, ym, pi) {
  esm = 4.351 * 10^(1) - 1.259 * 10^(-2) * pt + 1.168 * 10^(-5) * si^2 - 1.274*10^(-11)*si - 1.977 * 10^(-2) * ym + 4.139 * 10^(-2) * pi
  return(esm)
}

# Result of model for the monthly number of claims
predict_mthly_no_of_claims <- function(tnc, aca) {
  esm = exp(3.433 + 1.211 * 10^(-4) * tnc - 1.107 * 10^(-1) * aca)
  return (esm)
}

# # Run logistic regression
# random_split <- runif(nrow(claim_data))
# split = rep(0, nrow(claim_data))
# counter = 1
# for (num in random_split) {
#   if (num >= 0.3) split[counter] = 0 else split[counter] = 1
#   counter = counter + 1
# }
# 
# claim_frequency_data = subset(claim_data, select=-c(vehicle_class, risk_state_name, year_of_manufacture, 
#                                                     risk_postcode, total_claims_cost, claim_loss_date, 
#                                                     accident_month, quarter_in_year, month_in_year, 
#                                                     term_start_date, term_expiry_date, policy_id,
#                                                     FXRTWI, FXRJY, FXREUR, price_index, M1))
# claim_frequency_train_data <- claim_frequency_data[split==0, ]
# claim_frequency_test_data <- claim_frequency_data[split==1, ]
# 
# (accident_subset <- summary(regsubsets(accident~., claim_frequency_data, nvmax = 7, method = "exhaustive")))
# par(mfrow = c(2,2))
# plot(seq(1,7,1), accident_subset$rss, main="Residual Sum of Squares")
# plot(seq(1,7,1), accident_subset$adjr2, main="Adjusted R^2")
# plot(seq(1,7,1), accident_subset$cp, main="Cp")
# plot(seq(1,7,1), accident_subset$bic, main="Bayesian Information Criterion")
# 
# # Choose 5 predictors - policy_tenure + sum_insured + exposure + lockdown + car_age
# 
# logistic_model <- glm(accident ~ policy_tenure + sum_insured + exposure + lockdown + car_age,
#                       data = claim_frequency_train_data, family="binomial"(link='logit'))
# summary(logistic_model)
# 
# claim_freq.pred <- predict(logistic_model, claim_frequency_test_data, type = "response")
# claim_freq.pred <- ifelse(claim_freq.pred > 0.5, 1, 0)

# set.seed(120)
# bag <- randomForest(accident ~ ., data = claim_frequency_train_data)
# pred.bag = predict(bag, newdata = claim_frequency_test_data)
# (pred.bag.mse <- mean((claim_size_test_data$total_claims_cost - pred.bag)^2))
# (pred.bag.rmse = sqrt(pred.bag.mse))
# summary(claim_size_test_data$total_claims_cost)
# summary(pred.bag)
