# Import libraries here
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library("zoo")
library(sf)

# -----------------------------------------------------------------------------

# Helper functions
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k)))

# -----------------------------------------------------------------------------

# Import and modify data here

claim_data <- read.csv("ACTL31425110AssignmentData2022.csv", na.strings=c("", "NA"), header=TRUE)
m1_mthly_data <- read.csv("m1mthly.csv")
#m1_qtrly_data <- read.csv("m1qtrly.csv")
cpi_qtrly_data <- read.csv("qtrlycpi.csv")
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

claim_data <- merge(claim_data, cpi_qtrly_data, by="quarter_in_year", all.x=TRUE)
claim_data <- merge(claim_data, m1_mthly_data, by="month_in_year", all.x=TRUE)

# -----------------------------------------------------------------------------

# We model claim size first.
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

claim_size.glm = glm(data=claim_size_train_data, total_claims_cost ~ car_age+M1+sum_insured, family=gaussian(link="identity"))
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
claim_size.predict <- predict.glm(claim_size.glm, data.frame(price_index = claim_size_test_data$price_index, 
  car_age = claim_size_test_data$car_age, M1 = claim_size_test_data$M1, sum_insured = claim_size_test_data$sum_insured))
print(claim_size.predict)

summary(claim_size_test_data$total_claims_cost)
summary(claim_size.predict)

# Find the Mean Squared Error and the Root Mean Squared Error of the Model
MSE = mean((claim_size_test_data$total_claims_cost-claim_size.predict)^2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)

# -----------------------------------------------------------------------------

