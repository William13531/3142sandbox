library(ggplot2)
library(readr)
library(janitor)
library(dplyr)
library(readr)
library(tidyselect)
library(tidyverse)

#import dataset 
data_auto <- read_csv("ACTL31425110AssignmentData2022.csv")
dim(data_auto)
summary(data_auto)

#add a column of use time = accident year - manufacture year
accident_year <- format(as.Date(data_auto$accident_month),format="%Y")
used_time <- as.numeric(accident_year) - data_auto$year_of_manufacture
data <- data.frame(data_auto,used_time)

#remove claims exceed the insured
upper <- data$sum_insured
data_no_extra <- subset(data, data$total_claims_cost <= upper & 
                          data$total_claims_cost >= 0 | is.na(claim_loss_date))
dim(dtat_no_extra)

#remove error
data_no_false <- subset(data_no_extra, used_time >= 0)

clean_data <- data_no_false