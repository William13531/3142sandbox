library(dplyr)
library(ggplot2)

data <- read.csv("3142 Assignment Data.csv")
View(data)
summary(data)

#add a column of use time = accident year - manufacture year
accident_year <- format(as.Date(data$accident_month),format="%Y")
used_time <- as.numeric(accident_year) - data$year_of_manufacture
data <- data.frame(data,used_time)
View(data)

#group by state to compare the claim costs and frequency in different states
data_by_state <- data %>%
  group_by(risk_state_name) %>%
  summarise(avg_used_time = mean(used_time),
            avg_claims = mean(total_claims_cost,na.rm=TRUE),
            avg_sum_insured = mean(sum_insured),
            claim_freq = sum(!is.na(total_claims_cost))) 
            #not sure about the codes for claim_freq
            #only want to show observations that have a claim cost
View(data_by_state)
summary(data_by_state)

#plotting
ggplot(data) +
  geom_boxplot(aes(x = risk_state_name , y = total_claims_cost)) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(y="Total Claims Cost", x="State", title="Claims Cost by State")

ggplot(data=data_by_state, aes(x=risk_state_name,y=avg_claims)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Claims Cost", x="State", title="Average Claims by State")

ggplot(data=data_by_state, aes(x=risk_state_name,y=avg_sum_insured)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Insured", x="State", title="Average Insured by State")

#group by vehicle classes
data_by_class <- data %>%
  group_by(vehicle_class) %>%
  summarise(avg_used_time = mean(used_time),
            avg_claims = mean(total_claims_cost,na.rm=TRUE),
            avg_sum_insured = mean(sum_insured),
            claim_freq = sum(!is.na(total_claims_cost))) 
View(data_by_class)
