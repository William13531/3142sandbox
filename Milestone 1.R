library(dplyr)
library(ggplot2)

data <- read.csv("ACTL31425110AssignmentData2022.csv")
View(data)
summary(data)

#add a column of use time = accident year - manufacture year
accident_year <- format(as.Date(data$嚜瘸ccident_month),format="%Y")

used_time <- as.numeric(accident_year) - data$year_of_manufacture
data <- data.frame(data,used_time)
View(data)

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

#Check if there is claims larger than amount insured
sum(data$total_claims_cost > data$sum_insured, na.rm=TRUE)

#Group by car age, one data has car age of -1 
data_by_carage <- data %>%
  group_by(used_time) %>%
  summarise(avg_claims = mean(total_claims_cost,na.rm=TRUE),
            avg_sum_insured = mean(sum_insured),
            claim_freq = sum(!is.na(total_claims_cost)),
            number_insured = n())

view(data_by_carage)

# plot insurance data with respect to Car Age
claim_prob <- data_by_carage$claim_freq/data_by_carage$number_insured

install.packages("gridExtra")
require(gridExtra)

plot1 <- ggplot(data=data_by_carage, aes(x=used_time,y=claim_freq)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Number of Claims", x="Used Time", title="Number of Claims by Car Age")

plot2 <- ggplot(data=data_by_carage, aes(x=used_time,y=number_insured)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Number of Insured", x="Used Time", title="Number of Insured by Car Age")

plot3 <- ggplot(data=data_by_carage, aes(x=used_time,y=claim_prob)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Probability of Claims", x="Used Time", title="Probability of Claims by Car Age")

plot4 <- ggplot(data=data_by_carage, aes(x=used_time,y=avg_claims)) +
  geom_bar(stat="identity",width=0.5) +
  labs(y="Average Claims Cost", x="Used Time", title="Average Claims by Car Age")

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
