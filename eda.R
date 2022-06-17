library(dplyr)
library(ggplot2)
library(ozmaps)
library(sf)

data <- read.csv("3142 Assignment Data.csv")
View(data)
summary(data)

#add a column of use time = accident year - manufacture year
accident_year <- format(as.Date(data$accident_month),format="%Y")
used_time <- as.numeric(accident_year) - data$year_of_manufacture
data <- data.frame(data,used_time)
View(data)

#remove claims exceed the insured
upper <- data$sum_insured
data_no_extra <- subset(data, data$total_claims_cost <= upper & 
                          data$total_claims_cost >= 0 | is.na(claim_loss_date))

#remove error
data_no_false <- subset(data_no_extra, used_time >= 0)

clean_data <- data_no_false

#group by state to compare the claim costs and frequency in different states
data_by_state <- clean_data %>%
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

#plotting map of AU
ozmap()
sf_oz <- ozmap_data("states")
ggplot(data = sf_oz) + geom_sf()

cases <- c(874,1756,683,1293,2726,105,2,14,0)
sf_oz$cases <- cases
ggplot(data = sf_oz, aes(fill = cases)) + 
  geom_sf() +
  labs(title="claim frequency by states") +
  scale_fill_gradient(low ="lightblue", high = "purple")


#group by vehicle classes
data_by_class <- clean_data %>%
  group_by(vehicle_class) %>%
  summarise(avg_used_time = mean(used_time),
            avg_claims = mean(total_claims_cost,na.rm=TRUE),
            avg_sum_insured = mean(sum_insured),
            claim_freq = sum(!is.na(total_claims_cost))) 
View(data_by_class)
