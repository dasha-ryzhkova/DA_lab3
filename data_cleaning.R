install.packages('stringr')
library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')
data$children[is.na(data$children)] = round(mean(data$children, na.rm = TRUE),0)

data <- mutate(data, all_guests = adults + children + babies)   
data <- mutate(data, all_children = children + babies)  
data <-  filter(data, all_guests != 0) 

data <- data %>% select(-c(agent, company))

data <- mutate(data, stays_in_nights = stays_in_weekend_nights + stays_in_week_nights)  

data %>% group_by(stays_in_nights) %>% filter(stays_in_nights == 0) %>%   print(n = Inf)

data <-  filter(data, stays_in_nights != 0)

data$required_car_parking_spaces[data$required_car_parking_spaces > 2] <- 2

data <-  filter(data, adr > 0)
data <-  filter(data, adr < 400)

data <-  filter(data, babies < 5)
data <-  filter(data, children < 10)

data <-  filter(data, adults < 5)

data <-  filter(data, lead_time  < 700)

data <-  filter(data, country != 'NULL')

colSums(is.na(data))


other <- c('October', 'November', 'December', 'January', 'February', 'March', 'April')

warm <- c('May', 'June', 'July', 'August', 'September')

seasons <- c(warm, other)

data <- data %>% mutate(season = case_when(
  arrival_date_month %in% warm ~ 1,
  arrival_date_month %in% other ~ 0
))

data <- data %>% mutate(with_children = case_when(
  all_children > 0 ~ 1, #yes
  all_children == 0  ~ 0 #no
))


data <- data %>% mutate(lead_time_case = case_when(
  lead_time < 300  ~ 0, # short
  lead_time >= 300  ~ 1 #long
))

data <- data %>% mutate(market_segment_b = ifelse(market_segment == 'Online TA', 1, 0),
                        distribution_channel_b = ifelse(distribution_channel == 'TA/TO', 1, 0),
                        deposit_type_b = ifelse(deposit_type == 'No Deposit', 0, 1),
                        with_meal = ifelse(meal == 'SC' | meal == 'Undefined', 0, 1))

#rm()
