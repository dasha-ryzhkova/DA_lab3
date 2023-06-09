install.packages('fixest')
library(fixest)

# data <- data %>% mutate(month_Jan = ifelse(arrival_date_month == 'January', 1, 0),
#                         month_Feb = ifelse(arrival_date_month == 'February', 1, 0),
#                         month_Mar = ifelse(arrival_date_month == 'March', 1, 0),
#                         month_Apr = ifelse(arrival_date_month == 'April', 1, 0),
#                         month_May = ifelse(arrival_date_month == 'May', 1, 0),
#                         month_Jun = ifelse(arrival_date_month == 'June', 1, 0),
#                         month_Jul = ifelse(arrival_date_month == 'July', 1, 0),
#                         month_Aug = ifelse(arrival_date_month == 'August', 1, 0),
#                         month_Sep = ifelse(arrival_date_month == 'September', 1, 0),
#                         month_Oct = ifelse(arrival_date_month == 'October', 1, 0),
#                         month_Nov = ifelse(arrival_date_month == 'November', 1, 0),
#                         month_Dec = ifelse(arrival_date_month == 'December', 1, 0))
# 
# model <- lm(adr ~ month_Jan + month_Feb + month_Mar + month_Apr + month_May +
#               month_Jun + month_Jul + month_Aug + month_Sep + month_Oct + 
#               month_Nov + month_Dec + all_guests + stays_in_nights + 
#               total_of_special_requests + market_segment_b + 
#               distribution_channel_b + deposit_type_b, data = data)
# model
# summary(model)

model <- lm(formula = adr ~ factor(arrival_date_month), data = data)

model1 <- lm(formula = adr ~ factor(arrival_date_month) + all_guests + 
              total_of_special_requests + market_segment_b + 
              distribution_channel_b + deposit_type_b, data = data)
model2 <- lm(formula = adr ~ factor(arrival_date_month) + lead_time + stays_in_nights +
               all_guests + booking_changes + total_of_special_requests + 
               market_segment_b + distribution_channel_b + deposit_type_b + 
               with_meal, data = data)


model_hc1 <- coeftest(model, vcov. = hccm(model_mult, type = "hc1"))
model1_hc1 <- coeftest(model1, vcov. = hccm(model_mult, type = "hc1"))
model2_hc1 <- coeftest(model2, vcov. = hccm(model_mult, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))
ci1 <- coefci(model1, vcov. = hccm(model, type = "hc1"))
ci2 <- coefci(model2, vcov. = hccm(model, type = "hc1"))




stargazer(model, model1, model2, type = "latex",
          title = "Multiple regression", label = "table:evals-reg-mult",
          dep.var.labels = c("Середнє adr"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model1_hc1[, 2], model2_hc1[, 2]),
          ci = TRUE, ci.custom = list(ci, ci1, ci2),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE)


ggplot(data, aes(x=lead_time))+
  geom_histogram()



linearHypothesis(model, names(coef(model))[0:12], vcov. = hccm(model, type = "hc1"))
linearHypothesis(model1, names(coef(model))[0:12], vcov. = hccm(model1, type = "hc1"))
linearHypothesis(model2, names(coef(model))[0:12], vcov. = hccm(model2, type = "hc1"))

linearHypothesis()

