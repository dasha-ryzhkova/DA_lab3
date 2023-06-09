library(stargazer)
library(lmtest)
library(estimatr)
library(car)
library(GGally)
install.packages('car')

ggplot(data, aes(x=lead_time, y=adr)) +
  geom_point() +
  labs(title='lead_time ~ adr')


rm(df)
summary(data$lead_time)
summary(data$adr)

model <- lm(adr ~ lead_time, data = data)


# проаналізуємо відповідні середньоквадратичні відхилення
lead_time.sd <- sd(data$lead_time)
adr.sd <- sd(data$adr)
model$coefficients[2] * lead_time.sd / adr.sd
ci <- coefci(model, vcov. = hccm(model, type = "hc1"))
ci

stargazer(model, type = "latex",
          title = "Regression", label = "table:evals-reg",
          dep.var.labels = c("Середнє adr"),
          ci = TRUE, ci.custom = list(ci),
          se = list(model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"))

df <- data %>% dplyr::select(adr, lead_time, stays_in_nights, all_guests, booking_changes, 
                             total_of_special_requests, arrival_date_month, 
                             market_segment_b, distribution_channel_b,
                             deposit_type_b, with_meal)
ggcorr(df, label = TRUE, 
       method = c("pairwise", "spearman"))
# немає змінних які сильно корелюють між собою


model_mult <- lm(adr ~ lead_time + stays_in_nights +
                   all_guests + I(all_guests^2) + booking_changes + total_of_special_requests + 
                   market_segment_b + distribution_channel_b + deposit_type_b + 
                   with_meal, data=data)
model_mult_hc1 <- coeftest(model_mult, vcov. = hccm(model_mult, type = "hc1"))

model_mult_sig <- lm(adr ~ all_guests +  total_of_special_requests + 
                       market_segment_b, data=data)
model_mult_sig_hc1 <- coeftest(model_mult_sig, vcov. = hccm(model_mult_sig, type = "hc1"))

stargazer(model, model_mult, model_mult_sig, type = "latex",
          title = "Multiple regression", label = "table:evals-reg-mult",
          dep.var.labels = c("Середнє adr"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model_mult_hc1[, 2], model_mult_sig_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE)

model_mult_sig <- lm(adr ~ all_guests + stays_in_nights + total_of_special_requests + 
                       market_segment_b + distribution_channel_b  +
                       deposit_type_b, data=data)
model_mult_sig_hc1 <- coeftest(model_mult_sig, vcov. = hccm(model_mult_sig, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))
ci1 <- coefci(model_mult, vcov. = hccm(model, type = "hc1"))
ci2 <- coefci(model_mult_sig, vcov. = hccm(model, type = "hc1"))


stargazer(model, model_mult, model_mult_sig, type = "latex",
          title = "Multiple regression", label = "table:evals-reg-mult",
          dep.var.labels = c("Середнє adr"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model_mult_hc1[, 2], model_mult_sig_hc1[, 2]),
          ci = TRUE, ci.custom = list(ci, ci1, ci2),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE)

linearHypothesis(model_mult_sig, c("stays_in_nights = 0"), vcov. = hccm(model_mult_sig, type = "hc1"))

model_mult_sig <- lm(adr ~ all_guests + total_of_special_requests + 
                       market_segment_b + distribution_channel_b  +
                       deposit_type_b, data=data)
model_mult_sig_hc1 <- coeftest(model_mult_sig, vcov. = hccm(model_mult_sig, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))
ci1 <- coefci(model_mult, vcov. = hccm(model, type = "hc1"))
ci2 <- coefci(model_mult_sig, vcov. = hccm(model, type = "hc1"))


stargazer(model, model_mult, model_mult_sig, type = "latex",
          title = "Multiple regression", label = "table:evals-reg-mult",
          dep.var.labels = c("Середнє adr"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model_mult_hc1[, 2], model_mult_sig_hc1[, 2]),
          ci = TRUE, ci.custom = list(ci, ci1, ci2),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE)
