library(plm)
library(tidyverse)

setwd("C:/Users/phili/OneDrive/UNI/3rd Year/ES30029/Determinants of bank profitability/Data")
test <- read.csv("test2.csv")

test_reg  <- pdata.frame(test, index = c("id", "Year"))

str(test)

test_changes <- test %>%
  group_by(id) %>%
  arrange(Year) %>%
  mutate(TA = TA-lag(TA),
         RoE = RoE-lag(RoE),
         Efficiency = Efficiency-lag(Efficiency),
         LR = LR-lag(LR),
         GDP = GDP-lag(GDP)) %>%
  filter(Year != 2010) %>%
  ungroup()

test_changes_reg  <- pdata.frame(test_changes, index = c("id", "Year"))

test_lag <- test %>%
  group_by(id) %>%
  arrange(Year) %>%
  mutate(TA = lag(TA),
         RoE = RoE,
         Efficiency = lag(Efficiency),
         LR = lag(LR),
         GDP = lag(GDP)) %>%
  filter(Year != 2010) %>%
  ungroup()

test_lag_reg  <- pdata.frame(test_lag, index = c("id", "Year"))


## regress

fe_test <- plm(RoE ~ TA + Efficiency + LR + GDP, data = test_reg, model = "within")

summary(fe_test)


ols_test <- lm(RoE ~ TA + Efficiency + LR + GDP, data = test_reg)

summary(ols_test)

#TA sig in both FE and OLS regression
test %>%
  ggplot(aes(x = TA, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

#efficiency only significant in oLS regression, as slope for each bank different. [changed]
test %>%
  ggplot(aes(x = Efficiency, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

test %>%
  ggplot(aes(x = LR, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

#test changes

fe_test_changes <- plm(RoE ~ TA + Efficiency + LR + GDP, data = test_changes_reg, model = "within")

summary(fe_test_changes)


ols_test_changes <- lm(RoE ~ TA + Efficiency + LR + GDP, data = test_changes_reg)

summary(ols_test_changes)


#TA sig in both FE and OLS regression
test_changes %>%
  ggplot(aes(x = TA, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

#efficiency significant in neither
test_changes %>%
  ggplot(aes(x = Efficiency, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")


#test lag


fe_test_lag <- plm(RoE ~ TA + Efficiency + LR + GDP, data = test_lag_reg, model = "within")

summary(fe_test_lag)


ols_test_lag <- lm(RoE ~ TA + Efficiency + LR + GDP, data = test_lag_reg)

summary(ols_test_lag)

#TA not sig as TA impacts RoE via random shocks, not structural impact. Size last year will have no impact on RoE
test_lag %>%
  ggplot(aes(x = TA, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

#efficiency is significant, but negative. For each bank clearly efficiency increases RoE.
test_lag %>%
  ggplot(aes(x = Efficiency, y = RoE, col = factor(id), group = factor(id))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")