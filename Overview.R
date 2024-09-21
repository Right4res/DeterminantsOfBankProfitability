# packages
library(scales)
library(tidyverse)
library(Hmisc)
library(corrplot)



# import data
setwd("C:/Users/phili/OneDrive/UNI/3rd Year/ES30029/Determinants of bank profitability/Data")
data_raw <- read.csv("data4.csv", stringsAsFactors=FALSE)
macro <- read.csv("macro.csv", stringsAsFactors=FALSE)
random <- read.csv("random.csv")

data_raw <- data_raw %>%
  left_join(macro, by = "Year")

# solve formatting
colnames(data_raw)[1] <- "Bank"

# replaced NAs manually
data_raw[data_raw == "NULL"] <- NA
data_raw[data_raw == "#N/A"] <- NA
data_raw[data_raw == "Unable to resolve and collect data for all requested identifiers and fields."] <- NA
#data_raw[data_raw == ""] <- NA
data_raw[7:31][data_raw[7:31] == "Retrieving..."] <- NA

#write and read data to get numerics and factors
write.csv(data_raw, "Clean_data.csv", row.names = FALSE)
data_raw <- read.csv("Clean_data.csv")

data_raw$Date <- as.Date(data_raw$Date, format = "%d/%m/%Y")
#data_raw[7:24] <- sapply(data_raw[7:24],as.numeric)
#data_raw[26:31] <- sapply(data_raw[26:31],as.numeric)

str(data_raw)

## data availability

data <- data_raw %>%
  mutate(LoanLossProv_pct = LoanLossProv/Loans,
         NonPerfLoans_pct = NonPerfLoans/Loans,
         LoanstoTA = Loans/TA,
         NII_pct = NII/(NII+NonIntInc),
         LeverageRatio = Equity/TA,
         T1LeverageRatio = T1/TA,
         LongTermDebt_pct = LongTermDebt/Liabilities)

# want to remove banks with no loans etc. 

data <- data %>% filter(TA > 0) %>% filter(!is.na(Type)) %>% filter(!is.na(Loans)) %>% 
  filter(!is.na(RoE)) %>% filter(!is.na(RoA))

summary_type <- data %>%
  group_by(Type) %>%
  summarise(count = n(),
            mean_TA = mean(TA, na.rm = TRUE),
            Q.1st = quantile(TA, 0.25),
            Q.2nd = quantile(TA, 0.5),
            Q.3rd = quantile(TA, 0.75))


data <- data %>% filter(Type != "")

data_2018 <- data %>% filter(Year == "2018")

summary(data_2018)
summary(data)

summary <- data %>% group_by(Bank) %>%
  summarise(count = n()) 

# want to remove all of the samples for a company with missing data, not only half or something. 

to_remove <- summary %>%
  filter(count < 9) %>%
  mutate(to_remove = 1)

data <- data %>% left_join(to_remove) %>%
  filter(is.na(to_remove)) %>% select(-"to_remove", - "count")



sapply(data, function(x) sum(is.na (x))) / 9


## select key columns to use

data_select <- data %>%
  select(1:4, 25, 7, 32:33, 31, 38, 30, 19, 40:41, 22, 36:37, 42, 34:35) %>%
  mutate(not_ComBank = if_else(Type == "Commercial Banking", 0, 1))


data_select_2018 <- data_select %>%
  filter(Year == 2018)

summary(data_select)

data_short <- data_select %>%
  select(1:2, 6:21)



data_short$Bank <- droplevels(data_short$Bank, exclude = if(anyNA(levels(data_short$Bank))) NULL else NA)

data_short <- data_short %>%
  mutate(Bank = fct_reorder(Bank, TA),
         id = fct_anon(Bank),
         id = as.character(id),
         id = as.numeric(id))

data_short <- data_short %>%
  select(19, 1:18)


str(data_short)

data_short %>% 
  ggplot(aes(x = Bank, y = TA)) +
  geom_point()

#write.csv(data_short, "data_short2.csv", row.names = FALSE)

## examine data

summary(data_select_2018)

data_2018 <- data %>% filter(Year == "2018")




data_2018 %>%
  ggplot(aes(x = TA)) +
  geom_histogram() +
  scale_x_log10()

data %>%
  ggplot(aes(x = TA)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ Year)

data %>%
  ggplot(aes(x = NIBT)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ Year)

# profitability measures
# need cleaning

data %>%
  ggplot(aes(x = RoE)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.RoE = mean(RoE)), aes(xintercept = mean.RoE), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.RoE = quantile(RoE, 0.25)), aes(xintercept = st.RoE), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.RoE = quantile(RoE, 0.75)), aes(xintercept = rd.RoE), linetype = "dashed", col = "orange")


data %>%
  ggplot(aes(x = RoA)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.RoA = mean(RoA)), aes(xintercept = mean.RoA), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.RoA = quantile(RoA, 0.25)), aes(xintercept = st.RoA), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.RoA = quantile(RoA, 0.75)), aes(xintercept = rd.RoA), linetype = "dashed", col = "orange")



# Key explanatory varibales

#cost to income

data %>%
  filter(Efficiency > 0 & Efficiency < 200) %>%
  ggplot(aes(x = Efficiency)) +
  geom_histogram(bins = 75) +
  xlim(0,180) +
  facet_wrap(~ Year) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.Efficiency = mean(Efficiency)), aes(xintercept = mean.Efficiency), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.Efficiency = quantile(Efficiency, 0.25)), aes(xintercept = st.Efficiency), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.Efficiency = quantile(Efficiency, 0.75)), aes(xintercept = rd.Efficiency), linetype = "dashed", col = "orange")


#size
data %>%
  ggplot(aes(x = TA)) +
  geom_histogram(bins = 75) +
  scale_x_log10() +
  facet_wrap(~ Year) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.TA = mean(TA, na.rm = TRUE)), aes(xintercept = mean.TA), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.TA = quantile(TA, 0.25, na.rm = TRUE)), aes(xintercept = st.TA), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.TA = quantile(TA, 0.75, na.rm = TRUE)), aes(xintercept = rd.TA), linetype = "dashed", col = "orange")


#loan loss provision
data %>%
  ggplot(aes(x = LoanLossProv_pct)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.LoanLossProv_pct = mean(LoanLossProv_pct, na.rm = TRUE)), aes(xintercept = mean.LoanLossProv_pct), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.LoanLossProv_pct = quantile(LoanLossProv_pct, 0.25, na.rm = TRUE)), aes(xintercept = st.LoanLossProv_pct), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.LoanLossProv_pct = quantile(LoanLossProv_pct, 0.75, na.rm = TRUE)), aes(xintercept = rd.LoanLossProv_pct), linetype = "dashed", col = "orange")


# capital adequacy

#RWCR
data %>%
  ggplot(aes(x = T1_pct)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.T1_pct = mean(T1_pct, na.rm = TRUE)), aes(xintercept = mean.T1_pct), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.T1_pct = quantile(T1_pct, 0.25, na.rm = TRUE)), aes(xintercept = st.T1_pct), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.T1_pct = quantile(T1_pct, 0.75, na.rm = TRUE)), aes(xintercept = rd.T1_pct), linetype = "dashed", col = "orange")

#leverage
data %>%
  ggplot(aes(x = LeverageRatio)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.LeverageRatio = mean(LeverageRatio, na.rm = TRUE)), aes(xintercept = mean.LeverageRatio), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.LeverageRatio = quantile(LeverageRatio, 0.25, na.rm = TRUE)), aes(xintercept = st.LeverageRatio), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.LeverageRatio = quantile(LeverageRatio, 0.75, na.rm = TRUE)), aes(xintercept = rd.LeverageRatio), linetype = "dashed", col = "orange")


# Diversification

data %>%
  ggplot(aes(x = NonIntInc_pct)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.NonIntInc_pct = mean(NonIntInc_pct, na.rm = TRUE)), aes(xintercept = mean.NonIntInc_pct), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.NonIntInc_pct = quantile(NonIntInc_pct, 0.25, na.rm = TRUE)), aes(xintercept = st.NonIntInc_pct), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.NonIntInc_pct = quantile(NonIntInc_pct, 0.75, na.rm = TRUE)), aes(xintercept = rd.NonIntInc_pct), linetype = "dashed", col = "orange")


# Liquidity proxy

data %>%
  ggplot(aes(x = LtD)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.LtD = mean(LtD, na.rm = TRUE)), aes(xintercept = mean.LtD), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.LtD = quantile(LtD, 0.25, na.rm = TRUE)), aes(xintercept = st.LtD), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.LtD = quantile(LtD, 0.75, na.rm = TRUE)), aes(xintercept = rd.LtD), linetype = "dashed", col = "orange")


# Long-term debt

data %>%
  ggplot(aes(x = LongTermDebt_pct)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Year) +
  scale_x_continuous(labels = percent_format()) +
  geom_vline(data = data %>% group_by(Year) %>% summarise(mean.LongTermDebt_pct = mean(LongTermDebt_pct, na.rm = TRUE)), aes(xintercept = mean.LongTermDebt_pct), linetype = "dashed", col = "red") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(st.LongTermDebt_pct = quantile(LongTermDebt_pct, 0.25, na.rm = TRUE)), aes(xintercept = st.LongTermDebt_pct), linetype = "dashed", col = "orange") +
  geom_vline(data = data %>% group_by(Year) %>% summarise(rd.LongTermDebt_pct = quantile(LongTermDebt_pct, 0.75, na.rm = TRUE)), aes(xintercept = rd.LongTermDebt_pct), linetype = "dashed", col = "orange")


#### ROE vs variables

data_select %>%
  ggplot(aes(y = RoE, x = TA)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = RoA)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(limits = c(0,3)) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = LtD)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(limits = c(0,2), labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = LoanstoTA)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = NonIntInc_pct)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = Efficiency)) +
  geom_point() +
  #scale_y_continuous(limits = c(0,20)) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = LeverageRatio)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = T1LeverageRatio)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = T1_pct)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = LoanLossProv_pct)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = NonPerfLoans_pct)) +
  geom_point() +
  #scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = RoE, x = LongTermDebt_pct)) +
  geom_point() +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(labels = percent_format()) +
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Year)

data_select %>%
  ggplot(aes(y = LoanLossProv_pct, x = NonPerfLoans_pct)) +
  geom_point()
  
#macro variables examination

data_by_year <- data_select %>%
  group_by(Year) %>%
  summarise(RoE = mean(RoE),
            RoA = mean(RoA),
            Inflation = mean(Inflation),
            GDP_pct = mean(GDP_pct),
            T1_pct = mean(T1_pct, na.rm = TRUE),
            NonPerfLoans_pct = mean(NonPerfLoans_pct, na.rm = TRUE),
            Efficiency = mean(Efficiency))

data_by_year %>%
  gather(key = "key", value = "Value", -Year) %>%
  ggplot(aes(x = Year, y = Value, col = key)) +
  geom_line() +
  scale_y_log10()

##corr plot on 2018 data


cor_data_2018 <- data_2018 %>% select(7:9, 17, 19, 22, 27:33, 36:41)

cor <- cor(cor_data_2018, use = "complete.obs")
rcor <- rcorr(as.matrix(cor_data_2018))

rcor_DF <- as.data.frame(rcor$P)


corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,
         p.mat = rcor$P, sig.level = 0.05, insig = "blank")


##corr plot on select 2018 data
#~~~ Note: looking at all years changes the picture

cor_data_select_2018 <- data_select_2018 %>% select(6:18)

cor_select <- cor(cor_data_select_2018, use = "complete.obs")
rcor_select <- rcorr(as.matrix(cor_data_select_2018))

rcor_select_DF <- as.data.frame(rcor_select$P)


corrplot(cor_select, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(cor_select, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,
         p.mat = rcor_select$P, sig.level = 0.05, insig = "blank")



### Fixed or Random effects chart

# looking at 14 "random" samples

#LtD
data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LtD, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LtD, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

#TA -> very different for individual vs agreggate 
data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = TA, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = TA, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

#Efficiency -> similar for individual and aggregate 
data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = Efficiency, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = Efficiency, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

#T1_pct -> mixed impact some increasing, some falling
data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 4) %>%
  ggplot(aes(x = T1_pct, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random < 10) %>%
  ggplot(aes(x = T1_pct, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30)) +
  facet_wrap(~ random) +
  theme(legend.position = "none")

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = T1_pct, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))


## Loan loss provisions

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LoanLossProv_pct, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

# slope is changing with each year. Need random effect?

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LoanLossProv_pct, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

## % non-interest income

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 3) %>%
  ggplot(aes(x = NonIntInc_pct, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

# Each year the level is different, but relationship the same

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = NonIntInc_pct, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

## % long-term debt

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LongTermDebt_pct, y = RoE, group = Bank, col = Bank)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

# 

data_short %>%
  left_join(random, by = c("id")) %>%
  filter(random == 1) %>%
  ggplot(aes(x = LongTermDebt_pct, y = RoE, group = factor(Year), col = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", linetype = "dashed", se = FALSE, colour = "black", size = 2) +
  scale_y_continuous(limits = c(0, 30))

 #check
check <- data %>% group_by(Bank) %>% summarise(Year = sum(Year))


