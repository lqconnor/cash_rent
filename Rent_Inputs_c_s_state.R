# Define work space --------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(stargazer)


memory.limit()

memory.limit(size=300000)

setwd("C:/Users/connor.189/Documents/Data")
#setwd("C:/Users/Lawson/Documents/Data")


# Load all data files -------------------------------------------------------------------------
economics <- read_tsv("qs.economics_20180310.txt")

ARMS <- read_tsv("qs.crops_20180321.txt")

Income <- read_csv("C:/Box Sync/FIE Lawson/Data/farmincome_wealthstatisticsdata_february2018.csv")

rate <- read_csv("interest_rate.csv")

# Data Identifiers --------------------------------------------------------------------------------------
# State ID
state <- "US"

# Data Paramters
commodities <- c("CORN", "SOYBEANS")
prod_vars <- c("PRODUCTION", "YIELD", "HARVESTED", "PRICE")
cleaner <- c("SILAGE")

# Trim data ------------------------------------------------------------------------------------
econ <-  filter(economics, YEAR >= 1998 & YEAR <= 2017,
                str_detect(STATE_ALPHA, state))

crp_info <- filter(ARMS, str_detect(DOMAIN_DESC, "TOTAL"), 
               str_detect(STATE_ALPHA, state), str_detect(SOURCE_DESC, "SURVEY"), YEAR >= 1998 & YEAR <= 2017)

# Primary Variable gen ---------------------------------------------------------------------------------

# Expenses and Assets
expns <- filter(econ, str_detect(SHORT_DESC, "EXPENSE, MEASURED IN \\$"), !str_detect(SHORT_DESC, "OPERATION"),
                str_detect(SHORT_DESC, "EXPENSE TOTALS, PRODUCTION"), str_detect(DOMAIN_DESC, "TOTAL"))

rent <- filter(econ, str_detect(SHORT_DESC, "RENT, CASH, CROPLAND - EXPENSE"), str_detect(SHORT_DESC, "MEASURED IN"))

land <- filter(econ, str_detect(SHORT_DESC, "AG LAND, CROPLAND - ASSET VALUE"), str_detect(SHORT_DESC, "MEASURED IN"))

interest <- filter(econ, str_detect(SHORT_DESC, "INTEREST - EXPENSE, MEASURED IN \\$$"), 
                   str_detect(STATE_ALPHA, state), DOMAIN_DESC == "TOTAL", str_detect(SOURCE_DESC, "SURVEY"))

deflator <- select(Income, Year, ChainType_GDP_Deflator) %>%
  filter(Year <= 2016 & Year >= 1998) %>%
  distinct()

rate <- filter(rate, year <= 2016)

# Acres Planted
acres <- filter(crp_info, str_detect(SHORT_DESC, "HARVESTED"), str_detect(SHORT_DESC, "FIELD CROP TOTALS"))

acres_c <- filter(crp_info, str_detect(SHORT_DESC, "HARVESTED"), str_detect(SHORT_DESC, "^CORN, GRAIN"),
                  str_detect(REFERENCE_PERIOD_DESC, "YEAR$"))

acres_s <- filter(crp_info, str_detect(SHORT_DESC, "HARVESTED"), str_detect(SHORT_DESC, "^SOYBEANS - ACRES"),
                  str_detect(REFERENCE_PERIOD_DESC, "YEAR$"))

# Price
price_c <- filter(crp_info, str_detect(SHORT_DESC, "PRICE"), str_detect(SHORT_DESC, "CORN, GRAIN - PRICE RECEIVED, MEASURED IN \\$"),
                str_detect(REFERENCE_PERIOD_DESC, "MARKETING YEAR"))

price_s <- filter(crp_info, str_detect(SHORT_DESC, "PRICE"), str_detect(SHORT_DESC, "SOYBEANS - PRICE RECEIVED, MEASURED IN \\$"),
                  str_detect(REFERENCE_PERIOD_DESC, "MARKETING YEAR"))

#Production and Yield
prod_c <- filter(crp_info, str_detect(SHORT_DESC, "PRODUCTION"), str_detect(SHORT_DESC, "CORN, GRAIN"),
               str_detect(SHORT_DESC, "MEASURED IN \\$"))

prod_s <- filter(crp_info, str_detect(SHORT_DESC, "PRODUCTION"), str_detect(SHORT_DESC, "SOYBEANS"),
                 str_detect(SHORT_DESC, "MEASURED IN \\$"))

yield_c <- filter(crp_info, str_detect(SHORT_DESC, "YIELD"), str_detect(SHORT_DESC, "CORN, GRAIN"),
                str_detect(REFERENCE_PERIOD_DESC, "YEAR$"))

yield_s <- filter(crp_info, str_detect(SHORT_DESC, "YIELD"), str_detect(SHORT_DESC, "SOYBEANS"),
                  str_detect(REFERENCE_PERIOD_DESC, "YEAR$"))

# Farm Income
fi <- filter(Income, str_detect(VariableDescriptionTotal, "Net farm income$"), str_detect(State, state), 
             Year >= 1998 & Year <= 2017)


# Farm Income
ci <- filter(Income, str_detect(VariableDescriptionTotal, "Net cash income$"), str_detect(State, state), 
             Year >= 1998 & Year <= 2017)

# Convert VALUE column to Numeric
expns$expense <- as.numeric(gsub(",","", expns$VALUE, fixed = TRUE))
rent$rent <- as.numeric(gsub(",","", rent$VALUE, fixed = TRUE))
land$land <- as.numeric(gsub(",","", land$VALUE, fixed = TRUE))
acres$acres <- as.numeric(gsub(",","", acres$VALUE, fixed = TRUE))
acres_c$acres_c <- as.numeric(gsub(",","", acres_c$VALUE, fixed = TRUE))
acres_s$acres_s <- as.numeric(gsub(",","", acres_s$VALUE, fixed = TRUE))
fi$f_income <- as.numeric(gsub(",","", fi$Amount, fixed = TRUE))
ci$csh_income <- as.numeric(gsub(",","", ci$Amount, fixed = TRUE))
price_c$price_c <- as.numeric(gsub(",","", price_c$VALUE, fixed = TRUE))
price_s$price_s <- as.numeric(gsub(",","", price_s$VALUE, fixed = TRUE))
prod_c$prod_c <- as.numeric(gsub(",","", prod_c$VALUE, fixed = TRUE))
prod_s$prod_s <- as.numeric(gsub(",","", prod_s$VALUE, fixed = TRUE))
yield_c$yield_c <- as.numeric(gsub(",","", yield_c$VALUE, fixed = TRUE))
yield_s$yield_s <- as.numeric(gsub(",","", yield_s$VALUE, fixed = TRUE))
interest$interest <- as.numeric(gsub(",","", interest$VALUE, fixed = TRUE))

# Calculate the proportion of land that is rented. We use Census data from 2012 to get total acres rented.
# That is then divided by total acres planted. A check on 2007 revealed that the proportion is stable to 
# three decimal places.
acr <- select(acres, YEAR, acres)
rented <- filter(econ, str_detect(SHORT_DESC, "RENTED FROM OTHERS, IN FARMS - ACRES"),
                 str_detect(DOMAIN_DESC, "TOTAL")) %>%
  left_join(acr, by = c('YEAR' = 'YEAR')) %>%
  mutate(p_rented = VALUE/acres) %>%
  filter(YEAR == 2012) %>%
  select(p_rented)


# Graphing Variables gen ---------------------------------------------------------------------------
rent <- select(rent, YEAR, rent)
land <- select(land, YEAR, land)
fi <- select(fi, f_income, Year)
ci <- select(ci, csh_income, Year)
acres <- select(acres, YEAR, acres)
price_c <- select(price_c, YEAR, price_c)
price_s <- select(price_s, YEAR, price_s)
prod_c <- select(prod_c, YEAR, prod_c)
prod_s <- select(prod_s, YEAR, prod_s)
acres_c <- select(acres_c, YEAR, acres_c)
acres_s <- select(acres_s, YEAR, acres_s)
yield_c <- select(yield_c, YEAR, yield_c)
yield_s <- select(yield_s, YEAR, yield_s)
interest <- select(interest, YEAR, interest)

expenses <- left_join(expns, acres, by = c("YEAR" = "YEAR")) %>%
  mutate(exp_acr = expense/acres) %>%    #Convert expenses to per acre values
  left_join(rent, by = c("YEAR" = "YEAR")) %>%
  left_join(land, by = c("YEAR" = "YEAR")) %>%
  left_join(fi, by = c("YEAR" = "Year")) %>%
  left_join(ci, by = c("YEAR" = "Year")) %>%
  left_join(price_c, by = c("YEAR" = "YEAR")) %>%
  left_join(price_s, by = c("YEAR" = "YEAR")) %>%
  left_join(prod_c, by = c("YEAR" = "YEAR")) %>%
  left_join(prod_s, by = c("YEAR" = "YEAR")) %>%
  left_join(yield_c, by = c("YEAR" = "YEAR")) %>%
  left_join(yield_s, by = c("YEAR" = "YEAR")) %>%
  left_join(interest, by = c("YEAR" = "YEAR")) %>%
  left_join(acres_c, by = c("YEAR" = "YEAR")) %>%
  left_join(acres_s, by = c("YEAR" = "YEAR")) %>%
  left_join(rate, by = c("YEAR" = "year")) %>%
  mutate(rent_exp = rent/exp_acr) %>%
  mutate(exp_income = exp_acr/f_income) %>%
  mutate(fi_a = f_income*1000/(acres*rented$p_rented)) %>%      # This should calculate the per rented acre value of farm income. 
  mutate(r_i = rent/fi_a) %>%
  mutate(l_rtn = f_income/acres)

acres_tot <- left_join(acres_c, acres_s, by = c("YEAR" = "YEAR")) %>%
  mutate(tot_acr = acres_c + acres_s)

# Generate percentage change of key variables
expenses %<>%
  arrange(YEAR) %>%
  mutate(pct_lnd = land/lag(land) - 1) %>%
  mutate(pct_rnt = rent/lag(rent) - 1) %>%
  mutate(pct_finc = f_income/lag(f_income) - 1) %>%
  mutate(pct_prc_c = price_c/lag(price_c) - 1) %>%
  mutate(pct_prc_s = price_s/lag(price_s) - 1) %>%
  mutate(pct_int = interest/lag(interest) - 1) %>%
  mutate(pct_acc = acres_c/lag(acres_c) - 1) %>%
  mutate(pct_acs = acres_s/lag(acres_s) - 1) %>%
  mutate(pct_yldc = yield_c/lag(yield_c) - 1) %>%
  mutate(pct_ylds = yield_s/lag(yield_s) - 1) %>%
  mutate(pct_prdc = prod_c/lag(prod_c) - 1) %>%
  mutate(pct_prds = prod_s/lag(prod_s) - 1)

# Graphs -------------------------------------------------------------------------------------------
ggplot(data = expenses, aes(x=YEAR, y=r_i)) +
  geom_line() +
  labs(y = "Rent per Dollar of Income", x = "Year",
       title = "Cash Rent as a Proportion of Farm Income")

ggplot(data = expenses, aes(x=YEAR, y=rent_exp)) +
  geom_line()

ggplot(data = acres, aes(x=YEAR, y=acres)) +
  geom_line()

ggplot(data = acres_c, aes(x=YEAR, y=acres_c)) +
  geom_line()

ggplot(data = acres_s, aes(x=YEAR, y=acres_s)) +
  geom_line()

ggplot(data = acres_tot, aes(x=YEAR, y=tot_acr)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=l_rtn)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=rent)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=land)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=exp_income)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=price_c)) +
  geom_line()

ggplot() +
  geom_line(data = expenses, aes(x=YEAR, y=f_income/(100000*deflator$ChainType_GDP_Deflator)), color = "black") +
  geom_line(data = expenses, aes(x=YEAR, y=price_c), color = "orange") +
  geom_line(data = expenses, aes(x=YEAR, y=rent/20), color = "red") +
  geom_line(data = expenses, aes(x=YEAR, y=land/500), color = "blue") +
  geom_line(data = expenses, aes(x=YEAR, y=price_s*0.5), color = "green") +
  geom_line(data = expenses, aes(x=YEAR, y=interest/1000000000), color = "purple") +
  geom_line(data = rate, aes(x=year, y=rate*100/deflator$ChainType_GDP_Deflator), color = "pink")

ggplot() +
  geom_line(data = expenses, aes(x=YEAR, y=pct_finc), color = "black") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_prc_c), color = "orange") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_rnt), color = "red") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_lnd), color = "blue") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_prc_s), color = "green") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_int), color = "purple") +
  geom_hline(yintercept = 0)

ggplot() +
  geom_line(data = expenses, aes(x=YEAR, y=prod_c/1000000000), color = "black") +
  geom_line(data = expenses, aes(x=YEAR, y=yield_c/2.5), color = "blue") +
  geom_line(data = expenses, aes(x=YEAR, y=acres_c/1500000), color = "purple") +
  geom_line(data = expenses, aes(x=YEAR, y=prod_s/1000000000), color = "red") +
  geom_line(data = expenses, aes(x=YEAR, y=yield_s), color = "orange") +
  geom_line(data = expenses, aes(x=YEAR, y=acres_s/1500000), color = "green")

ggplot() +
  geom_line(data = expenses, aes(x=YEAR, y=pct_acc), color = "black") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_prdc), color = "orange") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_yldc), color = "red") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_acs), color = "blue") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_prds), color = "green") +
  geom_line(data = expenses, aes(x=YEAR, y=pct_ylds), color = "purple") +
  geom_hline(yintercept = 0)

rd <- diff(expenses$rent/deflator$ChainType_GDP_Deflator, differences = 1)
acf(rd)

lnd <- diff(expenses$land/(deflator$ChainType_GDP_Deflator), differences = 1)
acf(lnd)

ind <- diff(expenses$f_income/(100000*deflator$ChainType_GDP_Deflator), differences = 1)

csh <- diff(expenses$csh_income/(100000*deflator$ChainType_GDP_Deflator), differences = 1)
acf(ind)
ccf(rd, ind)
ccf(rd, csh)
ccf(expenses$f_income/deflator$ChainType_GDP_Deflator, expenses$rent/deflator$ChainType_GDP_Deflator)

results <- lm(log(rent) ~ lead(log(land)) + lead(log(csh_income)) + lead(log(f_income)) + log(land) + log(csh_income) + log(f_income) + lag(log(land)) + lag(log(csh_income)) + lag(log(f_income)), data = expenses)
summary(results)

expenses <- mutate(expenses, dlnd = log(land) - lag(log(land)),
                   dcsh = log(csh_income) - lag(log(csh_income)),
                   dfi = log(f_income) - lag(log(f_income)),
                   drnt = log(rent) - lag(log(rent)),
                   drt = log(rate) - lag(log(rate)))

dlnd <- na.omit(expenses$dlnd)
dlnd <- diff(dlnd, differences = 1)
acf(dlnd)

expenses <- mutate(expenses, up_lnd = as.numeric(dlnd > 0),
                   up_csh = as.numeric(dcsh > 0),
                   up_fi = as.numeric(dfi > 0),
                   csh_d = dcsh*(1-up_csh),
                   csh_up = dcsh*up_csh,
                   lnd_d = dlnd*(1-up_lnd),
                   lnd_up = dlnd*up_lnd)

fit <- lm(drnt ~ dfi + dlnd + dcsh + lag(dfi) + lag(dlnd) + lag(dcsh) + lag(dfi, 2) + lag(dlnd, 2) + lag(dcsh, 2), data = expenses)
summary(fit)

stargazer(fit, title = "Determinants of Movements in Cash Rents",
          covariate.labels = c("Farm Income", "Land Values", 
                             "Cash Income", "Lag Farm Income", 
                             "Lag Land Values", "Lag Cash Income",
                             "2 Year Lag Farm Income", "2 Year Lag Land Values",
                             "2 Year Lag Cash Income"), type = 'latex')

fit <- lm(drnt ~ up_csh*dcsh + up_csh*lag(dcsh), data = expenses)
summary(fit)

fit <- lm(drnt ~ csh_d + csh_up + lead(csh_d) + lead(csh_up) + lag(csh_d) + lag(csh_up) + lnd_d + lnd_up + lead(lnd_d) + lead(lnd_up) + lag(lnd_d) + lag(lnd_up), data = expenses)
summary(fit)

fit <- lm(drnt ~ lnd_d + lnd_up + lead(lnd_d) + lead(lnd_up) + lag(lnd_d) + lag(lnd_up), data = expenses)
summary(fit)