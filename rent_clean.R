# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(stargazer)
library(strucchange)
library(Hmisc)
library(rnass)
library(rnassqs)
library(xlsx)

api <- "4B894724-F6EE-361B-9DF0-AA95D222FE59"

# Data Import ------------------------------
# Cash rent data
cnty_rnt <- read_csv("./data/cash_rent.csv") %>%
  filter(State == "LOUISIANA")

cnty_rnt$Year <- as.numeric(cnty_rnt$Year)

cnty_rnt <- filter(cnty_rnt, Year >= 2008)

# NASS data: Price etc.
params = list(source_desc = "SURVEY", 
              commodity_desc = "CORN", 
              state_name = "OHIO",
              short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
              reference_period_desc = "MARKETING YEAR",
              year__GE = "2008")

price <- nassqs(params = params, key = api)

price$year <- as.Date(price$year, format = "%Y")
price$year <- as.numeric(format(price$year,"%Y"))


# Kansas Fed Interest Rate data
rates <- read.xlsx("../Sticky_Cash_Rent/ag_loan_rates.xlsx", sheetName = "afdr_a5")

int_rate <- filter(rates, !str_detect(Period, "Q")) %>%
  select(-NA.)

int_rate$Period <- as.Date(int_rate$Period, format = "%Y")
int_rate$Period <- as.numeric(format(int_rate$Period,"%Y"))

int_rate <- filter(int_rate, Period >= 2008)

final <- left_join(cnty_rnt, price, by = c("Year" = "year")) %>%
  left_join(int_rate, by = c("Year" = "Period")) %>%
  filter(Year < 2017)
