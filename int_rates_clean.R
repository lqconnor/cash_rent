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

rates <- read.xlsx("../Sticky_Cash_Rent/ag_loan_rates.xlsx", sheetName = "afdr_a5")

int_rate <- filter(rates, !str_detect(Period, "Q")) %>%
  select(-NA.)