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
api <- "4B894724-F6EE-361B-9DF0-AA95D222FE59"

# Data Import ------------------------------
cnty_rnt <- read_csv("./data/cash_rent.csv")

#price <- nass_data(source_desc = "SURVEY", commodity_desc = "CORN", state_name = "OHIO", year = "2016", token = api)


params = list(source_desc = "SURVEY", 
              commodity_desc = "CORN", 
              state_name = "OHIO",
              short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
              reference_period_desc = "MARKETING YEAR",
              year__GE = "2008")

price <- nassqs(params = params, key = api)
