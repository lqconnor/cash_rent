# Define work space --------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)


memory.limit()

memory.limit(size=300000)

setwd("C:/Users/connor.189/Documents/Data")

c_rent <- read_csv("cash_rent.csv")

prime <- filter(c_rent, str_detect(State, paste(c("IOWA", "ILLINOIS"), collapse = '|'))) %>%
  select()