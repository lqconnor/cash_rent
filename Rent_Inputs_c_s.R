# Define work space --------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)
library(ggplot2)


memory.limit()

memory.limit(size=300000)

setwd("C:/Users/connor.189/Documents/Data")
#setwd("C:/Users/Lawson/Documents/Data")


# Load all data files -------------------------------------------------------------------------
economics <- read_tsv("qs.economics_20180310.txt")
ARMS <- read_tsv("qs.crops_20180321.txt")
Income <- read_csv("C:/Box Sync/FIE Lawson/Data/farmincome_wealthstatisticsdata_february2018.csv")

# Trim data ------------------------------------------------------------------------------------
econ <-  filter(economics, str_detect(AGG_LEVEL_DESC, "^NATIONAL"), YEAR >= 2003 & YEAR < 2017)

crop <- filter(ARMS, str_detect(AGG_LEVEL_DESC, "^NATIONAL"), str_detect(DOMAIN_DESC, "TOTAL"), 
               str_detect(SOURCE_DESC, "SURVEY"), YEAR >= 2003 & YEAR < 2017)

# Primary Variable gen ---------------------------------------------------------------------------------
# Expenses 
expns <- filter(econ, str_detect(SHORT_DESC, "EXPENSE, MEASURED IN \\$"), !str_detect(SHORT_DESC, "OPERATION"),
                str_detect(SHORT_DESC, "EXPENSE TOTALS, PRODUCTION"), str_detect(AGG_LEVEL_DESC, "^NATIONAL"), 
                str_detect(DOMAINCAT_DESC, "CROP"), YEAR >= 1998)

rent <- filter(econ, str_detect(SHORT_DESC, "RENT, CASH, CROPLAND - EXPENSE"), str_detect(SHORT_DESC, "MEASURED IN"), 
               str_detect(AGG_LEVEL_DESC, "^NATIONAL"), YEAR <= 2017)

# Acres Planted
acres <- filter(crop, str_detect(SHORT_DESC, "HARVESTED"), str_detect(AGG_LEVEL_DESC, "^NATIONAL"), 
                str_detect(SHORT_DESC, "FIELD CROP TOTALS"), YEAR >= 1998 & YEAR < 2017)

acres_c <- filter(crop, str_detect(SHORT_DESC, "HARVESTED"), str_detect(AGG_LEVEL_DESC, "^NATIONAL"), 
                str_detect(SHORT_DESC, "^CORN, GRAIN"), str_detect(DOMAIN_DESC, "TOTAL"), 
                str_detect(REFERENCE_PERIOD_DESC, "YEAR$"), str_detect(SOURCE_DESC, "SURVEY"), YEAR >= 1998 & YEAR <= 2017)

acres_s <- filter(crop, str_detect(SHORT_DESC, "HARVESTED"), str_detect(AGG_LEVEL_DESC, "^NATIONAL"), 
                str_detect(SHORT_DESC, "^SOYBEANS - ACRES"), str_detect(DOMAIN_DESC, "TOTAL"), 
                str_detect(REFERENCE_PERIOD_DESC, "YEAR$"), str_detect(SOURCE_DESC, "SURVEY"), YEAR >= 1998 & YEAR <= 2017)

# Farm Income
fi <- filter(Income, str_detect(VariableDescriptionTotal, "Net farm income$"), str_detect(State, "US"), 
             Year >= 1998 & Year <= 2017)

# Convert VALUE column to Numeric
expns$expense <- as.numeric(gsub(",","", expns$VALUE, fixed = TRUE))
rent$rent <- as.numeric(gsub(",","", rent$VALUE, fixed = TRUE))
acres$acres <- as.numeric(gsub(",","", acres$VALUE, fixed = TRUE))
acres_c$acres <- as.numeric(gsub(",","", acres_c$VALUE, fixed = TRUE))
acres_s$acres <- as.numeric(gsub(",","", acres_s$VALUE, fixed = TRUE))
fi$f_income <- as.numeric(gsub(",","", fi$Amount, fixed = TRUE))

# Graphing Variables gen ---------------------------------------------------------------------------
rent <- select(rent, YEAR, rent)
fi <- select(fi, f_income, Year)
expenses <- mutate(expns, exp_acr = expns$expense/acres$acres) %>%    #Covnert expenses to per acre values
  left_join(rent, by = c("YEAR" = "YEAR")) %>%
  left_join(fi, by = c("YEAR" = "Year")) %>%
  mutate(rent_exp = rent/exp_acr) %>%
  mutate(fi_a = f_income*1000/acres$acres) %>%
  mutate(r_i = rent/fi_a)

# Graphs -------------------------------------------------------------------------------------------
ggplot(data = expenses, aes(x=YEAR, y=r_i)) +
  geom_line()

ggplot(data = expenses, aes(x=YEAR, y=rent_exp)) +
  geom_line()

ggplot(data = acres, aes(x=YEAR, y=acres)) +
  geom_line()

ggplot(data = acres_c, aes(x=YEAR, y=acres)) +
  geom_line()

ggplot(data = acres_s, aes(x=YEAR, y=acres)) +
  geom_line()
  
  