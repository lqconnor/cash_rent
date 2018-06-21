# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()
ptm <- proc.time()


# Load Packages ----------------------------------------------------
pckgs <- c("tidyverse", "stargazer", "rnass", "rnassqs", "xlsx")
lapply(pckgs, library, character.only = TRUE)

# Load the api key -------------------------------------------------
api <- readLines(".secret")
Sys.setenv(NASSQS_TOKEN = api)

# NASS data: State Cash Rent ---------------------------------------
# List parameters of interest to feed to rnassqs package
params = list(source_desc = "SURVEY", 
              agg_level_desc = "STATE",
              short_desc = "RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE",
              year__GE = "1998")

# Feed parameters to rnassqs package
rent <- nassqs(params = params) %>%
  select(state_name, state_alpha, short_desc, Value, year)  # keep variables of interest

rent$year <- as.Date(rent$year, format = "%Y")  # format year variable as a date
rent$year <- as.numeric(format(rent$year,"%Y")) # keep just the year portion of the date

# Make data balanced
rent <- group_by(rent, state_alpha) %>%
  mutate(all_there = NROW(state_alpha)) %>%   # count number of rows by group
  filter(all_there == 20) %>%                 # keep only states with all 20 years
  select(-all_there) %>%
  ungroup()

ptm2 <- proc.time() - ptm

#check <- as.data.frame(nassqs_field_values(field = 'short_desc')) %>%
  #filter(str_detect(short_desc, "RENT"))