# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()
ptm <- proc.time()


# Set up R environment --------------------------------------------------------------------
#Load Packages

# Don't load the PLM package here. It has conflicting lead and lag commands that will
# cause errors in your file

pckgs <- c("tidyverse", "stargazer", "rnassqs")
lapply(pckgs, library, character.only = TRUE)

# Load api key into R environment
Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

# Store state and county cash rent URL keys ----------------------------------------------
state_rent <- "58B27A06-F574-315B-A854-9BF568F17652#7878272B-A9F3-3BC2-960D-5F03B7DF4826"
county_rent <- "E0F5EB36-3313-3D7B-9E7F-E56A3365CF2B#9A9F55D7-E267-38C6-ACB9-DF106291B5A7"

# Get and clean NASS API data -------------------------------------------------------------
# List parameters of interest to feed to rnassqs package
params = list(source_desc = "SURVEY", 
              agg_level_desc = "STATE",
              short_desc = "RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE",
              year__GE = "1998")

# County Cash Rent
# Feed parameters to rnassqs package
rent <- nassqs(params = params) %>%
  select(state_name, state_alpha, agg_level_desc, short_desc, Value, year)  # keep variables of interest

rent$year <- as.Date(rent$year, format = "%Y")  # format year variable as a date
rent$year <- as.numeric(format(rent$year,"%Y")) # keep just the year portion of the date

# Make data balanced
rent <- group_by(rent, state_alpha) %>%
  mutate(all_there = NROW(state_alpha)) %>%   # count number of rows by group
  filter(all_there == 20) %>%                 # keep only states with all 20 years
  select(-all_there) %>%
  ungroup()

# Load NASS URL data: ---------------------------------------------------------------------
# State rent
rent_st <- read_csv(str_c('http://quickstats.nass.usda.gov/data/spreadsheet/',state_rent,'.csv'))

# County rent
rent_ct <- read_csv(str_c('http://quickstats.nass.usda.gov/data/spreadsheet/',county_rent,'.csv'))

# Clean State level URL data ---------------------------------------------------------------------------
rent_st <- select(rent_st, Year, `Geo Level`, State, `State ANSI`, `Data Item`, Value) %>%
  filter(`Geo Level` != "NATIONAL")
  
# Make data balanced
rent_st <- group_by(rent_st, State) %>%
  mutate(all_there = as.numeric(Year == 1998)) %>%   # count number of rows by group
  mutate(there = sum(all_there)) %>%
  filter(there > 0, Year >= 1998) %>%                 # keep only states with all 20 years
  mutate(ms = Year - lead(Year)) %>%
  mutate(ms2 = sum(ms, na.rm = TRUE)) %>%
  filter(ms2 > 1) %>%
  select(-all_there, -there, -ms, -ms2) %>%
  ungroup()

# Split rent variables into columns
rent_st$`Data Item` <- gsub("[[:blank:]]-.+", "", rent_st$`Data Item`)
rent_st$`Data Item` <- gsub("RENT, CASH, ", "", rent_st$`Data Item`)
rent_st$`Data Item` <- gsub("CROPLAND$", "CROPLAND, COMBINED", rent_st$`Data Item`)
rent_st <-rename(rent_st, Rent = Value)
rent_st$Rent <- as.numeric(rent_st$Rent)

rent_st <- spread(rent_st, key = `Data Item`, value = Rent) %>%
  mutate(irr_prp = `CROPLAND, IRRIGATED`/`CROPLAND, COMBINED`,
         nirr_prp = `CROPLAND, NON-IRRIGATED`/`CROPLAND, COMBINED`) %>%
  arrange(State, -Year)

# Clean County level URL data ---------------------------------------------------------------------------
rent_ct <- select(rent_ct, Year, `Geo Level`, State, `State ANSI`, County, `County ANSI`, `Ag District`, `Data Item`, Value) %>%
  filter(`Geo Level` != "NATIONAL")

# Split rent variables into columns
rent_ct$`Data Item` <- gsub("[[:blank:]]-.+", "", rent_ct$`Data Item`)
rent_ct$`Data Item` <- gsub("RENT, CASH, ", "", rent_ct$`Data Item`)
rent_ct <-rename(rent_ct, Rent = Value)
rent_ct$Rent <- as.numeric(rent_ct$Rent)

rent_ct <- spread(rent_ct, key = `Data Item`, value = Rent) %>%
  arrange(State, -Year)

# Plot State level proportion of non-irriadted of total -------------------------------------------------
plot_data = filter(rent_st, State == "OHIO", Year > 2008)
ggplot(data = plot_data, aes(x=Year, y=nirr_prp)) +
  geom_line()

ptm <- proc.time() - ptm

#check <- as.data.frame(nassqs_field_values(field = 'short_desc')) %>%
  #filter(str_detect(short_desc, "RENT"))