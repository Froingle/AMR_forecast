# NOTES -------------------------------------------------------------------

#1. can add 'budget required' for WHO data countries


# ADMIN -------------------------------------------------------------------

library(tidyverse)
library(wbstats)
library(fable)
library(tsibble)
library(cowplot)

rm(list = ls())

source("fc_tb.R")


# GET DATA ----------------------------------------------------------------

# Incidence data (for forecasting) from IHME GBD
file_location <- "data/GBD_MDR & XDR-TB_1990-2017.CSV"
ihme_data <- read_csv(file_location) %>% 
  select(location, year, cause, val) %>%  
  arrange(location, year) %>% 
  pivot_wider(names_from = cause, values_from = val) %>% 
  rename("country" = location,
         "index" = year,
         "mdr_tb" = `Multidrug-resistant tuberculosis without extensive drug resistance`,
         "xdr_tb" = `Extensively drug-resistant tuberculosis`)

# Country expenditure data from WHO
who_bud <- read_csv("https://extranet.who.int/tme/generateCSV.asp?ds=budget")
who_exp <- read_csv("https://extranet.who.int/tme/generateCSV.asp?ds=expenditure_utilisation")

who_data <- who_exp %>% 
  transmute(country,
         iso3,
         index = year,
         bud_dr_tot = who_bud$budget_sld + who_bud$budget_mdrmgt,   # add sum of 'required budget' for drugs and programme costs to treat drug-resistant TB (US Dollars)
         exp_dr_tot = exp_mdrmgt + exp_sld     # add spend on programme costs and drugs to treat drug-resistant TB (US Dollars)
  )

ihme_2017 <- ihme_data %>% 
  filter(index == 2017 & country %in% who_data_2017$country)

who_data_2017 <- who_data %>%
  filter(index == 2017,
         exp_dr_tot > 0,
         country %in% ihme_data$country) %>% 
  transmute(country, iso3, index,
            cpc_mdr = exp_dr_tot / (ihme_2017$mdr_tb + ihme_2017$xdr_tb),
            cpc_xdr = exp_dr_tot / (ihme_2017$mdr_tb + ihme_2017$xdr_tb)
  )
  


eu_data_2012 <- tribble(
#EU data from Table 3: https://erj.ersjournals.com/content/erj/43/2/554.full.pdf
#EUR2012 to EUR2017 using inflation calc: https://www.inflationtool.com/euro?amount=1&year1=2012&year2=2017
#EUR2017 to USD using annual fx rate: https://www.statista.com/statistics/412794/euro-to-u-s-dollar-annual-average-exchange-rate/

  ~country,          ~iso3,    ~index,  ~cpc_mdr,             ~cpc_xdr,   #2012 values updated by inflation, then converted to USD using 2017 fx rate
#./................../........./......./....................../......................
  "Austria",         "AUT",    2017,   (57231 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Belgium",         "BEL",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Cyprus",          "CYP",    2017,   (24166 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Czech Republic",  "CZE",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Denmark",         "DNK",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Finland",         "FIN",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "France",          "FRA",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Germany",         "DEU",    2017,   (57487 * 1.04) * 1.13, (190888 * 1.04) * 1.13,
  "Greece",          "GRC",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Hungary",         "HUN",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Ireland",         "IRL",    2017,   (57231 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Italy",           "ITA",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Latvia",          "LVA",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Lithuania",       "LTU",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Luxemburg",       "LUX",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Malta",           "MLT",    2017,   (24166 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "The Netherlands", "NLD",    2017,   (49424 * 1.04) * 1.13, (150570 * 1.04) * 1.13,
  "Poland",          "POL",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Portugal",        "PRT",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Slovakia",        "SVK",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Slovenia",        "SVN",    2017,   (24166 * 1.04) * 1.13, (24166  * 1.04) * 1.13,
  "Spain",           "ESP",    2017,   (57231 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "Sweden",          "SWE",    2017,   (57213 * 1.04) * 1.13, (170744 * 1.04) * 1.13,
  "United Kingdom",  "GBR",    2017,   (64777 * 1.04) * 1.13, (170744 * 1.04) * 1.13
)

us_data_2018 <- tribble(
  #US data from CDC: https://www.cdc.gov/nchhstp/newsroom/docs/factsheets/costly-burden-dr-tb-508.pdf

  ~country,          ~iso3,    ~index,  ~cpc_mdr,     ~cpc_xdr,
#./................../........./......./............../..........
  "United States",   "USA",    2018,   175000,        544000
)

exp_data <- rbind(eu_data_2012, us_data_2018) %>% 
  full_join(who_data_2017) %>% 
  arrange(country)
  
# Inflation data from World Bank API
inflation_data <- wb(country = exp_data$iso3, indicator = "FP.CPI.TOTL.ZG", startdate = 2015, enddate = 2017) %>% 
  transmute(year = date,
            country,
            iso3 = iso3c,
            value)

# Create avg inflation for 2015-17
inflation_avg <- inflation_data %>% 
  group_by(country) %>% 
  summarise(inflation = mean(value) / 100)


# TIDY DATA ---------------------------------------------------------------

countries <- exp_data %>% 
  semi_join(inflation_data, by = "iso3") %>% 
  semi_join(ihme_data, by = "country")
countries <- countries$country

incidence <- ihme_data %>% 
  filter(country %in% countries)


# RUN ---------------------------------------------------------------------

fc_tb(countries)
