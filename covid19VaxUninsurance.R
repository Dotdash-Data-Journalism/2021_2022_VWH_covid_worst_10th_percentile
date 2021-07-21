library(purrr)
library(dplyr)
library(rjson)
library(readxl)
library(magrittr)
library(DatawRappr)
library(stringr)
library(readr)

DW_API <- Sys.getenv("DW_API_KEY")

# A function to throw to map_df() to ensure null values are coded as NA
safe_extract <- function(l, wut) {
  res <- l[wut]
  null_here <- map_lgl(res, is.null)
  res[null_here] <- NA
  res
}

# This grabs the latest vaccination data for (almost) every county in the US
cdcVaxByCounty <- fromJSON(file = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data") %>% 
  extract2("vaccination_county_condensed_data") %>%   
  map_df(safe_extract) %>% 
  select(Date, FIPS, StateName, StateAbbr, County, Series_Complete_12PlusPop_Pct,
         Administered_Dose1_Recip_12PlusPop_Pct) %>% 
  filter(FIPS != "UNK", !is.na(Series_Complete_12PlusPop_Pct)) %>% 
  mutate(StateAbbr = str_trim(StateAbbr), Date = base::as.Date(Date)) %>% 
  rename(`FIPS-Code` = FIPS)

# This is a lookup table for Texas counties that includes FIPS code and county name
read_excel('./all-geocodes-v2018.xlsx', skip = 4) %>% 
  filter(`State Code (FIPS)` == "48", str_detect(`Area Name (including legal/statistical area description)`, "County")) %>% 
  mutate(`FIPS-Code` = paste0(`State Code (FIPS)`, `County Code (FIPS)`)) %>% 
  rename(County = `Area Name (including legal/statistical area description)`) %>% 
  select(`FIPS-Code`, County) -> txCounties 

# Downloading Texas vaccine data from their Excel sheet...because Texas.
download.file("https://dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls", 
              destfile = "COVID-19 Vaccine Data by County.xlsx")

# Cleaning and organizing the Texas vaccine by county data
read_excel("./COVID-19 Vaccine Data by County.xlsx", skip = 4,
           sheet = "By County", col_names = F) %>% 
  select(`...1`, `...5`, `...6`, `...7`) %>% 
  mutate(Series_Complete_12PlusPop_Pct = (`...6` / `...7`) * 100,
         Administered_Dose1_Recip_12PlusPop_Pct = (`...5` / `...7`) * 100,
         StateName = "Texas",
         StateAbbr = "TX") %>% 
  rename(County = `...1`) %>% 
  mutate(County = paste(County, "County"), Date = base::as.Date(pull(read_excel("./COVID-19 Vaccine Data by County.xlsx", 
                                                             range = "B2", col_names = F)))) %>% 
  select(-c(`...5`, `...6`, `...7`)) %>% 
  left_join(txCounties, by = "County") %>% 
  filter(!is.na(`FIPS-Code`)) -> txVax

# Combining TX and rest-of-US vax numbers, ordering them by fully vaccinated 12+ rate,
# and then creating a percentile column.
bind_rows(cdcVaxByCounty, txVax) %>% 
  arrange(Series_Complete_12PlusPop_Pct) %>% 
  mutate(Percentile_Vax = percent_rank(Series_Complete_12PlusPop_Pct)) -> cdcVaxByCountyFull


#######################################################
# This is getting the Covid by county data and calculating the
# 10th percentile and worse for Covid cases in the country.
# This also have poverty rate, uninsurance rate, and total population 
# by county data.

fromJSON(file = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_latest_external_data") %>% 
  extract2("integrated_county_latest_external_data") %>%   
  map_df(safe_extract) %>% 
  select(fips_code, State_name, 
         County,
         report_date,
         cases_per_100K_7_day_count_change,
         percent_uninsured_2019,
         poverty_rate_2019,
         percent_uninsured_2019_US_avg,
         poverty_rate_2019_US_avg,
         total_population_2019) %>% 
  mutate(fips_code = if_else(nchar(fips_code) < 5, paste0('0', as.character(fips_code)), as.character(fips_code)),
         Percentile_Covid = percent_rank(desc(cases_per_100K_7_day_count_change))) %>% 
  filter(Percentile_Covid <= .1) -> casesUninsurancePovertyCounty

# Need to get US-wide vax rate from the different CDC AJAX resource
USVaxRate <- fromJSON(file = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data") %>% 
  extract2("vaccination_data") %>% 
  map_df(safe_extract) %>% 
  filter(ShortName == "USA") %>% 
  select(Series_Complete_12PlusPop_Pct) %>% 
  pull()

# Now joining on the vax rates for those 10th percentile and worse counties from https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data
combo <- casesUninsurancePovertyCounty %>% 
  inner_join(cdcVaxByCountyFull, by = c("fips_code" = "FIPS-Code")) %>% 
  mutate(US_Series_Complete_12PlusPop_Pct_Natl_Avg = USVaxRate) 


### Census ACS & Covid Comparison Charts
## Covid 10th percentile and worse vs US average
# covidRateComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#        `Covid-19 Cases Per 100K` = c(fromJSON(file = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_latest_external_data") %>% 
#          extract2("integrated_county_latest_external_data") %>%   
#          map_df(safe_extract) %>% 
#          select(cases_per_100K_7_day_count_change) %>%
#          pull() %>% 
#          median(na.rm = T), median(combo$cases_per_100K_7_day_count_change, na.rm = T)))
# 
# write_csv(covidRateComparison, "./chartData/covidRateComparison.csv")

## Pct Uninsured 10th percentile and worse vs US average
# uninsuredRateComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#        `Percent Uninsured` = c(unique(combo$percent_uninsured_2019_US_avg), 
#                                median(combo$percent_uninsured_2019, na.rm = T)))
# write_csv(uninsuredRateComparison, "./chartData/uninsuredRateComparison.csv")

## Poverty Rate 10th percentile and worse vs US average
# povertyRateComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#        `Poverty Rate` = c(unique(combo$poverty_rate_2019_US_avg),
#                           median(combo$poverty_rate_2019, na.rm = T)))
# write_csv(povertyRateComparison, "./chartData/povertyRateComparison.csv")

## Vax Rate 10th percentile and worse vs US average
# vaxRateComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#        `Vaccination Rate` = c(unique(combo$US_Series_Complete_12PlusPop_Pct_Natl_Avg), 
#                               median(combo$Series_Complete_12PlusPop_Pct, na.rm = T)))
# 
# write_csv(vaxRateComparison, "./chartData/vaxRateComparison.csv")

## Median income lowest 10th percentile vs. worst 10th percentile COVID
## https://data.census.gov/cedsci/table?q=median%20income&tid=ACSST5Y2019.S1903
## Nationwide (Household) Median Income: $62,843
# worst10MedianIncome <- read_csv("./censusACS19Data/medianIncomeCountyACS19.csv", col_types = "ccci") %>% 
#   inner_join(combo, by = c("GEO_ID" = "fips_code")) %>% 
#   select(MEDIAN_INCOME_EST) %>% 
#   pull() %>% 
#   median(na.rm = T)
# 
# medianIncomeComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#        `Median Income` = c(62843L, worst10MedianIncome))
# 
# write_csv(medianIncomeComparison, "./chartData/medianIncomeComparison.csv")

## Age 10th percentile and worse vs US average from table S0101
## https://data.census.gov/cedsci/table?q=median%20age&tid=ACSST5Y2019.S0101
## 38.1 Median US age

# worst10MedianAge <- read_csv("./censusACS19Data/medianAgeCountyACS19.csv", col_types = "cccd") %>% 
#   inner_join(combo, by = c("GEO_ID" = "fips_code")) %>% 
#   select(MEDIAN_AGE_EST) %>% 
#   pull() %>% 
#   median(na.rm = T)
# 
# medianAgeComparison <- tibble(group = c("Nationwide", "Counties in bottom 10th Covid Percentile"),
#                                  `Median Age` = c(38.1, worst10MedianAge))
# 
# write_csv(medianAgeComparison, "./chartData/medianAgeComparison.csv")


### For EDUCATION and RACE need to take raw number estimates for each county, sum them
### for each group, and then calculate proportions. 

## Education 10th Percentile vs. US average from table S1501
## From https://data.census.gov/cedsci/table?q=education&tid=ACSST5Y2019.S1501
# worst10Edu <- read_csv("./censusACS19Data/educationCountyACS19.csv", col_types = "ccciiiiiiiii") %>% 
#   inner_join(combo, by = c("GEO_ID" = "fips_code"))
# 
# worst10Edu[,4:10] %>% reduce(sum, na.rm = T) %>% sum() -> totalEduPop
# 
# eduComparison <- tibble(edu_level = c("Less 9th Grade", "9th to 12th grade no diploma",
#                      "High School Grad or Equivalent", "Some College No Degree",
#                      "Associates Degree", "Bachelors Degree", "Graduate or Professional Degree",
#                      "High School Grad or Higher", "Bachelors Degree or Higher"),
#        `US Average` = c(5.1, 6.9, 27.0, 20.4, 8.5, 19.8, 12.4, 88.0, 32.1),
#        `10th Percentile & Worse Covid` = c((sum(worst10Edu$Less_9th, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$`9th_to_12th_no_diploma`, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$HS_grad_or_equ, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$Some_College_no_Degree, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$Associates, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$Bachelors, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$Graduate_Professional, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$HS_grad_higher, na.rm = T) / totalEduPop) * 100,
#                                            (sum(worst10Edu$Bachelors_higher, na.rm = T) / totalEduPop) * 100)) %>% 
#   mutate(`10th Percentile & Worse Covid` = map_dbl(`10th Percentile & Worse Covid`, round, digits = 1))
# 
# write_csv(eduComparison, "./chartData/eduComparison.csv")



## Race 10th Percentile vs. US average from table DP05
## From https://data.census.gov/cedsci/table?q=race&tid=ACSDP5Y2019.DP05
# worst10Race <- read_csv("./censusACS19Data/raceCountyACS19.csv", col_types = "ccciiiiiiii") %>% 
#   inner_join(combo, by = c("GEO_ID" = "fips_code"))
# 
# worst10Race[,4:11] %>% reduce(sum, na.rm = T) %>% sum() -> totalRacePop
# 
# raceComparison <- tibble(race = c("Hispanic or Latino", "White", "Black",
#                                   "American Indian & Alaska Native", "Asian",
#                                   "Native Hawaiian or other Pacific Islander", "Other Race", "Two or more races"),
#                         `US Average` = c(18.0, 60.7, 12.3, 0.7, 5.5, 0.2, 0.2, 2.4),
#                         `10th Percentile & Worse Covid` = c((sum(worst10Race$`Hispanic or Latino`, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$White, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$Black, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$`American Indian & Alaska Native`, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$Asian, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$`Native Hawaiian or other Pacific Islander`, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$`Other Race`, na.rm = T) / totalRacePop) * 100,
#                                                             (sum(worst10Race$`Two or more races`, na.rm = T) / totalRacePop) * 100)) %>% 
#   mutate(`10th Percentile & Worse Covid` = map_dbl(`10th Percentile & Worse Covid`, round, digits = 1))
# 
# write_csv(raceComparison, "./chartData/raceComparison.csv")

### CSV for county map ###
write_csv(combo, "./chartData/10thWorstPctCountyMap.csv")

### Updating the Datawrapper Chart ###
dw_data_to_chart(x = combo, chart_id = "I3Kke", api_key = DW_API)

combo %>% 
  group_by(State_name) %>% 
  summarize(numCounties = n()) %>% 
  arrange(desc(numCounties)) %>% 
  pull(State_name) -> mostStates

highestStates <- mostStates[1:3]

dw_edit_chart(chart_id = "I3Kke",
              intro = paste0(mostStates[1], ", ", mostStates[2], ", and ", 
                             mostStates[3], " have the most counties with the highest COVID-19 exposure"),
              annotate = paste0("Data is as of ", format(base::as.Date(unique(combo$Date)),
                                                         "%m/%d/%Y")),
              api_key = DW_API)


