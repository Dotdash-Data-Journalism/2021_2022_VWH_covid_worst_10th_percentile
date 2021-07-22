# COVID-19 Most Impacted Percentile

This repo has an R file that takes the most recent COVID-19 [data by county](https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_latest_external_data) from the CDC and calculates which counties are in the 10th percentile or worse in **COVID-19 cases per 100K people**, which is around 300 counties nationwide.

This R script also takes the most recent [COVID-19 vaccination data by county](https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data) and combines it with the COVID-19 vaccination by county data from the [state of Texas](https://dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls) to have an _almost_ complete picture of the 12 yr. old+ population in each county that has been completely vaccinated from COVID-19.

Finally, the script combines these two data sources to display on a US map the counties in the 10th or worst percentile of COVID-19 cases per 100K people along with said county's vaccination percentage and medical uninsurace rate that is included along with COVID-19 case data from the CDC.

The US county interactive map is made with [Datawrapper](https://app.datawrapper.de) and updated once daily with the new COVID-19 case & vaccination data from the CDC via the Datawrapper [API](https://developer.datawrapper.de/docs). The CSV that has the underlying data for the map is written out to the file `10thWorstPctCountyMap.csv` in the `chartData` folder for reference.

Lastly, ACS 5-year 2019 Census data on race, education, poverty, and income were downloaded and put into the `censusACS19Data` folder in order to compare the cohort of the 10th percentile or worse in COVID-19 per 100K cases to the national average in said characteristics and might be utitlized in the future for additional comparative charts. 
