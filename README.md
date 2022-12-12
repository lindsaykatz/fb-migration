# fb-migration

## data
This folder contains all the data used in our analysis. 
1. `pop_data_clean_age.csv` and `pop_data_clean_no_age.csv` have the cleaned daily and monthly population count data disaggregated by age and not disaggregated by age, respectively
2. `travel_data_clean_age.csv` and `travel_data_clean_no_age.csv` have the cleaned daily and monthly traveller count data disaggregated by age and not disaggregated by age, respectively
3. `rates_age.csv` and `rates_no_age.csv` contain data on rates of travel, disaggregated by age and not disaggregated by age, respectively. Each CSV has the observed rate (`obs_rate`) and the estimated rate and its associated standard error (`loess_rate` and `loess_se`), where the smoothed estimates were extracted from a loess model fit individually for each age/sex group for each region.

## plots
The PDF in this folder contains the plots of raw traveller rates for each region over time, disaggregated by age group and sex.
