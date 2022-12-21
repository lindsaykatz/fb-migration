# this script will compute the metrics for our analysis

library(tidyverse)
library(zoo)

######## READ IN CLEAN DATA ########
travel_df_age <- read_csv("fb-migration/data/travel_data_clean_age.csv", show_col_types = F)
pop_df_age <- read_csv("fb-migration/data/pop_data_clean_age.csv", show_col_types = F)
travel_df_no_age <- read_csv("fb-migration/data/travel_data_clean_no_age.csv", show_col_types = F)
pop_df_no_age <- read_csv("fb-migration/data/pop_data_clean_no_age.csv", show_col_types = F)

######## CALCULATING RATES ########
# we want to take set of dates where traveler data exist to be one we calculate rates for
# let's check where we are missing population data for that date (after or on 2020-05-29 for no age b/c that's when pop starts being collected)

#then if the population data dont exist for that date, then use the data from the closest date.
#if there's two closest dates, use the previous day

# not an issue for data without age (filter for when we population data collection begins)
nrow(setdiff(distinct(travel_df_no_age, date) %>% filter(date>='2020-05-29'), distinct(pop_df_no_age, date)))

# for data disaggregated by age, missing 28 dates of denominators
nrow(setdiff(distinct(travel_df_age, date), distinct(pop_df_age, date)))

# lets add rows to the population dataset for each of these dates, then fill using the closest available date
# extract the missing dates as a vector
missing_dates <- setdiff(distinct(travel_df_age, date), distinct(pop_df_age, date)) %>% pull()

# for each date, we want to add a row for each region, age, sex group (this gives us 390 rows per date)
# to do so, loop through each missing date and add those rows
for (i in 1:length(missing_dates)){
  pop_df_age <- pop_df_age %>% select(sex, region, age_gp) %>% 
    unique() %>% 
    mutate(date = missing_dates[i]) %>% 
     bind_rows(., pop_df_age)
}

# now that we've added the rows, group by region, sex and age group and arrange by date so we can fill in our denominators correctly
# for simplicity, just fill down
pop_df_age <- pop_df_age %>% group_by(region, sex, age_gp) %>% arrange(date) %>% 
  fill(c(daily, monthly), .direction = "down") %>% 
  ungroup()

# now, we can merge the population and travel data - age
d_merged_age <-  left_join(travel_df_age, pop_df_age, by = c("sex", "region", "age_gp", "date")) %>% 
  rename(daily_trav = daily.x,
         monthly_trav = monthly.x,
         daily_pop = daily.y,
         monthly_pop = monthly.y) %>% 
  select(date, sex, region, age_gp, daily_trav, monthly_trav, daily_pop, monthly_pop)

# now, we can merge the population and travel data - no age
# we filter for anything on or after 29 may 2020 b/c that's when population data start being collected
d_merged_no_age <-  left_join(travel_df_no_age, pop_df_no_age, by = c("sex", "region", "date")) %>% 
  rename(daily_trav = daily.x,
         monthly_trav = monthly.x,
         daily_pop = daily.y,
         monthly_pop = monthly.y) %>% 
  filter(date>='2020-05-29') %>% 
  select(date, sex, region, daily_trav, monthly_trav, daily_pop, monthly_pop)

# let's filter out rows where daily pop is 0, or daily pop is greater than 11000 and daily travelers is 0
# this cutoff is based on smaller regions like Yukon where we can realistically expect 0 travellers
df_age <- d_merged_age %>% 
  mutate(daily_pop = ifelse(daily_pop==0, NA, daily_pop)) %>% 
  filter(daily_trav==0 & daily_pop<=11000 | daily_trav!=0) %>% 
  filter(is.na(daily_pop) | daily_trav <= daily_pop)

df_no_age <- d_merged_no_age %>% 
  mutate(daily_pop = ifelse(daily_pop==0, NA, daily_pop)) %>% 
  filter(daily_trav==0 & daily_pop<=11000 | daily_trav!=0) %>% 
  filter(is.na(daily_pop) | daily_trav <= daily_pop)

# fill in missing stuff with closest previous day
df_age <- df_age %>% group_by(region, age_gp, sex) %>% arrange(date) %>% 
  fill(c(daily_pop, monthly_pop, daily_trav, monthly_trav), .direction = "downup") %>%
  ungroup()

df_no_age <- df_no_age %>% group_by(region, sex) %>% arrange(date) %>% 
  fill(c(daily_pop, monthly_pop, daily_trav, monthly_trav), .direction = "down") %>%
  ungroup()

# need to fix issue where place doesnt have all groups available for july 1st 2020 or 2021

# only one case of this in df age, I add the row for that demographic group then fill down b/c previous day is June 30
df_age <- df_age %>%
  add_row(region="South Dakota",
          sex = as.factor("M"),
          date = as.Date('2020-07-01'),
          age_gp = "50-65") %>% 
  arrange(date) %>% 
  group_by(region, sex, age_gp) %>% 
  fill(c(daily_trav, monthly_trav, daily_pop, monthly_pop), .direction = "down") %>% 
  ungroup()

# two cases in df no age, same idea as above
df_no_age <- df_no_age %>%
  add_row(region="British Columbia",
          sex = as.factor("F"),
          date = as.Date('2020-07-01')) %>%
  add_row(region="Nevada",
          sex = as.factor("F"),
          date = as.Date('2020-07-01')) %>% 
  arrange(date) %>% 
  group_by(region, sex) %>% 
  fill(c(daily_trav, monthly_trav, daily_pop, monthly_pop), .direction = "down") %>% 
  ungroup()

# compute daily and monthly rate of travel
df_age <- df_age %>% 
  mutate(daily_rate = daily_trav/daily_pop,
         monthly_rate = monthly_trav/monthly_pop)

df_no_age <- df_no_age %>% 
  mutate(daily_rate = daily_trav/daily_pop,
         monthly_rate = monthly_trav/monthly_pop)

# compute 3 day rolling average (in case we want it later)
df_age <- df_age %>%
  arrange(region) %>% 
  group_by(sex, region, age_gp) %>% 
  mutate(daily_rate_smooth = rollmean(daily_rate, k=3, fill=NA)) %>% 
  ungroup()

df_no_age <- df_no_age %>%
  arrange(region) %>% 
  group_by(sex, region) %>% 
  mutate(daily_rate_smooth = rollmean(daily_rate, k=3, fill=NA)) %>% 
  ungroup()

######## PLOTS - RATES ########
# note that i modified the code and the previous PDF is based on the smoothed rate, but i think we want to use the raw rate
# get names of regions without age breakdown to loop through
regions_no_age <- df_no_age %>%
  select(region) %>% 
  distinct() %>% 
  pull()

# pdf of plots for whole time period (not by age)
# start pdf
pdf("fb_rates_no_age.pdf", width = 8, height = 6)

# loop through regions
for(i in 1:length(regions_no_age)){
  p <- df_no_age %>% 
    filter(region==regions_no_age[i]) %>% 
    ggplot(aes(x=date, y=daily_rate, fill=sex, color=sex))+
    geom_smooth()+
    geom_point()+
    theme_bw()+
    scale_x_date(date_labels = "%b %Y")+
    facet_wrap(.~sex)+
    labs(x="Time", y = "Daily rate of travellers", 
         title="Traveller rate over time, by sex",
         subtitle = paste0(regions_no_age[i]))
  print(p)
}

# shut down pdf
dev.off()

# get names of regions with age breakdown to loop through
regions_age <- df_age %>%
  select(region) %>% 
  distinct() %>% 
  pull()

# start pdf
pdf("fb_rates_age.pdf", width = 8, height = 6)

# loop through regions
for(i in 1:length(regions_age)){
  p <- df_age %>% 
    filter(region == regions_age[i]) %>% 
    arrange(date) %>% 
    ggplot(aes(x=date, y=daily_rate, color=age_gp))+
    geom_point()+
    geom_smooth(aes(fill=age_gp))+
    theme_bw()+
    facet_wrap(.~as.factor(sex))+
    scale_x_date(date_labels = "%b %Y")+
    labs(title = "Traveller rate over time, by age group and sex",
         subtitle = paste0(regions_age[i]),
         y = "Daily rate of travellers", 
         x = "Time", color="Age Group", fill="Age Group")
  print(p)
}

# shut down pdf
dev.off()

###### EXTRACT SMOOTH LOESS ESTIMATES ######

# first split the dataset into the groups we want, then fit the model
# need to correct class of date variable for model
models_age <- df_age %>% mutate(date=as.numeric(date)) %>% 
  split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map(~stats::loess(daily_rate ~ date, span=0.5, data=.x))

models_no_age <- df_no_age %>% mutate(date=as.numeric(date)) %>%
  split(f=list(.$region, .$sex)) %>% 
  map(~stats::loess(daily_rate ~ date, span=0.5, data=.x))

# now, use the predict function to get smooth estimates, for data with age
predict.models_age <- models_age %>% map( ~ predict(.x, se=T))

predict.models_no_age <- models_no_age %>% map( ~ predict(.x, se=T))

# grab dates for each region/sex/age group, so we can bind those onto the df of the smoothed estimates
dates_age <- df_age %>% split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map( ~ .$date %>% as_tibble() %>% rename(date = value))

dates_no_age <- df_no_age %>% split(f=list(.$region, .$sex)) %>% 
  map( ~ .$date %>% as_tibble() %>% rename(date = value))

# just grab the fit and standard error, so we have a tibble we can bind the dates onto for each group
loess_ests_age <- predict.models_age %>% 
  map( ~ as_tibble(.) %>% select(fit, se.fit))

loess_ests_no_age <- predict.models_no_age %>% 
  map( ~ as_tibble(.) %>% select(fit, se.fit))

# bind columns of two lists together, so for each group we have a tibble with the smoothed estimate, SE, and associated date
loess_ests_age <- map2(loess_ests_age, dates_age, ~ .x %>% bind_cols(.y))

loess_ests_no_age <- map2(loess_ests_no_age, dates_no_age, ~ .x %>% bind_cols(.y))

# combine everything into one massive tibble by binding rows - loess estimates with age
# note: replace the separator from a . to a * b/c some states have full stops in their name (i.e. Washington D.C.)
# (filter out Canada b/c we're not using that data)
loess_ests_age  <- loess_ests_age %>% bind_rows(., .id="column_label") %>% 
  mutate(column_label = str_replace(column_label, "\\.(?=M|F\\.)", "*"),
         column_label = str_replace(column_label, "(?<=\\*M|\\*F)\\.", "*")) %>% 
  separate(column_label, c("region", "sex", "age_gp"), sep="\\*") %>% 
  filter(region!="Canada")

# combine everything into one massive tibble by binding rows - loess estimates without age
# (filter out Canada b/c we're not using that data)
loess_ests_no_age  <- loess_ests_no_age %>% bind_rows(., .id="column_label") %>% 
  mutate(column_label = str_replace(column_label, "\\.(?=M|F)", "*")) %>% 
  separate(column_label, c("region", "sex"), sep="\\*") %>% 
  filter(region!="Canada")

##### YEAR ON YEAR CHANGE ######
# create tibble of all 01 July 2020 and 01 July 2021 estimates, compute percent growth
yr_on_yr_age <- loess_ests_age %>% filter(date=="2020-07-01" | date=="2021-07-01") %>% 
  select(-se.fit) %>% 
  pivot_wider(., values_from = fit, names_from = date) %>% 
  rename(july1_2020 = `2020-07-01`,
         july1_2021 = `2021-07-01`) %>% 
  mutate(yr_on_yr = ((july1_2021 - july1_2020)/(july1_2020))*100)

# same for no age
yr_on_yr_no_age <- loess_ests_no_age %>% filter(date=="2020-07-01" | date=="2021-07-01") %>% 
  select(-se.fit) %>% 
  pivot_wider(., values_from = fit, names_from = date) %>% 
  rename(july1_2020 = `2020-07-01`,
         july1_2021 = `2021-07-01`) %>% 
  mutate(yr_on_yr = ((july1_2021 - july1_2020)/(july1_2020))*100)

# lets look at the top 5 positive change
yr_on_yr_age %>% arrange(desc(yr_on_yr)) %>% slice(1:5)

# now let's find the winter peak or trough - max/min between January and April 2021
# earliest date in January we have for age data is 03 Jan 2021, and then we'll use 01 April 2021 as our endpoint
peak_or_trough_age <- loess_ests_age %>% filter(date>="2021-01-03" & date<="2021-04-01") %>% 
  select(-se.fit) %>%
  left_join(., loess_ests_age %>% 
              filter(date=="2020-07-01") %>% 
              select(-se.fit) %>% 
              rename(july1_est = fit,
                     july1_2020 = date), 
            by=c("sex", "region", "age_gp")) %>% 
  mutate(diff = (fit-july1_est)/july1_est,
         abs_diff = abs(diff)) %>% 
  group_by(region) %>% 
  filter(abs_diff==max((abs_diff))) %>% 
  arrange(region) %>% 
  ungroup()

peak_or_trough_no_age <- loess_ests_no_age %>% filter(date>="2021-01-03" & date<="2021-04-01") %>% 
  select(-se.fit) %>%
  left_join(., loess_ests_age %>% 
              filter(date=="2020-07-01") %>% 
              select(-se.fit) %>% 
              rename(july1_est = fit,
                     july1_2020 = date), 
            by=c("sex", "region")) %>% 
  mutate(diff = (fit-july1_est)/july1_est,
         abs_diff = abs(diff)) %>% 
  group_by(region) %>% 
  filter(abs_diff==max((abs_diff))) %>% 
  arrange(region) %>% 
  ungroup()

# define states and provinces for max and min tables
provinces <- c("Alberta", "British Columbia", "Ontario", "Quebec", "Prince Edward Island", "Nunavut",
               "Northwest Territories", "Newfoundland and Labrador", "Yukon", "Saskatchewan", "Manitoba",
               "New Brunswick", "Nova Scotia")

states <- peak_or_trough_age %>% select(region) %>% filter(!(region %in% provinces)) %>% pull()

# top 5 positive change from July 1 2020 - both Canada and US
peak_or_trough_age %>% 
  arrange(desc(diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# top 5 positive change from July 1 2020 - just Canada
peak_or_trough_age %>% 
  filter(region %in% provinces) %>% 
  arrange(desc(diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# top 5 positive change from July 1 2020 - just US
peak_or_trough_age %>% 
  filter(region %in% states) %>% 
  arrange(desc(diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# top 5 negative change from July 1 2020
peak_or_trough_age %>% 
  arrange(diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# top 5 negative change from July 1 2020 - just Canada
peak_or_trough_age %>% 
  filter(region %in% provinces) %>% 
  arrange(diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# top 5 negative change from July 1 2020 - just US
peak_or_trough_age %>% 
  filter(region %in% states) %>% 
  arrange(diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, diff)

# bind observed rates onto dataframe with loess estimates so we have everything in one place
df_age_all <- df_age %>% 
  filter(region!="Canada") %>% 
  left_join(., loess_ests_age, by = c("sex", "age_gp", "region", "date")) %>% 
  rename(loess_rate = fit,
         loess_se = se.fit,
         obs_rate = daily_rate) %>% 
  select(-daily_rate_smooth, -monthly_pop, -monthly_rate, -monthly_trav)

df_no_age_all <- df_no_age %>% 
  filter(region!="Canada") %>% 
  left_join(., loess_ests_no_age, by = c("sex", "region", "date")) %>% 
  rename(loess_rate = fit,
         loess_se = se.fit,
         obs_rate = daily_rate) %>% 
  select(-daily_rate_smooth, -monthly_pop, -monthly_rate, -monthly_trav)

# compute root MSE
# nunavut, yukon and NWT have highest MSE - maybe we make cutoff 0.01 - notice yukon F 30-50 is RMSE of 0 b/c every single rate is 0.
rmse_age <- df_age_all %>% 
  mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex, age_gp) %>% 
  summarise(rmse=sqrt(mean(sq_diff)))

# same as age, nunavut, yukon and NWT have highest MSE
rmse_no_age <- df_no_age_all %>% 
  mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex) %>% 
  summarise(rmse=sqrt(mean(sq_diff)))

# export the cleaned datasets with estimates
write.csv(df_age_all, "~/Desktop/RA/fb_proj/fb-migration/data/rates_age.csv", row.names = F)
write.csv(df_no_age_all, "~/Desktop/RA/fb_proj/fb-migration/data/rates_no_age.csv", row.names = F)