## script to model population for ages 0 to 4

## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gsscoder)
library(data.table)
library(readxl)

functions_to_read <- list.files("R/functions")

lapply(
  paste0("R/functions/", functions_to_read),
  FUN = source
)


## 1. reading in data and lookups

adjusted_popest <- readRDS("data/intermediate/adjusted_population_estimates_5_15_lad.rds")

mye_2010_24 <- readRDS("data/intermediate/mye_2001_24_rev.rds") %>%
  filter(year >= 2010)


## 2. age on and apply components to births to get the population aged 5 without international migration

modelled_flows_null <- mye_2010_24 %>%
  filter(component %in% c("international_net", "international_out", "international_in")) %>%
  mutate(value = 0)

rebuilt_population <- create_pop_series(mye_coc = mye_2010_24, 
                          modelled_flows = modelled_flows_null,
                          yr_start = 2010,
                          yr_end = 2024,
                          age_max = 90) %>%
  filter(age == 5 & component == "population" & year >= 2015)


## 3. calculate the residual (the difference between the population aged 5 created above, and the pupil-based population aged 5) and apply over the cohort uniformly as net international migration

residual_difference <- adjusted_popest %>% ## getting the annual net migration figure estimates, by getting the residual difference and dividing by 6. 
  filter(age == 5) %>% 
  left_join(rebuilt_population, by = c("gss_code", "year", "age", "sex")) %>%
  mutate(cumulative_international_net = population - value) %>%
  mutate(annual_international_net = cumulative_international_net/6) %>%
  mutate(cohort = year - 5) %>% # yes, the last two could be in the same mutate call, and the first two could be together in the one more complicated calculation, but I think it's all clearer this way
  select(gss_code, gss_name, cohort, sex, annual_international_net)

adjusted_international_flows <- expand_grid(gss_code = unique(residual_difference$gss_code),
            cohort = unique(residual_difference$cohort), 
            sex = unique(residual_difference$sex), 
            age = 0:5) %>%
  left_join(residual_difference, by = c("gss_code", "cohort", "sex")) %>%
  mutate(year = cohort + age, component = "international_net") %>%
  rename(value = annual_international_net) %>%
  select(gss_code, gss_name, sex, age, value, year, component)


## 4. rebuild the 0-5 population again, this time using the new international flows calculated above, to create the population series along with components of change that will line up with the pupil-based series

rebuilt_population <- create_pop_series(mye_coc = mye_2010_24, 
                          modelled_flows = adjusted_international_flows,
                          yr_start = 2010,
                          yr_end = 2024,
                          age_max = 90
                        ) %>%
  mutate(cohort = year - age) %>%
  filter(cohort <= 2019 & year >= 2015 & age %in% 0:5, ) # we only have flows for cohorts 2019 and before, and we only the full full rebuilt series for all ages from 2015 onwards


## 5. roll forward the adjusted net international flows estimates 

  ### 5.1. initial calculation/modelling of international flows for ages 0-5 for cohorts 2020 onwards, based on rolling forward past averages by age
adjusted_international_flows <- adjusted_international_flows %>% 
  mutate(cohort = year - age) %>% 
  filter(cohort %in% 2015:2019) # taking the past 5 years of flow data, for each cohort. Will need to put more thought into how to properly automate this. 

mean_flows <- adjusted_international_flows %>% # just crude aggregation of net international flows.
  group_by(gss_code, gss_name, sex, age) %>%
  summarise(mean_flow = mean(value)) %>%
  ungroup() ## flows are the same for all ages, by the same gss_code/sex combination. By the way that we have done it, that is to be expected.

rolled_forward_flows_estimates <- expand_grid(gss_code = unique(mean_flows$gss_code), 
            sex = c("female", "male"), 
            cohort = 2020:2024,
            age = 0:4) %>%
  mutate(year = cohort + age) %>%
  filter(year <= 2024) %>%
  left_join(mean_flows, by = c("gss_code", "sex", "age")) %>%
  mutate(component = "international_net") %>% ## the sort of code we'll need 
  rename(value = mean_flow) %>%
  select(gss_code, gss_name, sex, age, value, year, component)


## 6. rebuild the final estimates and join the two datasets that model 0-4 year olds

rebuilt_population_2020_plus <- create_pop_series(mye_coc = mye_2010_24,
                                                  modelled_flows = rolled_forward_flows_estimates,
                                                  yr_start = 2019, # starting at 2019 even though we only need 2020 onwards, because otherwise we lose age 0 in 2020. I don't know why - probably beacuse that's the starting cohort so it's automatically deleted, or something like that. 
                                                  yr_end = 2024,
                                                  age_max = 90) %>%
  mutate(cohort = year - age) %>%
  filter(cohort >= 2020 & age <= 4) # narrow to only cohorts from 2020 onwards and age 4

full_new_adjusted_estimates <- rbind(rebuilt_population, rebuilt_population_2020_plus) %>%
  arrange(gss_code, year, age, sex, component) %>%
  select(-cohort) %>%
  filter(age <= 4 & year >= 2015)


## NOTE - actually, binding together the two datasets shouldn't happen in this script. We have all components (aside from gross international flows) for ages 0-4, but only population for ages 5-14. So save the population for 0-4 in this script, come up with the other components in another script, and bind them when they are in the same format and have everything we need in them. And after that, get gross flows for all of them all at once.  
## 7. binding the full 0-4 data along with the 5-14 data, for just population
## also, separately saving the new flows calculated for age 5. 

adjusted_popest <- adjusted_popest %>%
  rename(value = population)

adjusted_population_0_15 <- full_new_adjusted_estimates %>%
  filter(component == "population") %>%
  select(gss_code, year, age, sex, value) %>%
  rbind(adjusted_popest) %>%
  filter(year >= 2015) # agh, two filter calls. fix that. 

age_5_new_flows <- rebuilt_population %>%
  filter(age == 5 & component == "international_net")

saveRDS(object = adjusted_population_0_15,
        file = "data/processed/adj_pop_0_15.rds") # probably should be in intermediate

saveRDS(object = full_new_adjusted_estimates, 
        file = "data/intermediate/adjusted_mye_0_4.rds")

saveRDS(age_5_new_flows, 
        file = "data/intermediate/age_5_flows.rds") ## HERE, STOPPED. Come back. 


