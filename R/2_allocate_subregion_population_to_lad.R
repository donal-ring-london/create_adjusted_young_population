## script to get local authority level population data using gp data to allocate subregion estimates

## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gsscoder)
library(data.table)

functions_to_read <- list.files("R/functions")

lapply(
  paste0("R/functions/", functions_to_read),
  FUN = source
)


## 1. reading in the data and lookups

pupil_pop <- readRDS("data/intermediate/total_pupils_itl_5_15_2015_2024.rds")

pupil_pop_lad <- readRDS("data/intermediate/total_pupils_lad_5_15_2015_2024.rds")

gp_data <- readRDS("data/intermediate/gp_sya_lad.rds")

la_itl_lookup <- read_csv("lookups/la_itl_lookup_all.csv")


## 2. various bits of cleaning on the dataset

gp_data <- gp_data %>% 
  mutate(year = as.numeric(substr(extract_date, 1, 4)), 
         month = as.numeric(substr(extract_date, 6, 7))) %>%
  filter(month == 07 & year %in% 2015:2024 & age %in% 5:15 & sex != "persons") %>%
  select(gss_code, year, sex, age, value)


## 3. joining on the subregions and calculating the scaling factors

la_itl_lookup <- unique(la_itl_lookup[, c("ladxxcd", "itl221cd")]) ## the xx is in there because this lookup contains several years of local authorities, across a few updates

gp_data <- gp_data %>% # joining on itl areas, and getting the total population 5-14 in each itl
  left_join(la_itl_lookup, by = c("gss_code" = "ladxxcd")) %>%
  group_by(itl221cd, year) %>%
  mutate(itl_total_value = sum(value)) %>%
  ungroup()

scaling_factors <- gp_data %>% # creating the scaling factors, by dividing the value of each la-sex-age cell by the itl annual totals
  mutate(scaling_factor = value/itl_total_value) %>%
  select(itl221cd, gss_code, year, sex, age, scaling_factor)


## 4. applying the scaling factors to total population of pupils to get the initial estimates

pupil_population_at_itl <- pupil_pop %>% # getting total pupils 5-15 at itl - currently it's by single year of age. 
  group_by(itl221cd, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

population_estimates_la <- scaling_factors %>% # calculating the estimates by applying the scaling factors to the total itl population
  left_join(pupil_population_at_itl, by = c("itl221cd", "year")) %>%
  mutate(population = scaling_factor*value) %>% 
  select(gss_code, year, age, sex, population)


## 5. saving the output

saveRDS(object = population_estimates_la,
        file = "data/intermediate/adjusted_population_estimates_5_15_lad.rds") # the lad version that it uses will be whatever is used in the gp data. Should formalise the setting of this and state it clearly somewhere. 




