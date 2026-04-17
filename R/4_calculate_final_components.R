
## script to derive all missing components for ages 0-14
## later on it might make more sense to split this into two or more scripts, but for now keeping it in one

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

adj_mye_0_4 <- readRDS("data/intermediate/adjusted_mye_0_4.rds")

adj_pop_5_15 <- readRDS("data/intermediate/adjusted_population_estimates_5_15_lad.rds")

og_myes <- readRDS("data/intermediate/mye_2001_24_rev.rds") %>%
  filter(year >= 2011)

gss_code_name_lookup <- og_myes %>% ## creating this lookup.
  select(gss_code, gss_name) %>%
  unique()


## 2. get net international via residual change for ages 5-14 

res_change_data <- og_myes %>% 
  filter(component %in% c("births", "deaths", "internal_net") & year %in% 2015:2024 & age %in% 5:14) %>% # the years is something I'll need to automate
  pivot_wider(names_from = component, values_from = value) %>% 
  left_join(adj_pop_5_15, by = c("gss_code", "sex", "age", "year")) %>%
  mutate(cohort = year - age)

res_change_data <- adj_mye_0_4 %>% # we need age 4 to calculate residual change for age 5 and to therefore estimate international migration. So we're binding it on here. This is a sort of ugly way to do and there are probably too many lines of code, so should go over this.
  filter(component %in% c("births", "deaths", "internal_net", "population") & year %in% 2015:2024 & age == 4) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  mutate(cohort = year - age) %>%
  rbind(res_change_data)

res_change_data_prev <- res_change_data %>%
  mutate(year = year + 1) %>% 
  select(gss_code, gss_name, sex, year, cohort, population) %>%
  rename(population_prev = population)

new_international_net_5_15 <- res_change_data %>%
  filter(age >= 5 & year >= 2016) %>%
  left_join(res_change_data_prev, by = c("gss_code", "gss_name", "sex", "year", "cohort")) %>%
  mutate(international_net = population - population_prev - births - internal_net + deaths) %>%
  select(gss_code, gss_name, sex, year, age, international_net)


## 3. using new international net calculated above for ages 5 upwards to adjust the international flows calculated in script 3 for ages 0-4
## because, for cohorts 2020 onwards, we just rolled forward international flows estimated previously for those ages. Which means that we'll miss out on any real trends in flows after that date. So we use the flows calculated above to add in those trends. 
## this is a harder problem that I thought it would be...a real issue is the negatives in the current flows

test <- mye_2010_24 %>%
  mutate(cohort = year - age) %>%
  filter(year >= 2020 & cohort <= 2019 & component == "international_net")




## 3. aligning net international flows with original gross international flows for all ages from 0 to 14, and adjusting gross to fit net to come up with final international flows estimates

new_international_net_0_15 <- adj_mye_0_4 %>% ## extracting international_net from the new estimates, binding together 0 to 4 and 5 to 14, and getting them in the same format as the original mid year estimates
  filter(component == "international_net") %>%
  rename(international_net = value) %>%
  select(gss_code, gss_name, sex, year, age, international_net) %>%
  rbind(new_international_net_5_15) %>%
  arrange(gss_code, gss_name, sex, age, year) %>%
  filter(!grepl("W", gss_code))

flows_to_adjust <- og_myes %>% ## extracting gross flows (in and out international flows) from the original mid year estimates, which will be adjusted below to match the new net flows
  filter(component %in% c("international_in", "international_out")) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  right_join(new_international_net_0_15, by = c("gss_code", "gss_name", "sex", "year", "age")) %>%
  filter(!grepl("W", gss_code))

adjusted_gross_flows <- optimise_gross_flows(base_in = flows_to_adjust$international_in, ## adjusting the original gross flows from the mid year estimates to match the new net flows we've calculated
                                             base_out = flows_to_adjust$international_out,
                                             target_net = flows_to_adjust$international_net)

adjusted_gross_flows_dt <- rbindlist(lapply( ## cleaning up the new dataset - syntax a bit unpleasant and inconsistent, but something to fix later
  X = adjusted_gross_flows,
  FUN = function(x){as.list(x)}
))

new_international_net_0_15$international_in <- adjusted_gross_flows_dt$inflow
new_international_net_0_15$international_out <- adjusted_gross_flows_dt$outflow


## 4. getting all components into the one dataset, so that we end up with a full alternative set of mid year estimates

adj_mye_5_15 <- adj_pop_5_15 %>% ## reformatting the adjusted population estimates for 5-14, joining with other components below
  rename(value = population) %>%
  mutate(component = "population") %>%
  left_join(gss_code_name_lookup, by = "gss_code") %>%
  select(gss_code, gss_name, sex, age, value, year, component)

adj_mye_5_15 <- og_myes %>%
  filter(component %in% c("births", "deaths", "internal_in", "internal_net", "internal_out") & age %in% 5:15 & year >= 2015) %>%
  rbind(adj_mye_5_15)

new_international_net_0_15 <- new_international_net_0_15 %>% ## reshaping and reformatting the international flows estimates
  pivot_longer(cols = c(international_net, international_in, international_out), names_to = "component") %>%
  select(gss_code, gss_name, sex, age, value, year, component)

adjusted_mye_0_15_full <- adj_mye_0_4 %>%
  filter(component != "international_net") %>% # filtered out because as well as being in this dataset, they are also in the dataset with all international flows, and they need to be filtered out somewhere...
  rbind(adj_mye_5_15, new_international_net_0_15) %>%
  arrange(gss_code, gss_name, sex, year, age, component) %>%
  filter(!grepl("W", gss_code))


## 5. some checks, saving the dataset

adjusted_mye_0_15_full <- adjusted_mye_0_15_full %>% ## keeping to just population for 2015, getting rid of other components in dataset
  filter(!(year == 2015 & component %in% c("births", "deaths", "internal_in", "internal_net", "internal_out", "international_in", "international_net", "international_out")))

saveRDS(object = adjusted_mye_0_15_full, 
        file = "data/processed/adjusted_mye_0_15_full.rds")

