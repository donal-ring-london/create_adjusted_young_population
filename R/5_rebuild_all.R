
## rebuild population series using components

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


## 1. read in data

adjusted_mye <- readRDS("data/processed/adjusted_mye_0_15_full.rds")

og_mye <- readRDS("data/intermediate/mye_2011_24_rev.rds") %>%
  filter(year >= 2011) %>%
  filter(!grepl("W", gss_code))

## 2. replace the components in the original myes with the new ones we've calculated for the right years and cohorts etc

adjusted_mye <- adjusted_mye %>% 
                    filter(year != 2019)

mye_new_components <- og_mye %>%
  filter(!(age %in% 0:15 & year %in% 2020:2024)) %>%
  rbind(adjusted_mye) %>%
  arrange(gss_code, sex, year, age)


## 3. rebuild the population

mye_new_net_flows <- mye_new_components %>%
  filter(component %in% c("international_net", "international_out", "international_in"))

rebuilt_consistent_population <- create_pop_series(mye_coc = mye_new_components, 
                          modelled_flows = mye_new_net_flows, 
                          yr_start = 2011,
                          yr_end = 2024, 
                          age_max = 90)


## 4. save the output

saveRDS(object = rebuilt_consistent_population,
        file = "data/processed/adjusted_rebuilt_mye_backseries.rds")


