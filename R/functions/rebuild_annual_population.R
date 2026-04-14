library(dplyr)
library(tidyr)

#takes the population by birth cohort at yr_start, plus births for years between yr_start+1 and yr_end,
#and applies annual cumulative change by cohort.  Returns resulting the population by year and age.

 rebuild_annual_population <- function(start_cohort, cumulative_change_cohort, yr_start, yr_end, max_age = 90) {

   annual_population <- start_cohort  %>%
     filter(gss_code %in% unique(cumulative_change_cohort$gss_code)) %>%
     expand_grid(year = c(yr_start:yr_end)) %>%
     left_join(cumulative_change_cohort, by = c("gss_code", "gss_name", "sex", "year", "cohort")) %>%
     replace_na(list(annual_change = 0)) %>%
     group_by(gss_code, gss_name, sex, cohort) %>%
     mutate(cumulative_change = cumsum(annual_change)) %>%
     ungroup() %>%
     mutate(value = round(value_start + cumulative_change), 0) %>%
     mutate(age = case_when(
       year - cohort < max_age ~ year - cohort,
       TRUE ~ max_age)) %>%
     filter(age >= 0) %>%
     group_by(gss_code, gss_name, sex, year, age) %>%
     summarise(value = sum(value), .groups = "drop") %>%
     mutate(component = "population")

   return(annual_population)
 }
