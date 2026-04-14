library(dplyr)
library(tidyr)

source("R/functions/get_start_cohort.R")
source("R/functions/get_end_cohort.R")
source("R/functions/get_deaths_net_domestic_cohort.R")
source("R/functions/get_cumulative_change_cohort.R")
source("R/functions/rebuild_annual_population.R")

#takes the mid-year estimates with components of change and the modelled international migration flows
#returns a series with the original international migration components replaced with the modelled ones
#and consistent annual populations

create_pop_series <- function(mye_coc, modelled_flows, yr_start, yr_end, age_max = 90) {

  mye_coc <- filter(mye_coc, gss_code %in% modelled_flows$gss_code)

  start_cohort <- get_start_cohort(mye_coc, yr_start, yr_end, cohort_max = yr_start - age_max)

  cumulative_change_cohort <- get_cumulative_change_cohort(mye_coc, modelled_flows, yr_start, yr_end)

  annual_population <- rebuild_annual_population(start_cohort, cumulative_change_cohort, yr_start, yr_end)

  annual_coc <- mye_coc %>%
    filter(component %in% c("internal_net", "internal_in", "internal_out", "births", "deaths")) %>%
    bind_rows(modelled_flows) %>%
    filter(between(year, yr_start + 1, yr_end))

  modelled_backseries <- bind_rows(annual_population, annual_coc) %>%
    arrange(component, gss_code, sex, age, year)

  return(modelled_backseries)
}
