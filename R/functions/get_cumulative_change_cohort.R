library(dplyr)
library(tidyr)

#takes the mid-year estimates with components of change, together with modelled international flows
#and returns annual cumulative change by birth cohort between the specified years resulting from
#deaths, domestic and international migration.

get_cumulative_change_cohort <- function(mye_coc, modelled_flows, yr_start, yr_end) {

  cumulative_change_cohort <- mye_coc %>%
    filter(component %in% c("internal_net", "deaths")) %>%
    mutate(value = case_when(
      component == "deaths" ~ -value,
      TRUE ~ value
    )) %>%
    bind_rows(modelled_flows) %>%
    filter(!component %in% c("international_in", "international_out")) %>%
    filter(between(year, yr_start + 1, yr_end)) %>%
    mutate(cohort = year - age) %>%
    select(-age) %>%
    arrange(year) %>%
    group_by(gss_code, gss_name, sex, year, cohort) %>%
    summarise(annual_change = sum(value), .groups = "drop")

  return(cumulative_change_cohort)
}
