library(dplyr)

#takes the mid-year estimates with components of change, and returns deaths and net domestic
#migration by birth cohort between the specified start and end years.
#as population estimates have a maximum age, the results are forced to fit with the
#oldest birth cohort at the end year

get_deaths_net_domestic_cohort <- function(mye_coc, yr_start, yr_end, age_max = 90) {

  cohort_max  = yr_end - age_max

  annual_deaths_net_domestic <- mye_coc %>%
    filter(component %in% c("internal_net", "deaths")) %>%
    filter(between(year, yr_start + 1, yr_end)) %>%
    pivot_wider(names_from = "component", values_from = "value") %>%
    mutate(cohort = year - age) %>%
    mutate(cohort = case_when(
      cohort > cohort_max ~ cohort,
      TRUE ~ cohort_max)) %>%
    group_by(gss_code, gss_name, year, sex, cohort) %>%
    summarise(internal_net = sum(internal_net),
              deaths = sum(deaths),
              .groups = "drop")

  deaths_net_domestic_cohort <- annual_deaths_net_domestic %>%
    group_by(gss_code, gss_name, sex, cohort) %>%
    summarise(deaths = sum(deaths),
              internal_net = sum(internal_net),
              .groups = "drop")

  return(deaths_net_domestic_cohort)
}
