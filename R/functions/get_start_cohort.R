library(dplyr)

#takes the mye with components of change and returns the population by birth cohort at yr_start, together
#with births for years between yr_start + 1 and yr_end.
#this function is used in different contexts:
#   1. calculating the change in the size of cohorts between two years
#   2. providing a start point for reconstructing the population series
#
#The argument cohort_max is included to facilitate switching between use-cases:
# in the first case, cohort_max should be specified as the highest cohort present at yr_end
# in the second case, cohort_max should be specified as the highest cohort present at yr_start

get_start_cohort <- function(mye, yr_start, yr_end, cohort_max) {

  births <- mye %>%
    filter(component == "births",
           age == 0) %>%
    filter(between(year, yr_start + 1, yr_end)) %>%
    mutate(cohort = year) %>%
    select(-c(age, year))

  start_cohort <- mye %>%
    filter(component == "population") %>%
    filter(year == yr_start) %>%
    mutate(cohort = year - age) %>%
    mutate(cohort = case_when(
      cohort > cohort_max ~ cohort,
      TRUE ~ cohort_max)) %>%
    group_by(gss_code, gss_name, sex, cohort) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    bind_rows(births) %>%
    select(gss_code, gss_name, sex, cohort, value) %>%
    arrange(gss_code, sex, cohort) %>%
    rename(value_start = value)

  return(start_cohort)
}
