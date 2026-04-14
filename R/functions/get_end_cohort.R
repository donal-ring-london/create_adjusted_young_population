library(dplyr)

#takes the mye with components of change and returns the population by birth cohort at yr_end
get_end_cohort <- function(mye_coc, yr_end) {

  end_cohort <- mye_coc %>%
    filter(year == yr_end) %>%
    filter(component == "population") %>%
    mutate(cohort = year - age) %>%
    select(gss_code, gss_name, sex, cohort, value_end = value) %>%
    arrange(gss_code, sex, cohort)

  return(end_cohort)
}
