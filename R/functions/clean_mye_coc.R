library(dplyr)
library(tidyr)
library(stringr)

clean_mye_coc <- function(raw_df) {

  mye_coc <- raw_df %>%
    rename(any_of(c(gss_code = "ladcode21",
                    gss_code = "ladcode23",
                    gss_code = "ladcode24",
                    gss_code = "ladcode18",
                    gss_name = "laname21",
                    gss_name = "laname23",
                    gss_name = "laname24",
                    gss_name = "laname18",
                    country = "Country")
                  )
           ) %>%
    mutate(sex = recode(sex,
                        "M" = "male",
                        "F" = "female",
                        "m" = "male",
                        "f" = "female",
                        "1" = "male",
                        "2" = "female")) %>%
    select(-country) %>%
    pivot_longer(cols = -any_of(c("gss_code", "gss_name", "age", "sex", "country")),
                 names_to = "component_year",
                 values_to = "value"
    ) %>%
    mutate(year = str_sub(component_year, -4, -1)) %>%
    mutate(component = str_sub(component_year, 1, -6)) %>%
    select(-component_year) %>%
    mutate(year = as.numeric(year)) %>%
    replace_na(list(value = 0))


  return(mye_coc)
}
