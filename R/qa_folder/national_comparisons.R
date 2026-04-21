
## national population comparisons

## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(readxl)
library(data.table)
library(shiny)

functions_to_read <- list.files("R/functions")

lapply(
  paste0("R/functions/", functions_to_read),
  FUN = source
)


## 1. reading in data
mye_2011_24 <- data.table(readRDS("data/intermediate/mye_2011_on(2023_geog).rds"))

adjusted_estimates <- data.table(readRDS("data/processed/adjusted_rebuilt_mye_backseries.rds"))

mye_2011_24 <- mye_2011_24[!grep("W", gss_code), ]
adjusted_estimates <- adjusted_estimates[!grep("W", gss_code), ]

length(unique(mye_2011_24$gss_code))

length(unique(adjusted_estimates$gss_code))


## 2. putting together a table with a side-by-side comparison of the two datasets

mye_og_ests <- mye_2011_24[component == "population" & year >=2019, .(mye_og_population = sum(value)), 
                                                                    by = list(year, age)]

adj_ests <- adjusted_estimates[component == "population" & year >= 2019, .(adjusted_population = sum(value)), 
                                                      by = list(year, age)]

pop_ests_comparisons <- mye_og_ests[adj_ests, on = c("year", "age")]

pop_ests_comparisons[, difference := adjusted_population - mye_og_population]
pop_ests_comparisons[, percentage_difference := round(100*(difference/mye_og_population), 2)]

pop_ests_comparisons[age %in% 0:7, ]


## 3. making some static plots of the national population comparisons in the table above

par(mar = c(2.5, 4, 1, 1))
par(mfrow = c(3, 5))

for(age_sel in 0:14){

  dat_age <- pop_ests_comparisons[age == age_sel, ]

  ymin <- min(dat_age[, c("mye_og_population", "adjusted_population")])
  ymax <- max(dat_age[, c("mye_og_population", "adjusted_population")])

  plot(x = 1, y = 1, type = "n", bty = "n", las = 1, ylab = "", xlab = "",
                 xlim = c(2019, 2025), ylim = c(ymin, ymax))

  lines(x = dat_age[, year], y = dat_age[, mye_og_population], lwd = 2, col = "navy")
  lines(x = dat_age[, year], y = dat_age[, adjusted_population], lwd = 2, col = "orange")

  title(main = paste0("Age ", age_sel))

}

par(mfrow = c(1, 1))



