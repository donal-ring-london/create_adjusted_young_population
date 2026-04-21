## rougher script, just for qa
## app that gives age distributions for population and international flows
## filtered to just London
## also compares the adjusted estimates from 0-15 with data from the original myes post 15 tacked on, with the version where the whole population series is rebuilt using the new components, to examine the "step"


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

adjusted_estimates <- data.table(readRDS("data/processed/adjusted_mye_0_15_full.rds"))

adjusted_r_estimates <- data.table(readRDS("data/processed/adjusted_rebuilt_mye_backseries.rds"))

gp_data <- data.table(readRDS("data/intermediate/gp_sya_lad.rds"))

pupils <- data.table(readRDS("data/intermediate/total_pupils_lad_5_15_2015_2024.rds"))

pupils_11_19 <- fread("data/intermediate/resident_pupils_lea_ncy_2011_2019_from_cbm.csv")

  ### 1.1. sub task - combining the post and pre-2019 pupil datasets
pupils_11_19[, age := nc_year + 4]

colnames(pupils_11_19)[c(1, 3, 5, 6)] <- c("gss_code", "gss_name", "value", "year")
pupils_11_19 <- pupils_11_19[year <= 2018, c("gss_code", "year", "age", "value")] # undocumented/unclear which year in xxxx/yyyy was taken from full academic year, and code was hard to decipher, but I'm still fairly sure it's the first

# pupils <- rbind(pupils_11_19, pupils)

pupils <- pupils[order(year), ]

  ### 1.2. adding on 15 to 20 to the new estimates, narrowing the full estimates to only age 20 and below
adjusted_estimates <- mye_2011_24 %>% 
  filter(gss_code %in% adjusted_estimates$gss_code & year %in% 2015:2024 & age %in% 16:20) %>%
  rbind(adjusted_estimates) %>%
  arrange(gss_code, sex, age, year)

adjusted_r_estimates <- adjusted_r_estimates[age <= 20, ]


## 2. reformatting and cleaning up data etc

  ### 2.1. simple filtering and selecting and the like 
gss_lookup <- unique(mye_2011_24[, c("gss_code", "gss_name")])

adjusted_estimates <- gss_lookup[adjusted_estimates, on = "gss_code"]
adjusted_r_estimates <- gss_lookup[adjusted_r_estimates, on = "gss_code"]

  ### 2.2. narrowing down to London, aggregating over sex (easier that way), sorting out year in the GP data, getting them all to ages 0-14
mye_2011_24 <- mye_2011_24[grep("E09", gss_code, fixed = TRUE), ]
gp_data <- gp_data[grep("E09", gss_code, fixed = TRUE), ]
adjusted_estimates <- adjusted_estimates[grep("E09", gss_code, fixed = TRUE), ]
adjusted_r_estimates <- adjusted_r_estimates[grep("E09", gss_code, fixed = TRUE), ]
pupils <- pupils[grep("E09", gss_code, fixed = TRUE), ]

mye_2011_24 <- mye_2011_24[, .(value = sum(value)),
                            by = .(gss_code, gss_name, year, age, component)]

adjusted_estimates <- adjusted_estimates[, .(value = sum(value)),
                     by = .(gss_code, gss_name, year, age, component)]

adjusted_r_estimates <- adjusted_r_estimates[, .(value = sum(value)),
                                         by = .(gss_code, gss_name, year, age, component)]

gp_data <- gp_data[sex == "persons", ]
gp_data[, year := tstrsplit(extract_date, "-")[1]]
gp_data <- gp_data[grep("07", extract_date, fixed = TRUE), ] # narrowing down to mid year

mye_2011_24 <- mye_2011_24[age %in% 0:20, ]
gp_data <- gp_data[age %in% 0:20, ]
pupils <- pupils[age %in% 0:15, ]


## 3. creating the age distributions app

  ### 3.1. setting the selections and creating the ui
year_selections <- 2015:2024

lad_selections <- c(mye_2011_24[, unique(gss_code)], "London total")
help("plotOutput")
ui <- fluidPage(

  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),

  selectInput("year", "Choose year", year_selections),

  selectInput("lad", "Choose local authority", lad_selections),

  plotOutput("plot", height = "700px")

)

  ### 3.2. creating the server
server <- function(input, output, session){

  output$plot <- shiny::renderPlot({

    #### the inputs
    year_sel <- as.numeric(input$year)
    lad_sel <- input$lad

    #### filtering the datasets by the input, or aggregating if London total is selected
    if(lad_sel == "London total"){

      mye_lad <- mye_2011_24[, .(value = sum(value)),
                                by = list(year, age, component)]

      adj_lad <- adjusted_estimates[, .(value = sum(value)),
                                by = list(year, age, component)]
      
      adj_r_lad <- adjusted_r_estimates[, .(value = sum(value)),
                                    by = list(year, age, component)]

      } else {

      mye_lad <- mye_2011_24[gss_code == lad_sel, ]
      adj_lad <- adjusted_estimates[gss_code == lad_sel, ]
      adj_r_lad <- adjusted_r_estimates[gss_code == lad_sel, ]

    }

    mye_sel_pop <- mye_lad[year == year_sel & component == "population", ]
    adj_sel_pop <- adj_lad[year == year_sel & component == "population", ]
    adj_r_sel_pop <- adj_r_lad[year == year_sel & component == "population", ]
    
    mye_sel_int <- mye_lad[year == year_sel & component == "international_net", ]
    adj_sel_int <- adj_lad[year == year_sel & component == "international_net", ]
    adj_r_sel_int <- adj_r_lad[year == year_sel & component == "international_net", ]
    
    #### making the plots
    par(mfrow = c(2, 1))
    
    #### the population age distribution graph
    xmin <- 0
    xmax <- 20
    
    ymin <- min(mye_sel_pop[, value], adj_sel_pop[, value], adj_r_sel_pop[, value])
    ymax <- max(mye_sel_pop[, value], adj_sel_pop[, value], adj_r_sel_pop[, value])
    
    plot(x = 1, y = 1, type = "n", bty = "n", las = 1, ylab = "", xlab = "",
                xlim = c(xmin, xmax), ylim = c(ymin, ymax))
    
    lines(x = c(16, 16), y = c(ymin, ymax), col = "lightgrey", lty = 3, lwd = 2)
    
    lines(x = mye_sel_pop[, age], y = mye_sel_pop[, value], lwd = 2, col = "navy")
    lines(x = adj_sel_pop[, age], y = adj_sel_pop[, value], lwd = 2, col = "orange")
    lines(x = adj_r_sel_pop[, age], y = adj_r_sel_pop[, value], lwd = 2, col = "pink")
    
    lad_name <- gss_lookup[gss_code == lad_sel, gss_name]
    title(main = paste("Population age distribution for ", lad_name))

    #### the international flows age distribution graph
    xmin <- 0
    xmax <- 20

    ymin <- min(mye_sel_int[, value], adj_sel_int[, value])
    ymax <- max(mye_sel_int[, value], adj_sel_int[, value])

    plot(x = 1, y = 1, type = "n", bty = "n", las = 1, ylab = "", xlab = "",
                xlim = c(xmin, xmax), ylim = c(ymin, ymax))

    lines(x = mye_sel_int[, age], y = mye_sel_int[, value], lwd = 2, col = "navy")
    lines(x = adj_sel_int[, age], y = adj_sel_int[, value], lwd = 2, col = "orange")

    lad_name <- gss_lookup[gss_code == lad_sel, gss_name]
    title(main = paste("Net flows age distribution for ", lad_name))

     par(mfrow = c(1, 1))

    })

}

      ### 3.3 running the app
shinyApp(
  ui = ui,
  server = server
)


