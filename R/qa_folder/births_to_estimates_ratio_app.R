## rougher script, just for qa


## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gsscoder)
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
adjusted_estimates <- adjusted_estimates[component == "population", ]

adjusted_r_estimates <- data.table(readRDS("data/processed/adjusted_rebuilt_mye_backseries.rds"))
adjusted_r_estimates <- adjusted_r_estimates[component == "population", ]

gp_data <- data.table(readRDS("data/intermediate/gp_sya_lad.rds"))

births <- fread("https://data.london.gov.uk/download/2w43n/9698d0b1-663c-4594-8687-67469ce07e6d/actual_and_predicted_births.csv")

pupils <- data.table(readRDS("data/intermediate/total_pupils_lad_5_15_2015_2024.rds"))

pupils_11_19 <- fread("data/intermediate/resident_pupils_lea_ncy_2011_2019_from_cbm.csv")

  ### 1.1. sub task - combining the post and pre-2019 pupil datasets
pupils_11_19[, age := nc_year + 4]

colnames(pupils_11_19)[c(1, 3, 5, 6)] <- c("gss_code", "gss_name", "value", "year")
pupils_11_19 <- pupils_11_19[year <= 2018, c("gss_code", "year", "age", "value")] 

#pupils <- rbind(pupils_11_19, pupils)

pupils <- pupils[order(year), ]


## 2. reformatting and cleaning up data etc

  ### 2.1. simple filtering and selecting and the like 
mye_2011_24 <- mye_2011_24[component == "population", ]

births <- births[type == "actual", ]
births <- births[, c("gss_code", "gss_name", "sex", "date", "annual_births")]

gss_lookup <- unique(mye_2011_24[, c("gss_code", "gss_name")])

adjusted_estimates <- gss_lookup[adjusted_estimates, on = "gss_code"]
adjusted_r_estimates <- gss_lookup[adjusted_r_estimates, on = "gss_code"]

  ### 2.2. narrowing down to London, aggregating over sex (easier that way), sorting out year in the GP data
mye_2011_24 <- mye_2011_24[grep("E09", gss_code, fixed = TRUE), ]
gp_data <- gp_data[grep("E09", gss_code, fixed = TRUE), ]
births <- births[grep("E09", gss_code, fixed = TRUE), ]
adjusted_estimates <- adjusted_estimates[grep("E09", gss_code, fixed = TRUE), ]
adjusted_r_estimates <- adjusted_r_estimates[grep("E09", gss_code, fixed = TRUE), ]
pupils <- pupils[grep("E09", gss_code, fixed = TRUE), ]

mye_2011_24 <- mye_2011_24[, .(value = sum(value)),
                            by = .(gss_code, gss_name, year, age)]

adjusted_estimates <- adjusted_estimates[, .(value = sum(value)),
                     by = .(gss_code, gss_name, year, age)]

adjusted_r_estimates <- adjusted_r_estimates[, .(value = sum(value)),
                                            by = .(gss_code, gss_name, year, age)]

gp_data <- gp_data[sex == "persons", ]
gp_data[, year := tstrsplit(extract_date, "-")[1]]
gp_data <- gp_data[grep("07", extract_date, fixed = TRUE), ] # narrowing down to mid year

births <- births[sex == "persons", ]
births[, year := tstrsplit(date, "-")[1]]
births <- births[grep("07", date, fixed = TRUE), ]
# births <- births[year >= 2011, ]
colnames(births)[colnames(births) == "annual_births"] <- "value"


## 3. shiny app showing the ratio of births to population, for the various different sources of population estimates

      ### 3.1. setting the selections and creating the UI
age_selections <- 0:18

lad_selections <- c(mye_2011_24[, unique(gss_code)], "London total")

ui <- fluidPage(

  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),

  selectInput("age", "Choose age", age_selections),

  selectInput("lad", "Choose local authority", lad_selections),

  plotOutput("plot", height = "650px")

)

      ### 3.2. creating the server
server <- function(input, output, session){

  output$plot <- shiny::renderPlot({

    #### the inputs
    age_sel <- as.numeric(input$age)
    lad_sel <- input$lad

    #### filtering the datasets by the input, or aggregating if London total is selected
    if(lad_sel == "London total"){

      births_rat <- births[, .(births = sum(value)),
                           by = .(year)]

      births_rat[, year_lag := as.numeric(year) + age_sel]
      births_rat <- births_rat[, c("year_lag", "births")]

      mye_ons_rat <- mye_2011_24[age == age_sel, .(mye_ons_pop = sum(value)),
                          by = list(year)]

      adj_rat <- adjusted_estimates[age == age_sel, .(adj_pop = sum(value)),
                              by = list(year)]
      
      adj_r_rat <- adjusted_r_estimates[age == age_sel, .(adj_r_pop = sum(value)),
                                        by = list(year)]

      gp_rat <- gp_data[age == age_sel, .(gp_pop = sum(value)),
                          by = list(year)]

      gp_rat[, year := as.numeric(year)]

      if(age_sel >= 4){
        
      pupils_rat <- pupils[age == age_sel, .(pupil_pop = sum(value)),
                          by = list(year)]
      }

      } else {

      births_rat <- births[gss_code == lad_sel, ]
      births_rat[, year_lag := as.numeric(year) + age_sel]
      colnames(births_rat)[colnames(births_rat) == "value"] <- "births"
      births_rat <- births_rat[, c("year_lag", "births")]

      mye_ons_rat <- mye_2011_24[age == age_sel & gss_code == lad_sel]
      colnames(mye_ons_rat)[colnames(mye_ons_rat) == "value"] <- "mye_ons_pop"
      mye_ons_rat <- mye_ons_rat[, c("year", "mye_ons_pop")]

      adj_rat <- adjusted_estimates[age == age_sel & gss_code == lad_sel]
      colnames(adj_rat)[colnames(adj_rat) == "value"] <- "adj_pop"
      adj_rat <- adj_rat[, c("year", "adj_pop")]
      
      adj_r_rat <- adjusted_r_estimates[age == age_sel & gss_code == lad_sel]
      colnames(adj_r_rat)[colnames(adj_r_rat) == "value"] <- "adj_r_pop"
      adj_r_rat <- adj_r_rat[, c("year", "adj_r_pop")]

      gp_rat <- gp_data[age == age_sel & gss_code == lad_sel]
      colnames(gp_rat)[colnames(gp_rat) == "value"] <- "gp_pop"
      gp_rat <- gp_rat[, c("year", "gp_pop")]
      gp_rat[, year := as.numeric(year)]

      if(age_sel >= 4){
        
      pupils_rat <- pupils[age == age_sel & gss_code == lad_sel]
      colnames(pupils_rat)[colnames(pupils_rat) == "value"] <- "pupil_pop"
      pupils_rat <- pupils_rat[, c("year", "pupil_pop")]
      
      }

    }

    #### creating the ratios
    setkey(births_rat, "year_lag")
    setkey(gp_rat, "year")
    setkey(adj_rat, "year")
    setkey(adj_r_rat, "year")
    setkey(mye_ons_rat, "year")

    if(age_sel >= 4){
    setkey(pupils_rat, "year")
    }

    ratios <- births_rat[mye_ons_rat]
    ratios <- gp_rat[ratios]
    ratios <- mye_ons_rat[ratios]
    ratios <- adj_rat[ratios]
    ratios <- adj_r_rat[ratios]
    if(age_sel >= 4){
    ratios <- pupils_rat[ratios]
    }

    ratios[, births_gp_ratio := gp_pop/births]
    ratios[, births_mye_ons_ratio := mye_ons_pop/births]
    ratios[, births_adj_ratio := adj_pop/births]
    ratios[, births_adj_r_ratio := adj_r_pop/births]
    if(age_sel >= 4){
    ratios[, births_pupil_ratio := pupil_pop/births]
    }

      #### creating a graph to show the ratios
    
    if(age_sel >= 4){
      ymin <- min(ratios[, c("births_gp_ratio", "births_mye_ons_ratio", "births_adj_ratio", "births_adj_r_ratio", "births_pupil_ratio")], na.rm = TRUE)
      ymax <- max(ratios[, c("births_gp_ratio", "births_mye_ons_ratio", "births_adj_ratio", "births_adj_r_ratio", "births_pupil_ratio")], na.rm = TRUE)
    }else{
      ymin <- min(ratios[, c("births_gp_ratio", "births_mye_ons_ratio", "births_adj_ratio", "births_adj_r_ratio")], na.rm = TRUE)
      ymax <- max(ratios[, c("births_gp_ratio", "births_mye_ons_ratio", "births_adj_ratio", "births_adj_r_ratio")], na.rm = TRUE)
    }

      plot(x = 1, y = 1, type = "n", bty = "n", las = 1, ylab = "", xlab = "",
            xlim = c(2015, 2025), ylim = c(ymin, ymax))

      lines(x = c(2021, 2021), y = c(ymin, ymax), col = "grey", lty = 3, lwd = 3)

      lines(x = ratios[, year], y = ratios[, births_gp_ratio],
            lwd = 2, col = "darkgreen")

      lines(x = ratios[, year], y = ratios[, births_mye_ons_ratio],
            lwd = 2, col = "navy")

      lines(x = ratios[, year], y = ratios[, births_adj_ratio],
            lwd = 2, col = "pink")
      
      lines(x = ratios[, year], y = ratios[, births_adj_r_ratio],
            lwd = 2, col = "darkred")

    if(age_sel >= 4){
      lines(x = ratios[, year], y = ratios[, births_pupil_ratio],
            lwd = 2, col = "darkgrey")
    }

      if(lad_sel == "London total"){

        title(main = paste0("London total, age ", age_sel))

      }else{

        lad_name <- mye_2011_24[gss_code == lad_sel, unique(gss_name)]
        title(main = paste0(lad_name, ", age ", age_sel))

      }

    if(age_sel >= 4){
    legend("bottomleft", legend = c("births:gp", "births:mye", "births:adjusted", "births:adjusted_r", "births:pupils"), bty = "n",
            lwd = c(2, 2, 2, 2), col = c("darkgreen", "navy", "pink", "darkred", "darkgrey"))
    }else{
    legend("bottomleft", legend = c("births:gp", "births:mye", "births:adjusted", "births_adjusted_r"), bty = "n",
            lwd = c(2, 2, 2, 2), col = c("darkgreen", "navy", "pink", "darkred"))
    }
      
    })

}

      ### 3.3 running the app
shinyApp(
  ui = ui,
  server = server
)

