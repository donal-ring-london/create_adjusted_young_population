source("R/functions/clean_mye_coc.R")

fetch_and_clean_mye_data <- function(url_raw,
                                     fpath_raw,
                                     fpath_clean,
                                     sheet_name) {

  if(!dir.exists("data/raw/")) dir.create("data/raw/", recursive = TRUE)
  if(!dir.exists("data/intermediate/")) dir.create("data/intermediate/", recursive = TRUE)


  download.file(url = url_raw,
                destfile = fpath_raw,
                mode = "wb")

  mye_coc <- clean_mye_coc(read_xlsx(fpath_raw, sheet = sheet_name, skip = 1))

  saveRDS(mye_coc, file = fpath_clean)

}
