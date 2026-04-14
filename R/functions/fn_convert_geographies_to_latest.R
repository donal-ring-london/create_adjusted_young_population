# this function works mostly the same as my other functions to aggregate geographies, such as aggregate_geographies_2 (which is in the same folder). 
# the difference with this function is that it is not designed simply for mapping lower geographies to higher geographies.
# it is designed for dealing with splits and mergings of geographies over time, which need to be mapped to a common geography and then aggregated to that common geography. 
# the first major input is a dataset, with multiple time periods, with geographies to be corrected. The second major input is a lookup, that contains only the geographies to be corrected, not all geographies (although it can contain all geographies and will still work just fine).

convert_geographies_to_latest <- function(data, lookup,
                                    geog_from_data, geog_from_lookup,
                                    geog_to_lookup, count_names){
  
  ## making sure everything is in a data.table
  data <- data.table(data)
  lookup <- data.table(lookup)
  
  ## in the lookup, only keeping geography columns that we're using
  look_keep <- c(geog_from_lookup, geog_to_lookup)
  lookup <- lookup[,..look_keep]
  
  
  ## joining the dataset with the geographical lookup
  setkeyv(data, geog_from_data)
  
  setkeyv(lookup, geog_from_lookup)
  
  data <- lookup[data]
  
  data[is.na(get(geog_to_lookup)), (geog_to_lookup) := get(geog_from_lookup)] # for any NA values - geographies that aren't included in the lookup - they are set to the values in the original dataset
  
  data <- data[, -..geog_from_lookup]
  
  ## the final aggregation, from lower geographies to higher geographies
  to_rem <- c(count_names) # name of the count variable(s)
  
  by_cols <- colnames(data)[!(colnames(data) %in% to_rem)] # getting a vector of columns to aggregate over. This is just all of the columns other than the count column(s). 
  
  data <- data[,lapply(.SD, sum, na.rm = TRUE), # applying sum function to each column specified in SD cols
               .SDcols = c(count_names), # setting SDcols. In this case, only the count column(s)
               by = by_cols] # the columns that we want to aggregate over/by. 
  
  return(data)
  
}


