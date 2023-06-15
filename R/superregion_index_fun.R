#' Apply subcontinent indexing to data
#' @param my_data National level family planning source data
#' @param n_region A vector of subcontinents listed in the dataset
#' @return Dataframe with subconintental indexing applied
#' @example
#' national_FPsource_data <- mcmsupply::national_FPsource_data
#' n_subcont <- unique(national_FPsource_data$Super_region)
#' superregion_index_fun(national_FPsource_data, n_subcont)

superregion_index_fun <- function(my_data, n_region) {
  my_data$index_superregion <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Super_region[j]==region_name) {
        my_data$index_superregion[j] <- i
      }
      else {
        next
      }
    }
  }
  return(my_data)
}
