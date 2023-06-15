#' Get median, 95% and 80% credible intervals for posterior samples of P from JAGS model
#' @name get_subnational_global_P_estimates
#' @param main_path String. Path where you have set your model results to be saved to
#' @param P_samp String. Options are: "P_public.RDS","P_CM.RDS" and "P_other.RDS"
#' @param subnat_index_table Dataframe with subnational district indexing applied. Used to match estimates to data.
#' @param method_index_table Dataframe with method indexing applied. Used to match estimates to data.
#' @param sector_type String. Name of sector you are interested in. One of either ("Public", "Commercial medical", "Other")
#' @param year_index_table Dataframe with time indexing applied. Used to match estimates to data.
#' @return Dataframe of labelled posterior samples with median, 95% and 80% credible intervals estimates.
#' @examples
#' cleaned_data <- get_data(national=FALSE, local=TRUE, mycountry="Nepal", fp2030=TRUE, surveydata_filepath=NULL)
#' pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_data)
#' mydata <- pkg_data$data
#' country_index_table <- tibble(Country = pkg_data$n_country, index_country = unique(mydata$index_country))
#' method_index_table <- tibble(Method = pkg_data$n_method, index_method = 1:5)
#' sector_index_table <- tibble(Sector = c("Public", "Commercial_medical", "Other"), index_sector = 1:3)
#' year_index_table <- tibble(average_year = pkg_data$all_years, index_year = 1:pkg_data$n_years, floored_year = floor(pkg_data$all_years))
#' get_subnational_global_P_estimates(subnat_index_table, method_index_table, sector_type, year_index_table, "P_public.RDS")
#' @export

# Calculation for sector data
get_subnational_global_P_estimates <- function(main_path, P_samp, subnat_index_table, method_index_table, sector_type, year_index_table) { # Median alpha values

  P_samps <- readRDS(paste0(main_path,P_samp))

  time_index <- year_index_table %>% # match index years to pooled years (pool 6 monthly estimates)
    dplyr::filter(average_year>floored_year) %>%
    dplyr::select(index_year) %>%
    unlist() %>%
    as.vector()

  averageyear_index_table <- year_index_table %>%
    dplyr::filter(average_year>floored_year) %>%
    dplyr::select(average_year, index_year) %>%
    dplyr::mutate(index_year = 1:n())

  P_dims <- dim(P_samps)

  #### P median
  P_s_med <- array(dim=c(length(time_index),P_dims[2],P_dims[3])) # method, year, subnat

  # Create a table for storing individual true country public data
  for(k in 1:length(time_index)) { # time loop
    for(s in 1:P_dims[3]) { # subnat
      for (m in 1:P_dims[2]) { # method
        P_s_med[k,m,s] <- stats::median(P_samps[,m,s,time_index[k]])
      }
    }
  }

  P_s_med <- plyr::adply(P_s_med, c(1,2,3))
  colnames(P_s_med) <- c("index_year", "index_method", "index_subnat", "median_p")

  P_s_med <- P_s_med %>%
    dplyr::mutate(index_year = as.numeric(index_year)) %>%
    dplyr::mutate(index_method = as.numeric(index_method)) %>%
    dplyr::mutate(index_subnat = as.numeric(index_subnat)) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(averageyear_index_table) %>%
    dplyr::mutate(Sector = sector_type)

  #### P median
  P_s_quant <- array(dim=c(length(time_index),P_dims[2],P_dims[3],4)) # method, year, subnat, quantile(95, 80)

  # Create a table for storing individual true country public data
  for(k in 1:length(time_index)) { # time loop
    for(s in 1:P_dims[3]) { # subnat
      for (m in 1:P_dims[2]) { # method
        P_s_quant[k,m,s,1:2] <- as.vector(unlist(stats::quantile(P_samps[,m,s,time_index[k]], prob=c(0.025, 0.975))))
        P_s_quant[k,m,s,3:4] <- as.vector(unlist(stats::quantile(P_samps[,m,s,time_index[k]], prob=c(0.1, 0.9))))
      }
    }
  }

  P_s_quant <- plyr::adply(P_s_quant, c(1,2,3))
  colnames(P_s_quant) <- c("index_year", "index_method", "index_subnat", "lower_95", "upper_95", "lower_80", "upper_80")

  P_s_quant <- P_s_quant %>%
    dplyr::mutate(index_year = as.numeric(index_year)) %>%
    dplyr::mutate(index_method = as.numeric(index_method)) %>%
    dplyr::mutate(index_subnat = as.numeric(index_subnat)) %>%
    dplyr::left_join(subnat_index_table) %>%
    dplyr::left_join(method_index_table) %>%
    dplyr::left_join(averageyear_index_table) %>%
    dplyr::mutate(Sector = sector_type)

  P_med_quantile <- dplyr::left_join(P_s_med, P_s_quant)
  return(P_med_quantile)
}
