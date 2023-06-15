#' Combines the data sources to create one JAGS input list
#' @name get_subnational_JAGSinput_list
#' @param pkg_data The data list from the 'mcmsupply::get_national_modelinputs' function.
#' @param local TRUE/FALSE. Default is FALSE for global runs. Decides if this is a single-country or global run.
#' @param mycountry The name of country of interest. Default is NULL. For the names of potential countries, review vignette.
#' @return returns a list ready for input into the JAGS model
#' @example
#' Single-country:
#' cleaned_natdata <- get_data(national=FALSE, local=TRUE, mycountry="Nepal", fp2030=TRUE, surveydata_filepath=NULL)
#' pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
#' get_subnational_JAGSinput_list(pkg_data, local= TRUE, mycountry="Nepal")
#'
#' Multi-country:
#' cleaned_natdata <- get_data(national=FALSE, local=FALSE, mycountry=NULL, fp2030=TRUE, surveydata_filepath=NULL)
#' pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12, raw_data = cleaned_natdata)
#' get_subnational_JAGSinput_list(pkg_data, local= FALSE, mycountry=NULL)
#' @export

get_subnational_JAGSinput_list <- function(pkg_data, local= FALSE, mycountry=NULL) {
  if(local==TRUE & is.null(mycountry)==FALSE) {
    local_params <- get_subnational_local_parameters(mycountry = mycountry) # local informative prior parameters
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                        se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                        alpha_cms_hat = local_params$alpha_cms,
                        tau_alpha_pms_hat = local_params$tau_alphapms,
                        natRmat = local_params$natRmat,
                        natdf = length(pkg_data$n_method)+1,
                        kstar = pkg_data$kstar,
                        B.ik = pkg_data$B.ik,
                        n_years = pkg_data$n_years,
                        n_obs = pkg_data$n_obs,
                        K = pkg_data$K,
                        H = pkg_data$H,
                        P_zeroes = rep(0, length(pkg_data$n_subnat)),
                        P_count = pkg_data$P_count,
                        M_count = pkg_data$M_count,
                        matchsubnat = pkg_data$matchsubnat,
                        matchmethod = pkg_data$matchmethod,
                        matchyears = pkg_data$matchyears
      )
  } else {
    estimated_global_subnational_correlations <- mcmsupply::estimated_global_subnational_correlations  # load global subnational correlations
    estimated_rho_matrix <- estimated_global_subnational_correlations %>%
      dplyr::select(row, column, public_cor, private_cor)
    my_SE_rho_matrix <- estimated_rho_matrix %>%
        dplyr::select(public_cor, private_cor)
    jags_data <- list(y = pkg_data$data[,c("Public", "Commercial_medical")], # create JAGS list
                        se_prop = pkg_data$data[,c("Public.SE", "Commercial_medical.SE")],
                        rho = my_SE_rho_matrix,
                        kstar = pkg_data$kstar,
                        B.ik = pkg_data$B.ik,
                        n_years = pkg_data$n_years,
                        n_obs = pkg_data$n_obs,
                        K = pkg_data$K,
                        H = pkg_data$H,
                        C_count = pkg_data$C_count,
                        P_count = pkg_data$P_count,
                        R_count = pkg_data$R_count,
                        M_count = pkg_data$M_count,
                        matchsubnat = pkg_data$matchsubnat,
                        matchcountry = pkg_data$matchcountry,
                        matchregion = pkg_data$matchsuperregion,
                        matchmethod = pkg_data$matchmethod,
                        matchyears = pkg_data$matchyears)
  }
  return(jags_data)
}
