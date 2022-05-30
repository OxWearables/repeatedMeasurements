#' Calculate univariable lambda
#'
#' Calculates lambda (regression dilution coefficient) using repeated measurements data.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#'
#' @param var_name Name of variable to calculate lambda for
#' @param suffix_baseline Suffix denoting a baseline measurement
#' @param suffix_rep Suffix(es) denoting a follow-up measurement
#' @param exclusion_indic Name of variable indicating row should be excluded for that repeat
#' @param data Dataset containing repeated measurements data
#' @param covariates Covariates to adjust for in lambda estimation model
#' @return List containing lambda estimate and its variance.
#' @export
calc_lambda <- function(var_name, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL){

  # Checks of inputs
  var_name_base <- paste0(var_name,  suffix_baseline)
  var_names_rep <- paste0(var_name, suffix_rep)

  for (var_loc in c(var_name_base, var_names_rep)){
    if (!(var_loc %in% colnames(data))){
      stop(paste0(var_loc, " does not appear in data. Is the data set up in the required format?"))
    }
  }

  # Mean of relevant repeats
  data$mean_reps_w_data <- calc_mean_reps(var_name = var_name, data = data, suffix_baseline = suffix_baseline, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)

  # Set up linear model
  covs <- paste0(c(var_name_base, covariates), collapse = "+")
  form <- as.formula(paste0("mean_reps_w_data ~", covs))
  lambda_lin <- lm(form, data)

  # Extract lambda and var
  lambda <- coef(lambda_lin)[[var_name_base]]
  var_lambda <- (summary(lambda_lin)$coef[var_name_base, "Std. Error"])^2

  # Return variables
  return(list("Lambda Estimate" = lambda, "Variance" = var_lambda))
  }
