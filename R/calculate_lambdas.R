#' Calculate univariable lambda
#'
#' Calculates lambda (regression dilution coefficient) using repeated measurements data. Data should be in wide format i.e. one row per participant with multiple columns for the different repeats. Different repeats should be indicated consistently by the same suffix.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#'
#' @param var_name Name of variable to calculate lambda for
#' @param suffix_baseline Suffix denoting a baseline measurement
#' @param suffix_rep Suffix(es) denoting a follow-up measurement
#' @param exclusion_indic Name of variable indicating row should be excluded for that repeat (should have same suffix as indicated in `suffix_baseline`, `suffix_rep`)
#' @param data Dataset containing repeated measurements data
#' @param covariates Covariates to adjust for in lambda estimation model
#' @param precision_weighted Indicator of whether precision weighting should be used in model for follow up based on number of repeats
#' @return List containing lambda estimate and its variance.
#' @export
calc_lambda <- function(var_name, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL, precision_weighted = FALSE){

  # Checks of inputs
  var_name_base <- paste0(var_name, suffix_baseline)
  var_names_rep <- paste0(var_name, suffix_rep)

  for (var_loc in c(var_name_base, var_names_rep)){
    if (!(var_loc %in% colnames(data))){
      stop(paste0(var_loc, " does not appear in data. Is the data set up in the required format?"))
    }
  }

  # Mean of relevant repeats
  data$mean_reps_w_data <- calc_mean_reps(var_name = var_name, data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)

  # Precision weights
  precision_weights <- calc_precision_weights(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic, precision_weighted = precision_weighted)

  # Set up linear model
  covs <- paste0(c(var_name_base, covariates), collapse = "+")
  form <- as.formula(paste0("mean_reps_w_data ~", covs))
  lambda_lin <- lm(form, data, weights = precision_weights)

  # Extract lambda and var
  lambda <- coef(lambda_lin)[[var_name_base]]
  var_lambda <- (summary(lambda_lin)$coef[var_name_base, "Std. Error"])^2

  # Return variables
  return(list("Lambda Estimate" = lambda, "Variance" = var_lambda))
  }
