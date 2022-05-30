#' Calculates linear models for multivariable adjustment
#'
#' Calculates linear models for multivariable adjustment using repeated measurements data.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#'
#' @param var_names Name of variables to calculate models for
#' @param suffix_baseline Suffix denoting a baseline measurement
#' @param suffix_rep Suffix(es) denoting a follow-up measurement
#' @param exclusion_indic Name of variable indicating row should be excluded for that repeat
#' @param data Dataset containing repeated measurements data
#' @param covariates Covariates to adjust for in lambda estimation model
#' @return List of models for different outcomes
#' @export
calc_models_multi <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL){

  # Checks of inputs
  var_names_base <- paste0(var_names,  suffix_baseline)
  var_names_rep <- paste0(var_names, suffix_rep)

  for (var_loc in c(var_names_base, var_names_rep)){
    if (!(var_loc %in% colnames(data))){
      stop(paste0(var_loc, " does not appear in data. Is the data set up in the required format?"))
    }
  }

  # Prep list of models
  models_list <- vector("list", length(var_names))
  names(models_list) <- var_names

  # X
  cols <- c(covariates, var_names_base)
  X <- model.matrix(~ ., data = data[, cols])


  for (var_name in var_names){
    # Mean of relevant repeat is outcome
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_baseline = suffix_baseline, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)
    y <- data[, paste0("mean_reps_w_data", var_name)]

    # Calc model
    lambda_lin <- lm.fit(X, y)

    # Append model to list and remove
    models_list[[var_name]] <- list(lambda_lin, cols)
    rm(lambda_lin)
  }

  # Return models
  return(models_list)
  }



#' Calculates linear models for multivariable adjustment
#'
#' Calculates linear models for multivariable adjustment using repeated measurements data.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#' NOTE: I think this is essentially redundant with
#'
#' @param var_names Name of variables to calculate models for
#' @param suffix_baseline Suffix denoting a baseline measurement
#' @param suffix_rep Suffix(es) denoting a follow-up measurement
#' @param exclusion_indic Name of variable indicating row should be excluded for that repeat
#' @param data Dataset containing repeated measurements data
#' @param covariates Covariates to adjust for in lambda estimation model
#' @return Multivariable model
#' @export
calc_models_multi_single <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL){

  # Checks of inputs
  var_names_base <- paste0(var_names,  suffix_baseline)
  var_names_rep <- paste0(var_names, suffix_rep)

  for (var_loc in c(var_names_base, var_names_rep)){
    if (!(var_loc %in% colnames(data))){
      stop(paste0(var_loc, " does not appear in data. Is the data set up in the required format?"))
    }
  }

  for (var_name in var_names){
    # Mean of relevant repeat
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_baseline = suffix_baseline, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)
  }

  # Parts of model
  cols <- c(covariates, var_names_base)
  covs <- paste0(cols, collapse = "+")
  outcomes <- paste0("mean_reps_w_data", var_names)

  # Calc model
  form <- as.formula(paste0("cbind(", paste0(outcomes, collapse = ", "), ")~", covs))
  lambda_lin <- lm(form, data)

  # Return models
  return(lambda_lin)
}
