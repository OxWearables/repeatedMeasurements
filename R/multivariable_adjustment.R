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
#' @param precision_weighted Indicator of whether precision weighting should be used in model for follow up based on number of repeats
#' @return List of models for different outcomes
#' @export
calc_models_multi <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL, precision_weighted = FALSE){

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

  # Precision weights for outcome
  precision_weights <- calc_precision_weights(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic, precision_weighted = precision_weighted)


  for (var_name in var_names){
    # Mean of relevant repeat is outcome
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)

    y <- data[, paste0("mean_reps_w_data", var_name)]

    # Calc model
    if (is.null(precision_weights)){
      lambda_lin <- lm.fit(X, y)
    }
    else {
      lambda_lin <- lm.wfit(X, y, w = precision_weights)
    }

    # Append model to list and remove
    models_list[[var_name]] <- list(lambda_lin, cols)
    rm(lambda_lin)
  }

  # Return models
  return(models_list)
  }



#' Calculates linear models for multivariable adjustment (direct multivariate regression calibration model)
#'
#' Calculates multivariate linear model for multivariable adjustment using repeated measurements data.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#' NOTE: This is essentially redundant with `calc_models_multi`, but it's directly creating multivariate model rather than creating each univariate model (coefficients within each model should be same, though)
#'
#' @inheritParams calc_models_multi
#' @return Multivariate model
#' @export
calc_models_multi_single <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL, precision_weighted = FALSE){

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
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)
  }

  # Precision weights for outcome
  precision_weights <- calc_precision_weights(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic, precision_weighted = precision_weighted)


  # Parts of model
  cols <- c(covariates, var_names_base)
  covs <- paste0(cols, collapse = "+")
  outcomes <- paste0("mean_reps_w_data", var_names)

  # Calc model
  form <- as.formula(paste0("cbind(", paste0(outcomes, collapse = ", "), ")~", covs))
  lambda_lin <- lm(form, data, weights = precision_weights)

  # Return models
  return(lambda_lin)
}






#' Calculates linear models for multivariable adjustment (slow check)
#'
#' Calculates linear models for multivariable adjustment using repeated measurements data. Slower version of calc_models_multi, implemented as a check.
#'
#' NOTE: uses mean value across all follow-up measurements with data
#'
#' @inheritParams calc_models_multi_slow
#' @return List of models for different outcomes
#' @export
calc_models_multi_slow <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL, precision_weighted = FALSE){

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

  # Parts of models
  cols <- c(covariates, var_names_base)
  covs <- paste0(cols, collapse = "+")

  # Precision weights for outcome
  precision_weights <- calc_precision_weights(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic, precision_weighted = precision_weighted)


  for (var_name in var_names){
    # Mean of relevant repeat is outcome
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)

    # Calc model
    form <- as.formula(paste0("mean_reps_w_data", var_name, "~", covs))
    lamdba_lin <- lm(form, data, weights = precision_weights)

    # Append model to list and remove
    models_list[[var_name]] <- list(lambda_lin, cols)
    rm(lambda_lin)
  }

  # Return models
  return(models_list)
}
