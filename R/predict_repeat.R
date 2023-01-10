#' Predicts true value using regression calibration framework
#'
#' @param models_list Names list of models (probably output of calc_models_multi)
#' @param new_data Dataset to predict true values for
#' @param suffix_pred Suffix to append to name of predicted variable
#' @return List of models for different outcomes
#' @export
pred_rep <- function(models_list, newdata, suffix_pred = "_pred"){
  # Set up
  var_names <- names(models_list)
  dat <- newdata

  for (var_name in var_names){

    # Grab relevant models
    model <- models_list[[var_name]][[1]]
    covariates <- models_list[[var_name]][[2]]
    X <- model.matrix(~., dat[, covariates])

    # Check there has been no relevelling of factor variables in data frame, which could mess up these matrices:
    if (!isTRUE(all.equal(colnames(X), names(model[["coefficients"]])))){
      stop("Model matrix and coefficients do not match. Have you relevelled factors in the data frame?")
    }


    # Do calculation
    dat[, paste0(var_name, suffix_pred)] <- X %*% model[["coefficients"]]
  }

  return(dat)
}


#' Predicts true value using regression calibration framework (slow version for check)
#'
#' @param models_list Names list of models (probably output of calc_models_multi_slow)
#' @param new_data Dataset to predict true values for
#' @param suffix_pred Suffix to append to name of predicted variable
#' @return List of models for different outcomes
#' @export
pred_rep_slow <- function(models_list, newdata, suffix_pred = "_pred"){
  # Set up
  var_names <- names(models_list)
  dat <- newdata

  for (var_name in var_names){

    # Grab relevant models
    model <- models_list[[var_name]]

    # Do calculation
    dat[, paste0(var_name, suffix_pred)] <- predict(model, newdata = dat)
  }

  return(dat)
}

