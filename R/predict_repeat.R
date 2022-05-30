#' Predicts true val using reg cal framework
#'
#' @param models_list Names list of models (probably output of calc_models_multi)
#' @param new_data Dataset to predict true values for
#' @param suffix_pred Suffix to append to name of predicted variable
#' @return List of models for different outcomes
#' @export
pred_rep <- function(models_list, newdata, suffix_pred = "_pred"){
  var_names <- names(models_list)
  dat <- newdata
  for (var_name in var_names){
    model <- models_list[[var_name]][[1]]
    covariates <- models_list[[var_name]][[2]]
    X <- model.matrix(~., newdata[, covariates])
    dat[, paste0(var_name, suffix_pred)] <- X %*% model[["coefficients"]]
  }

  return(dat)
}
