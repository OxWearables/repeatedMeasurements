#' Adjust model coefficients for regression dilution
#'
#' Adjusts a beta coefficient for regression dilution using a calculated value of lambda.
#'
#' XXX THIS IS ONLY FOR COX MODELS AT THE MOMENT
#'
#' @param models Models
#' @param coef_name Name of coefficient to adjust
#' @param lambdas Values of regression dilution coefficient
#' @param var_lambdas Variance of regression dilution coefficient estimates
#' @param hr_scale If using a Cox model, also return coefficients on HR scale?
#' @return Data frame with original and adjusted coefficient estimates
#' @export
adjust_betas <- function(models, coef_name, lambdas, var_lambdas, hr_scale = TRUE) {
  # Check inputs in right form
  if ((length(coef_name) != length(models)) & (length(coef_name) != 1)) {
    stop("Length of coef_name vector should either be same length as list of models or 1.")
  }
  if ((length(lambdas) != length(models)) & (length(lambdas) != 1)) {
    stop("Length of lambdas vector should either be same length as list of models or 1.")
  }
  if ((length(var_lambdas) != length(models)) &
      (length(var_lambdas) != 1)) {
    stop(
      "Length of variance of lambdas vector should either be same length as list of models or 1."
    )
  }
  if ((length(var_lambdas) != length(var_lambdas))) {
    stop("Length of variance of lambdas vector should the same as length of lambdas vector")
  }

  if ((class(models) != "list")){
    models <- list("model" = models) # Aim here is if it's passed just one model, coerce to a list
  }
  # Extract model characteristics
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c("model", "beta_orig", "se_beta_orig")

  for (i in 1:length(models)) {
    # Deal with multiple coef_name
    if (length(coef_name) != 1){
      coef_name <- coef_name[i]
    }
    # Get model
    model <- models[[i]]

    # Extract info from model
    df <-
      rbind(df,
            data.frame(
              "model" = names(models)[i],
              "beta_orig" = as.data.frame(coef(summary(model)))[coef_name, "coef"],
              "se_beta_orig" = as.data.frame(coef(summary(model)))[coef_name, "se(coef)"]
            ))
  }

  # Calculate CIs on original scale
  df$ci_orig_lo <-
    df$beta_orig - 1.96 * df$se_beta_orig # Assuming approx normality
  df$ci_orig_hi <- df$beta_orig + 1.96 * df$se_beta_orig



  # Adjust beta and se
  df$beta_adj <- adjust_beta(df$beta_orig, lambdas)
  df$se_beta_adj <- adjust_se(df$beta_orig, df$se_beta_orig, lambdas, var_lambdas)

  # Calculated adjusted CIs
  df$ci_adj_lo <-
    df$beta_adj - 1.96 * df$se_beta_adj # Assuming approx normality
  df$ci_adj_hi <- df$beta_adj + 1.96 * df$se_beta_adj

  # Return outputs on HR scale too
  if (hr_scale == TRUE){
    df[, c("HR_orig",
           "HR_orig_lo",
           "HR_orig_hi",
           "HR_adj",
           "HR_adj_lo",
           "HR_adj_hi")] <-
      exp(df[, c("beta_orig",
                 "ci_orig_lo",
                 "ci_orig_hi",
                 "beta_adj",
                 "ci_adj_lo",
                 "ci_adj_hi")])
  }
  # Return
  return(df)
}

