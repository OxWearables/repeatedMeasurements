#' Adjust a beta coefficient for regression dilution
#'
#' Adjusts a beta coefficient for regression dilution using a calculated value of lambda.
#'
#' @param beta Coefficient from epidemiological model to be adjusted
#' @param lambda Regression dilution coefficient
#' @return Regression-dilution adjusted beta coefficient
adjust_beta_oneval <- function(beta, lambda){
  return(beta/lambda)
}

#' Adjust standard error on a beta coefficient for regression dilution
#'
#' Adjusts the standard error on a beta coefficient for regression dilution analytically.
#'
#' @param beta Coefficient from epidemiological model to be adjusted
#' @param beta_se Standard error on beta coefficient from epidemiological model
#' @param lambda Regression dilution coefficient to adjust
#' @param var_lambda Variance of regression dilution coefficient estimate
#' @return Regression-dilution adjusted standard error of coefficient
adjust_se_oneval <- function(beta, beta_se, lambda, var_lambda){
  beta_se_adj <- sqrt((beta_se/lambda)^2 + ((beta/(lambda^2))^2)*var_lambda)
  return(beta_se_adj)
}

#' Adjust beta coefficients for regression dilution
#'
#' Adjusts beta coefficients for regression dilution using a calculated value of lambda. Arguments can be single values or vectors. This is a vectorised version of univariable adjustment i.e. it performs univariable adjustment for multiple models at once. It does NOT perform multivariable regression calibration.
#'
#' @param beta Coefficients from epidemiological model to be adjusted
#' @param lambda Regression dilution coefficient
#' @return Regression-dilution adjusted beta coefficients
#' @export
adjust_beta <- Vectorize(adjust_beta_oneval)



#' Adjust standard errors on beta coefficients
#'
#' Adjusts standard errors on a beta coefficient for regression dilution analytically. Arguments can be single values or vectors. This is a vectorised version of univariable adjustment i.e. it performs univariable adjustment for multiple models at once. It does NOT perform multivariable regression calibration.
#'
#' @param beta Coefficient from epidemiological model to be adjusted
#' @param beta_se Standard error on beta coefficient from epidemiological model
#' @param lambda Regression dilution coefficient to adjust
#' @param var_lambda Variance of regression dilution coefficient estimate
#' @return Regression-dilution adjusted beta coefficients
#' @export
adjust_se <- Vectorize(adjust_se_oneval)
