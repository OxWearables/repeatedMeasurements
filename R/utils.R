round_dp <- function(dp) {
  force(dp)
  function(x) {
    format(round(x, digits = dp), nsmall = dp, trim = TRUE)
  }
}
#' Utility function to round to 3 d.p.
#'
#' @param x Object to round
#' @return Input rounded to 3 dp
#' @export
round_2_dp <- round_dp(3)

#' Utility function to round to 2 d.p.
#'
#' @param x Object to round
#' @return Input rounded to 2 dp
#' @export
round_2_dp <- round_dp(2)

#' Utility function to round to 1 d.p.
#'
#' @param x Object to round
#' @return Input rounded to 1 dp
#' @export
round_1_dp <- round_dp(1)

