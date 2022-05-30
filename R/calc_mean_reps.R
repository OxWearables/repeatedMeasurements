#' Calculate mean of repeats with valid data
#'
#' Calculates mean of repeats with valid data
#'
#' @inheritParams calc_lambda
#' @return Column containing the mean of repeats with valid data
#' @export
calc_mean_reps <- function(var_name, data, suffix_baseline, suffix_rep, exclusion_indic){

  for (suff in suffix_rep) {
    data[, paste0("with_good_data", suff)] <- NA # need to assign first as only assigning non-NA on subset for inclusion

    # Condition for inclusion in calculation for this repeat:
    inc_ind <- (data[, paste0(exclusion_indic, suff)] == FALSE) &  !(is.na(data[, paste0(exclusion_indic, suff)]))

    # Store value for those included:
    data[inc_ind, paste0("with_good_data", suff)] <- data[inc_ind, paste0(var_name, suff)]

    rm(inc_ind)
  }

  col_to_return <- apply(data[, paste0("with_good_data", suffix_rep)], 1, mean, na.rm = TRUE)

  return(col_to_return)
}
