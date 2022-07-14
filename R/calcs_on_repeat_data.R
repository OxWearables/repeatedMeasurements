#' Calculate mean of repeats with valid data
#'
#' Calculates mean of repeats with valid data
#'
#' @inheritParams calc_lambda
#' @return Column containing the mean of repeats with valid data
calc_mean_reps <- function(var_name, data, suffix_rep, exclusion_indic){

  # Store value for those included:
  for (suff in suffix_rep){
    inc_ind <- make_inc_ind(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)[, paste0("with_good_data", suff)]
    data[, paste0(var_name, "_with_good_data_", suff)] <- NA
    data[inc_ind, paste0(var_name, "_with_good_data_", suff)] <- data[inc_ind, paste0(var_name, suff)]
  }

  # Calculate mean
  col_to_return <- apply(data[, paste0(var_name, "_with_good_data_", suffix_rep)], 1, mean, na.rm = TRUE)

  return(col_to_return)
}


#' Count valid repeats
#'
#' Utility function to count valid repeats
#'
#' @inheritParams calc_lambda
#' @return Vector with number of valid repeats for each row
n_valid_repeats <- function(data, suffix_rep, exclusion_indic){
  data <- make_inc_ind(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)
  col_to_return <- apply(data[, paste0("with_good_data", suffix_rep)], 1, sum, na.rm = TRUE)

  return(col_to_return)
}

#' Calculate precision weights
#'
#' @inheritParams calc_lambda
#' @return Vector of precision weights
calc_precision_weights <- function(data, suffix_rep, exclusion_indic, precision_weighted){
  if (precision_weighted){
    precision_weights <- n_valid_repeats(data = data, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic) # This is because the weight should be inversely proportional to variance and variance of mean is proportional to 1/n (SE of mean proportional to 1/sqrt(n))
  }
  else {
    precision_weights <- NULL
  }
  return(precision_weights)
}



#' Make inclusion indicator
#'
#' @inheritParams calc_lambda
#' @return Add inclusion indicator for each repeat to data
make_inc_ind <- function(data, suffix_rep, exclusion_indic){

  for (suff in suffix_rep) {
    # Condition for inclusion in calculation for this repeat:
    inc_ind <- (data[, paste0(exclusion_indic, suff)] == FALSE) & !(is.na(data[, paste0(exclusion_indic, suff)]))

    # Store value for those included:
    data[, paste0("with_good_data", suff)] <- inc_ind

    # remove vector
    rm(inc_ind)
  }
  return(data)

}
