#' Assesses correlation between different errors
#'
#'
#' @param var_names Name of variables to calculate models for
#' @param suffix_baseline Suffix denoting a baseline measurement
#' @param suffix_rep Suffix(es) denoting a follow-up measurement
#' @param exclusion_indic Name of variable indicating row should be excluded for that repeat
#' @param data Dataset containing repeated measurements data
#' @return Correlation matrix
#' @export
correlate_errors <- function(var_names, data, suffix_baseline = "_s0", suffix_rep = c("_s1", "_s2", "_s3", "_s4"), exclusion_indic = "excluded", covariates = NULL){

  # Checks of inputs
  var_names_base <- paste0(var_names,  suffix_baseline)
  var_names_rep <- paste0(var_names, suffix_rep)

  for (var_loc in c(var_names_base, var_names_rep)){
    if (!(var_loc %in% colnames(data))){
      stop(paste0(var_loc, " does not appear in data. Is the data set up in the required format?"))
    }
  }

  diffs <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  for (var_name in var_names){
    # Mean of relevant repeat is outcome
    data[, paste0("mean_reps_w_data", var_name)] <- calc_mean_reps(var_name = var_name, data = data, suffix_baseline = suffix_baseline, suffix_rep = suffix_rep, exclusion_indic = exclusion_indic)

    # Diff is what we need to consider
    bas <- data[, paste0(var_name, suffix_baseline)]
    fup <- data[, paste0("mean_reps_w_data", var_name)]
    diff <- bas - fup

    # Assign
    diffs[, paste0("diff_", var_name)] <- diff

    # Tidy
    rm(bas, fup, diff)
  }

  # Return cross correlation
  return(cor(diffs))
}
