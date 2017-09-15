sumCheckOutOfRange = function(cols, df, time_critical) {
  
  # Checks that the sum of calculated values across a set of columns for
  # out of range results (either time-critical or non-time-critical) in
  # the Monthly Template matches the expected value (sum of all time-criticals
  # or non-time-criticals)
  
  # Accepted values for time_critical_indic are TRUE (for time-critical out-of-range
  # results) or FALSE (for non-time-critical out-of-range results)
  
  # Test for time_critical being entered correctly
  if (time_critical == TRUE) {
    col_check = t_list
  } else if (time_critical == FALSE) {
    col_check = n_list
  } else {
    cat(addLineBreaks("\nPlease enter either TRUE or FALSE for the third argument in the 'sumCheckOutOfRange' function; TRUE if you wish to filter for time-critical results and FALSE if you wish to filter for non-time-critical results."))
    stopQuietly()
  }
  
  df <- data.frame(df)
  
  expect_sum <- sum(df[, col_check])
  test_sum <- rowSums(Monthly_Template[grepl(cols, colnames(Monthly_Template))])
  
  # If the sums do not match, create an error message, print it out, and stop the running
  # of the report.
  if (expect_sum != test_sum) {
    message = addLineBreaks(paste0("The sum of the amounts for the '", cols, 
                     "' columns is ", test_sum, ", which does not match the expected value, ", 
                     expect_sum, ". Please review the 'NewSTEPs_QIs.R' file by searching for '", cols, 
                     "' to determine the source of the problem."))
    cat(message)
    stopQuietly()
    
  }
  
}