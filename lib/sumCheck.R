sumCheck = function(cols, df) {
  
  # Checks that the sum of calculated values across a set of columns in
  # the Monthly Template matches the expected value (nrows of a filtered
  # dataframe)
  
  expect_sum <- nrow(df)
  test_sum <- rowSums(Monthly_Template[grepl(cols, colnames(Monthly_Template))])
  
  # If the sums do not match, create an error message, print it out, and stop the running
  # of the report.
  if (expect_sum != test_sum) {
    message = addLineBreaks(paste0("\nThe sum of the amounts for the '", cols, 
                     "' columns is ", test_sum, ", which does not match the expected value, ", 
                     expect_sum, ". Please review the 'NewSTEPs_QIs.R' file by searching for '", cols, 
                     "' to determine the source of the problem."))
    cat(message)
    stopQuietly()
    
  }
  
}
