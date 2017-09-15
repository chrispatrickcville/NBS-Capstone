dateCompCheck <- function(df, data_type, filt_col) {
  
  # Compares start date and end date entered by user with
  # minimum and maximum dates of filter column defined 
  # by user.
  
  # Uses getDates and dateCompare functions.
  
  # Get minimum and maximum dates for filter column in df
  date_test <- getDates(df, filt_col)
  
  # Perform check of start date and end date (if "year_check" does not exist)
  if (!exists("year_check")) {
  
    # Check that earliest date for filt_col in data is earlier than requested start_date
    dateCompare(data_type, "start", date_test, filt_col)
    
    # Check that latest date for filt_col in data is later than requested end_date
    dateCompare(data_type, "end", date_test, filt_col)
    
  } else {
    
    # Otherwise, test that there is one year of data for supporting the
    # report card visualizations
    
    comp_s <- as.Date(date_test[1][[1]]) 
    
    # Get date one year prior to end date
    d_check <- end_date - years(1)
    
    # Check to see if d_check is earlier than dates in data
    if(d_check < comp_s) {
      message1 <- addLineBreaks(paste0("\nWARNING: The earliest date in your sample data is ", comp_s, 
                                       ", so the plots in your report cards will not show a full year of performance."))
      message2 <- "\n\nDo you wish to proceed? (enter 'Y' or 'y' for yes)\n"
      cat(paste0(message1, message2))                
      ans <- readline(prompt = "> ")
      if (tolower(ans) != 'y') {
        cat("\nStopping per your request.")
        stopQuietly()
      } else {
        message <- paste0("\nContinuing with report generation.\nThis will take a few moments.\n")
        cat(message)
      }
      
    }
    
  }
  
}