dateCompare <- function(data_type, start_or_end, compare_obj, filt_col) {
  
  # Checks data to see if start date or end date desired by user
  # is outside of bounds of data source
  
  # compare_obj is a list that contains minimum and maximum date
  # from the data source (created by date_check function)
  
  comp_s = as.Date(compare_obj[1][[1]]) # minimum date
  comp_e = as.Date(compare_obj[2][[1]]) # maximum date
  
  if (start_or_end == "start") {
    adj <- "earliest"
    adv <- "earlier"
    date <- start_date
    compare_date <- comp_s
  } else {
    adj <- "latest"
    adv <- "later"
    date <- end_date
    compare_date <- comp_e
  }
  
  # Stop report completely if requested start date is after the end date in the data
  # (regardless of whether user has indicated a check for start or end)
  if (start_date > comp_e) {
    message <- addLineBreaks(paste0("\nERROR: Your desired start_date, ", start_date, ", is later than any dates for ", 
                                    filt_col, " in your ", data_type, " data source (the latest of which is ", comp_e, 
                                    "). Because there is no overlap between your requested dates and the dates in your data, the report generation will stop."))
    cat(message)
    stopQuietly()
  }
  
  # Stop report completely if requested end date is before the start date in the data
  # (regardless of whether user has indicated a check for start or end)
  if (end_date < comp_s) {
    message <- addLineBreaks(paste0("\nERROR: Your desired end_date, ", end_date, ", is earlier than any dates for ", 
                                    filt_col, " in your ", data_type, " data source (the earliest of which is ", comp_s, 
                                    "). Because there is no overlap between your requested dates and the dates in your data, the report generation will stop."))
    cat(message)
    stopQuietly()
  }
  
  # Allow user to choose to stop report if start date is after minimum date in the data or end date
  # is before maximum date in the data
  if ( (start_or_end == "start" & comp_s > start_date) || (start_or_end == "end" & comp_e < end_date) ) {
    message1 <- addLineBreaks(paste0("\nWARNING: Your desired ", start_or_end, "_date, ", date, ", is ", adv, 
                      " than any dates for ", filt_col, " in your ",
                      data_type, " data source (the ", adj , " of which is ", as.Date(compare_date), ")."))
    message2 <- "\n\nDo you wish to proceed? (enter 'Y' or 'y' for yes)\n"
    cat(paste0(message1, message2))                
    ans <- readline(prompt = "> ")
    if (tolower(ans) != 'y') {
      cat("\nStopping per your request.")
      stopQuietly()
    } else {
      message <- paste0("\nContinuing with report generation with your selection for ", start_or_end, "_date.\nThis will take a few moments.\n")
      cat(message)
    }
  }
}