formatStartAndEndDates <- function(s_date, e_date) {
  
  # Inputs start date and end date, checks that 
  # each is a valid date, checks that end date is
  # later than start date, and writes each as 
  # a date object to the global environment
  
  # Uses helper function formatDate
  
  start_date <<- formatDate(s_date, "start")
  end_date <<- formatDate(e_date, "end")
  
  # Check that end date is later than start date
  if (end_date < start_date) {
    date_message = addLineBreaks("\nERROR: Your end date is earlier than your start date. Please make sure your end date is later than your start date.\n")
    cat(date_message)
    stopQuietly()
  }
  
  # Convert dates to strings for use in file titles
  start_date_file <<- gsub("/", "-", start_date)
  end_date_file <<- gsub("/", "-", end_date)
  
}

