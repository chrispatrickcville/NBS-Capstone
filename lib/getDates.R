getDates <- function(df, filt_col) {
  
  # Gets minimum and maximum date for the column data will be 
  # filtered by. Used as input for date_compare.
  
  # Get the earliest date from filt_col
  min_date = min(df[, filt_col], na.rm = TRUE)
  
  # Get the latest date from filt_col
  max_date = max(df[, filt_col], na.rm = TRUE)
  
  return(list(min_date, max_date))
  
}