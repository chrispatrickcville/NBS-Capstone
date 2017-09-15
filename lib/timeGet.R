timeGet <- function(df, dateCol, timeCol, newCol) {
  
  # Given a dataframe with a date column and a time column, returns
  # a dataframe with column of date and time combined
  
  # Inputs:
  # df         - data frame with columns to be converted to date+time
  # dateCol    - column in df with dates formatted as dates
  # timeCol    - column in df with times formatted as numbers in military time (e.g., 9:30
  #              a.m. should appear as 930, 1:45 pm should appear as 1345)
  # newCol     - name for the new column for combining dateCol1 + timeCol1
  
  
  # Change time column to string with leading zero (if is not converted already)
  if (!is.character(df[, timeCol])) {
    df[, timeCol] <- sprintf("%04d", df[, timeCol])
  }
  
  # Paste dates and times
  df[, newCol] <- paste0(df[, dateCol], df[, timeCol])
  
  # Change to date/time objects
  df[, newCol] <- as.POSIXct(strptime(df[, newCol], format = "%Y-%m-%d%H%M"))
  
  # Return the dataframe
  return(df)
  
}