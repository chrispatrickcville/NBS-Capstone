timeDiff <- function(df, dateCol1, timeCol1, dateCol2, timeCol2, newCol1, newCol2, newColDiff, u_val="days",
                     round_val=NULL) {
  
  # Given a dataframe and a set of date columns and time columns, returns
  # a dataframe with columns of dates and times combined, as well as a column
  # of the differences between the dates. Uses 'timeGet' function.
  
  # Inputs:
  # df         - data frame with columns to be evaluated for date & time difference
  # dateCol1   - column in df with dates formatted as dates. This column should
  #              have dates earlier than dateCol2 (e.g., if evaluating the difference
  #              between BIRTHDATE and COLLECTIONDATE, BIRTHDATE should be dateCol1)
  # timeCol1   - column in df with times formatted as numbers in military time (e.g., 9:30
  #              a.m. should appear as 930, 1:45 pm should appear as 1345)
  # dateCol2   - see note for dateCol1
  # timeCol2   - see note for timeCol1
  # newCol1    - name for the new column for combining dateCol1 + timeCol1. This
  #              function will only find this column if the column does not already
  #              exist.
  # newCol2    - name for the new column for combining dateCol2 + timeCol2. This
  #              function will only find this column if the column does not already
  #              exist.
  # newColDiff - column with date difference
  # u_val      - how to express the date difference; default is days
  # round_val  - number of digits for rounding newColDiff (if NULL, will not be rounded)
  
  
  # Format times, paste dates and times, and change to date/time objects
  for (i in 1:2) {
    
    new_col = eval(parse(text=paste0("newCol", i)))
    
    if (!new_col %in% names(df)) {
      
      date_col = eval(parse(text=paste0("dateCol", i)))
      time_col = eval(parse(text=paste0("timeCol", i)))
      
      df <- timeGet(df, date_col, time_col, new_col)
      
    }
    
  }
  
  # Get time difference between two new columns
  df[, newColDiff] <- as.numeric(difftime(df[, newCol2], df[, newCol1], units=u_val))
  
  # Change to NA if value is negative (indicating a DATE or TIME metric has not been entered correctly)
  df[, newColDiff] <- ifelse(df[, newColDiff] < 0, NA, df[, newColDiff])
  
  # If round_val is not NULL, round to the number of digits indicated
  if (!is.null(round_val)) {
    df[, newColDiff] <- round(df[, newColDiff], round_val)
  }
  
  return(df)

}