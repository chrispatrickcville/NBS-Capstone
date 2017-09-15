removeExamples <- function(examples, df) {
  
  # Removes from call log data any rows that appear
  # in the example file
  
  # Get subset of columns from df and examples that are likely to be formatted the same way. 
  df_sub = subset(df, select = c(EXTERNAL_ID, BIRTH.TIME, Critical.1:Critical.5))
  temp_sub = subset(examples, select = c(EXTERNAL_ID, BIRTH.TIME, Critical.1:Critical.5))
  
  # Bind both dataframes together
  both = rbind(df_sub, temp_sub)
  
  # Get which indices for duplicated rows in 'both' (reading from end of the dataframe to 
  # the beginning). This will give the indices in df that need to be removed (i.e., are
  # the same as the observations in 'examples')
  indices = which(duplicated(both, fromLast = TRUE))
  
  # Remove duplicated indices from df if indices has any values
  if (length(indices) > 0) {
    df = df[-indices, ]
  }
  
  # Return the filtered df
  return(df)
  
}
