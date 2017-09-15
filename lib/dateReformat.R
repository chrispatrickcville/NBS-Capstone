dateReformat <- function(df, ...) {
  
  # Reformats set of columns as dates in a dataframe, first checks to see how year is 
  # formatted
  
  date_cols = list(...)
  
  for (col in unlist(date_cols)) {
    
    # Find separator character
    char = unique(unlist(strsplit(gsub("[0-9]+", "", df[1, col]), "")))
    
    # Get indices of rows (if any) where a 2-digit format is used for year
    ind_short_year = which(grepl(char, substrRight(df[, col], 3)))
    
    # If ind_short_year has any values, subset the data
    if (length(ind_short_year) > 0) {
    
      # Remove these rows from df
      df_short_year = df[ind_short_year, ]
      df = df[-ind_short_year, ]
    
      # Reformat these dates as dates
      df_short_year[, col] = as.Date(df_short_year[, col], "%m/%d/%y")
      df[, col] = as.Date(df[, col], "%m/%d/%Y")
    
      # Combine back into df
      df = rbind(df, df_short_year)
      
    } else {
      
      # Reformat entire dataframe as dates
      df[, col] = as.Date(df[, col], "%m/%d/%Y")
      
    }
    
  }
  
  return(df)
    
}