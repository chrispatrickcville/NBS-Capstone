getCols <- function(df_row, colValue, colsToCheck, strCheck, cutoff=1) {
  
  # Returns a list of all analyte columns that feature a certain value,
  # when the number of columns that contain that value is greater than
  # a cutoff value
  
  # df_row -      a row of sample data that features columns of analyte results
  # colValue -    which column in the df_row that has a stored list of analytes
  #               that have already been checked
  # colsToCheck - which set of columns to check for result values 
  # strCheck    - string to check for in analyte columns (e.g., 'Abnormal',
  #               'Critical', or both - 'Abnormal|Critical')
  # cutoff      - count of values equivalent to a Critical. For example,
  #               this should be set to 2 when checking for combinations of
  #               Abnormal or Critical results, but set to 1 when just checking
  #               for Critical results (since a single critical is treated as 
  #               a critical)
  
  # if length of vector containing string is greater than cutoff:
  if (sum(grepl(strCheck, df_row[colsToCheck])) >= cutoff) {
    
    # Creates string containing all column names that contain the strCheck
    cols <- paste0(names(unlist(lapply(df_row[colsToCheck], function(x) which(grepl(strCheck, x))))),
                   collapse=",")
    
  } else {
    
    cols <- ""
    
  }
  
  # If value of colValue is not NULL, NA, or "", and if cols != "", 
  # paste the current results to the previous value; otherwise cols = previous value
  if (!is.null(df_row[colValue]) & !is.na(df_row[colValue]) & as.character(df_row[colValue]) != "") {
    
    if (cols != "") {
      cols <- paste0(as.character(df_row[colValue]), ",", cols)
    } else {
      cols <- as.character(df_row[colValue])
    }
    
  }
  
  # Remove duplicates from cols
  if (cols != "") {
    col_list <- unique(unlist(strsplit(cols, ",")))
    cols <- paste(col_list, collapse=",")
    
  }
  
  return(cols)
  
}