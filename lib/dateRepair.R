dateRepair <- function(df, type=c("sample", "diagnosis"), ...) {
  
  # Repairs dates that have been read incorrectly (e.g., if year is listed as '0016' 
  # instead of '2016'). Dates for checking should have already been formatted as dates.
  # Also reports out if any dates are bad.
  
  # Requires that several variables be set outside the function, including:
  #   - slash - character for separating folder from file name
  #   - admin_path - location for outputting Bad dates file
  
  date_cols = list(...)
  
  # Initialize df_bad to store rows with bad date info
  df_bad <- df
  df_bad$BAD_DATE_COL <- NA
  df_bad <- df_bad[0, ]
  
  # Pull all records where the dates do not appear to have been formatted correctly
  for (col in unlist(date_cols)) {
    
    rows = which(!is.na(df[, col]) & df[, col] < as.Date("1950-01-01")) 
    
    if (length(rows) > 0) {
      
      # Combine bad rows to df_bad
      df_temp <- df[rows, ]
      df_temp$BAD_DATE_COL <- col
      df_bad <- rbind(df_bad, df_temp)
      
      for (row in rows) {
        
        if (as.numeric(substr(df[row, col], 3, 4)) > as.numeric(format(Sys.Date(), "%y"))) {
          cent = "19"
        } else {
          cent = "20"
        }
        
        repaired = gsub("00", cent, df[row, col])
        df[row, col] = as.Date(repaired, format = "%Y-%m-%d")
        
      }
      
    }
    
  }
  
  if (nrow(df_bad) > 0) {
    
    if (nrow(df_bad) == 1) {
      verb = "is"
      plural = ""
      bd = "a bad date"
      pro = "this record"
    } else {
      verb = "are"
      plural = "s"
      bd = "bad dates"
      pro = "these records"
    }
    
    message <- addLineBreaks(paste0("\nWARNING: There ", verb, " ", nrow(df_bad), 
                      " record", plural, " with ", bd, " in the ", type, " data. Although R will fix the date", plural, 
                      " for analysis, ", pro, " should be repaired in LIMS. Please see the 'Bad dates for repair in LIMS_", type, 
                      " data.csv' file in your output directory for more information.\n"))
    cat(message)
    write.csv(df_bad, paste0(admin_path, slash, "Bad dates for repair in LIMS_", type, " data.csv"), row.names=TRUE)
    
  }
  
  return(df)
  
}