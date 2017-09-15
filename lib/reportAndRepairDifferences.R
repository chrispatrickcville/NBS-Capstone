reportAndRepairDifferences <- function(df, id, type=c("sample", "diagnosis", "critical reporting"), 
                                       module=c("Report Card", "Epi Tool", "NewSTEPs"), 
                                       cols, eval_cols=NULL) {
  
  # Given a dataframe and an ID column, identify all duplicated
  # IDs as well as any columns for those IDs that have 
  # differing values.
  
  # Generates a warning if such samples exist, creates
  # an ancillary report listing them, and changes the values
  # for columns with differing values to NAs.
  
  # Inputs:
  #    df         - data frame to be evaluated for erroneously duplicated observations, occurring
  #                 because of multiple values for one or more variables matching to a particular identifier
  #                 (for example, a sample ID being problematically associated with two different 
  #                 birthdates)
  #    id         - name of column with IDs (e.g., SAMPLEID)
  #    type       - sample, diagnosis, or critical reporting (indicating which type of data we're evaluating)
  #    module     - indicates which module is being run (e.g., Report Card)
  #    cols       - IMPORTANT: this is a column or a set of columns from your data that 
  #                 you suspect may have more than one value for a particular
  #                 identifier. For example, if you suspect that a sample appears
  #                 more than once in your data with 2 different BIRTHDATES, you
  #                 would want 'BIRTHDATE' to be your cols value (do not include
  #                 your ID column in this set). Include ALL columns you believe
  #                 may have differing values, **with this exception**: if you have a case 
  #                 where you SHOULD have the possibility of differing values associated 
  #                 with a particular identifier (as in the case of diagnosis data, because 
  #                 a sample may be correctly associated with more than one diagnosis), 
  #                 make cols equal to the columns that could correctly have more than
  #                 one value (e.g., DIAGNOSIS), and set eval_cols to the column(s) that
  #                 should not have different values when everything else is the same.
  #                 In the case of diagnosis data, DIAGNOSIS should definitely be at 
  #                 least one of the cols values (I have also included LINKID and SUBMITTERID).
  #    eval_cols  - See above. Use this argument only when you have data where there SHOULD
  #                 be columns that have more than one value associated with a particular
  #                 identifier (e.g., diagnosis data will at times have more than one diagnosis
  #                 associated with the same sample ID). In such a scenario, eval_cols
  #                 should be set to the column(s) that should **NOT** differ when everything
  #                 else is the same --- in the case of diagnosis data, that would be DIAGNOSIS
  #                 DATE. A single sample associated with a single diagnosis should not have 
  #                 2 different diagnosis dates, so this is the element we want to test for.
  
  # Identify set of columns we want to evaluate for
  # duplicates and obtain SAMPLEIDs with duplicates. This 
  # differs for sample and diagnosis data. For diagnosis data, 
  # we only want cases where there are differing values for 
  # DIAGNOSISDATE for the same sample ID (when everything else
  # about the sample is the same)
  
  cols = unlist(cols)
  if (!is.null(eval_cols)) {
    eval_cols = unlist(eval_cols)
  }
  
  if (type == "sample") {
    col_text = "at least one column"
  } else if (type == "diagnosis") {
    col_text = "the DIAGNOSISDATE column"
  } else if (type == "critical reporting") {
    col_text = "the date of call column"
  }
  
  # Get ids that are duplicated in the columns of interest
  # (by subsetting on the columns not of interest and finding
  # the ids for any duplicates in this set)
  df_sub = subset(df, select=!colnames(df) %in% cols)
  dupes = df[, id][duplicated(df_sub)]
  
  # If dupes is empty, return unaltered df
  if (length(dupes) == 0) {
    
    return(df)
    
  } else {
    
    # Subset data for observations with IDs that appear in dupes
    df_dupes = df[df[, id] %in% dupes, ]
    
    # Remove this duplicated data from df
    df_red <- anti_join(df, df_dupes, by=id)
    
    # Get list of columns with different values by sample, also 
    # set any differing values to NA
    DIFFERING_COLUMNS = c()
    
    for (s in dupes) {
      temp = df_dupes[df_dupes[, id]==s, ]
      bad_cols = c()
      for (column in cols) {
        temp_red = temp[, c(column, eval_cols)]
        temp_red = unique(temp_red)
        if (length(temp_red) != 1) {
          bad_cols = c(bad_cols, column)
          df_dupes[df_dupes[, id]==s, column] <- NA
        }
      }
      
      bad_cols = paste(bad_cols, collapse=", ")
      DIFFERING_COLUMNS = c(DIFFERING_COLUMNS, bad_cols)
      
    }
    
    bad_df = data.frame(cbind(dupes, DIFFERING_COLUMNS))
    names(bad_df)[names(bad_df)=="dupes"] <- id
    
    # Write results to csv
    write.csv(bad_df, paste0(admin_path, slash, start_date_file, "_", end_date_file, "_", module, "_", id, 
                             "s duplicated in ", type, " data.csv"), row.names=FALSE)
    
    # Reduce df_dupes to unique values (now that NAs have overwritten
    # differing records)
    df_dupes = unique(df_dupes)
    
    # Join df_dupes back to df_red
    df <- rbind(df_red, df_dupes)
    
    # Plural for message if bad_df has more than one row
    plural <- ifelse(nrow(bad_df) == 1, "", "s")
    verb <- ifelse(nrow(bad_df) == 1, "is", "are")
    adj <- ifelse(nrow(bad_df) == 1, "this", "these")
    
    # Report out issue
    if (!exists("testing")) {
      message <- addLineBreaks(paste0("\nWARNING: ", length(dupes), " ", id, plural, " ", verb, 
                        " listed more than once in the ", type, " data, which is caused by ", adj, " sample", plural, 
                        " being associated with different values in ", col_text, 
                        ". Information on ", adj, " sample", plural, " and the problematic column(s) is in your output folder in the '", 
                        id, "s duplicated in ", type, 
                        " data' csv file. Also, any differing values have been overwritten with NA values and the duplicates have been removed (although they still exist in the source data).\n"))
    
      cat(message)
    }
    
    # Return df
    return(df)
    
  }
}