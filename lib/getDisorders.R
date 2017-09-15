getDisorders <- function(df, check_diag=FALSE, remove_diag=TRUE) {
  
  # Finds DISORDER for each DIAGNOSIS in diagnosis data and returns
  # data frame with DISORDER added. 
  
  # Inputs:
  #    df -          Diagnosis data, including DIAGNOSIS column
  #    check_diag -  If set to TRUE, perform check to see if all DIAGNOSES are 
  #                  listed in diag_narr and report out if not
  #    remove_diag - If set to TRUE, will remove DIAGNOSIS column
  #                  and remove any records that are duplicated without
  #                  this column (e.g., if a sample is associated with
  #                  more than one diagnosis, but both of those diagnoses
  #                  are associated with the same disorder)

  # Read in diganosis narrative file for use in cleaning data
  diag_narr <<- read.csv(paste0(codes_path, slash, 'diagnosis_narratives.csv'), stringsAsFactors = FALSE)
  
  #### Explode the VariantNames so these can be checked against the values in the diagnosis data ####
  
  # Split up the values
  var_split <- strsplit(diag_narr$VariantNames, ";", fixed = TRUE)
  
  # Determine length of each element
  num_col <- vapply(var_split, length, 1L)
  
  # Create an empty character matrix to store the results
  exp_var <- matrix(NA_character_, nrow = nrow(diag_narr),
                    ncol = max(num_col), 
                    dimnames = list(NULL, paste0("V", sequence(max(num_col)))))
  
  # Use matrix indexing to figure out where to put the results
  exp_var[cbind(rep(1:nrow(diag_narr), num_col), 
                sequence(num_col))] <- unlist(var_split, use.names = FALSE)
  
  # Bind the values back together
  diag_extend <- cbind(diag_narr, exp_var)
  
  # Get length and width of diag_extend
  diag_extend_len <- nrow(diag_extend)
  diag_extend_wid <- ncol(diag_extend)
  
  ####
  
  # Find the disorder name for each diagnosis in the data
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) {
      loc = which(diag_extend[,c(6:diag_extend_wid)]==df$DIAGNOSIS[i])
      if (length(loc) == 0) {
        loc = NA
        df$DISORDER[i] <- NA
      } else {
        num_row = ifelse(loc %% diag_extend_len != 0, loc %% diag_extend_len, diag_extend_len)
        df$DISORDER[i] <- diag_extend[num_row, 1]
      }
    }
  }
  
  # Check for any missing diagnoses in diagnoses narratives
  if (check_diag) {
    
    need_disorder_list <- df %>%
      filter(DISORDER == "" | is.na(DISORDER)) %>%  
      select(DIAGNOSIS)
    
    need_disorder_list <- unique(need_disorder_list)
    
    # If need_disorder_list has any records, create csv in admin_path and send warning
    # message to console
    checkNotInDiagnosisNarratives(need_disorder_list, proceed=TRUE)
    
  }
  
  # Remove diagnoses if remove_diag is set to TRUE and remove any records
  # that are duplicated after DIAGNOSIS is removed
  if (remove_diag) {
    
    df$DIAGNOSIS = NULL
    df <- unique(df)
    
  }
  
  return(df)
  
}