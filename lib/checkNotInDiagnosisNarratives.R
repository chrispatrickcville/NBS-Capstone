checkNotInDiagnosisNarratives <- function(df, proceed=FALSE) {
  
  # Given a data frame of diagnoses that do not appear in 
  # the diagnosis_narratives.R file, generates a warning
  # message and outputs a file to the admin_path folder
  
  # If zero_message is set to TRUE, will print message to
  # console indicating that all diagnoses are listed in 
  # the diagnosis_narratives.csv. Note that zero_message
  # must be set outside of the function.
  
  # If proceed is set to TRUE, function will ask user if 
  # they wish to proceed with the analysis.
  
  # Print warning message to console and save file of 
  # information on missing diagnoses to admin_path folder
  if (nrow(df) != 0) {
    
    # Write file to admin_path
    write.csv(df, paste0(admin_path, slash, "need_adding_to_diagnosis_narratives.csv"), row.names=FALSE)
    
    if (nrow(df) == 1) {
      verb1 = "is a"
      plural1 = "diagnosis"
      verb2 = "is"
      pron = "this"
      plural2 = ""
    } else {
      verb1 = "are"
      plural1 = "diagnoses"
      verb2 = "are"
      pron = "these"
      plural2 = "s"
    }
    miss_disorders <- paste0("  ", as.character(df$DIAGNOSIS), collapse="\n")
    message1 <- addLineBreaks(paste0("\nWARNING: There ", verb1, " ", plural1, 
                       " in the diagnosis data that ", verb2, 
                       " not yet listed in the 'VariantNames' column of the diagnosis_narratives.csv file. Please enter ",
                       pron, " ", plural1, " to that column (in the appropriate row", plural2, 
                       ") before proceeding with any analyses involving diagnoses:\n\n"))
    message3 <- addLineBreaks(paste0("\n\nThis information is also in the 'need_adding_to_diagnosis_narratives.csv' file in your output folder."))
    message <- paste0(message1, miss_disorders, message3)
    cat(message)
    
    # Check if 'proceed' is set to TRUE. If so, allow user to decide if she will continue 
    # running the report.
    if (proceed) {
      cat("\n\nDo you wish to proceed? (enter 'Y' or 'y' for yes)\n")
      ans <- readline(prompt = "> ")
      if (tolower(ans) != 'y') {
        cat("\nStopping per your request.")
        stopQuietly()
      } else {
        message <- addLineBreaks(paste0("\nContinuing with report generation. Note that the ", plural1, " listed above will NOT be included in the analysis. This will take a few moments.\n"))
        cat(message)
      }
    }
    
  } else {
    
    # Otherwise, there are no rows in the dataframe (and no diagnoses that need to be added to diagnosis_narratives.csv)
    if (exists("zero_message")) {
      
      message <- "\nAll diagnoses for the period of interest are listed in diagnosis_narratives.csv."
      cat(message)
    
    }
    
  }
  
}