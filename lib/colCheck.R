colCheck <- function(folder, type) {
  
  # Checks that a given list of files have the correct columns, returns
  # list of 'bad files' along with column names that are not found in the
  # files indicated.
  
  # Get file list
  temp = getFileList(folder)
  
  # If temp is empty, output error message
  if (length(temp) < 2 && !grepl(".csv|.txt", temp)) {
    cat(addLineBreaks(paste0("\nERROR: There are no csv or txt files at the ", folder, " location. Please add data to this folder or redirect to a different location before before proceeding.")))
    stopQuietly(remove_ls = c('summary_filter', 'testing'))
  }
  
  # Loop through files to ensure they all have the correct columns
  bad_files = c()
  
  if (type == "sample") {
    cols =  sample_cols
    separator = "|"
  } else if (type == "diagnosis") {
    cols = diagnosis_cols
    separator = "|"
  } else if (type == "call_log") {
    cols = call_log_cols
    separator = ","
  } else if (type == "approve_log") {
    cols = approve_log_cols
    separator = ","
  }
  
  for (f in temp) {
    # Read in single row from each file to get the column names
    temp_file = suppressWarnings(read.table(f, nrows = 1, header = TRUE, sep=separator, 
                                            fill = TRUE))
    
    # Strip odd characters from column names
    colnames(temp_file) <- iconv(colnames(temp_file), "latin1", "ASCII", sub="")
    colnames(temp_file) <- gsub("\\.\\.", "", colnames(temp_file))
    
    # Find set difference between expected columns and columns in temp_file
    bf_temp = setdiff(cols, colnames(temp_file))
    if (length(bf_temp) != 0) {
      bad_files = c(bad_files, paste0(f, ":\n   missing columns - ", paste(bf_temp, collapse = ", "), "\n"))
    } 
    
  }
  
  if (length(bad_files) != 0) {
    e_begin <- ifelse(length(bad_files) == 1, "One file", "Several files")
    e_verb <- ifelse(length(bad_files) == 1, "does", "do")
    e_art <- ifelse(length(bad_files) == 1, "this", "these")
    e_plur <- ifelse(length(bad_files) == 1, "", "s")
    message1 = addLineBreaks(paste0("\n", e_begin, " in the ", type, "_data_path location ", e_verb, 
                      " not have the required column headings. Please check the column headings for ",
                      e_art, " file", e_plur, " before running reports:\n\n"))
    message = paste0(message1, paste0(bad_files, collapse=""))
    cat(message)
    stopQuietly()
  }
  
}