readData <- function(folder, patt=NULL, separator, type, remove_testing_cats=FALSE, date_cols=NULL) {
  
  # Returns dataframe of data. Optional arguments are columns to be reformatted as dates
  # (for use with csv and txt files).
  
  # Get file list
  temp <- getFileList(folder)
  
  # If pattern is identified, get files that contain pattern
  if (!is.null(patt)) {
    temp <- temp[grepl(patt, temp)]
  }
  
  # If temp is empty, output error message
  if (length(temp) < 2 && !grepl(".csv|.txt", temp)) {
    cat(addLineBreaks(paste0("\nERROR: There are no csv or txt files at the ", folder, " location. Please add data to this folder or redirect to a different location before before proceeding.")))
    stopQuietly(remove_ls = c('summary_filter', 'testing'))
  }
  
  # read in data and bind together
  lst <- lapply(temp, function(x) suppressWarnings(read.csv(x, stringsAsFactors = FALSE, 
                                                            header=TRUE, sep=separator, fileEncoding="latin1")))
  initial_dd <- rbindlist(lst, fill=TRUE)
  
  # read in data second time in order to get character vector of SAMPLEID
  sub_lst <- lapply(temp, function(x) suppressWarnings(read.csv(x, stringsAsFactors = FALSE, header=TRUE, 
                                                                sep=separator, fileEncoding="latin1",
                                                                colClasses="character")))
  
  sub_dd <- rbindlist(sub_lst, fill=TRUE)
  
  # replace initial_dd SAMPLEID with sub_dd version (to keep leading zeros)
  if ("SAMPLEID" %in% names(initial_dd)) {
    initial_dd$SAMPLEID <- as.character(sub_dd$SAMPLEID)
  }
  
  # change initial_dd to a dataframe
  initial_dd <- data.frame(initial_dd)
  
  # Strip odd characters from column names
  colnames(initial_dd) <- iconv(colnames(initial_dd), "latin1", "ASCII", sub="")
  colnames(initial_dd) <- gsub("\\.\\.", "", colnames(initial_dd))
  
  # If remove_cats is set to TRUE, remove 'testing' records from data
  if (remove_testing_cats) {
    remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
    initial_dd <- initial_dd[!(initial_dd$CATEGORY %in% remove_cats),]
    initial_dd$CATEGORY <- NULL
  }
  
  # Remove any records from temp_file that appear in 'examples' and strip spaces from critical results
  if (type == "call_log") {
    
    initial_dd <- removeExamples(examps, initial_dd)
    
    for (c in colnames(initial_dd)[grepl("Critical", colnames(initial_dd))]) {
      initial_dd[, c] <- trim(initial_dd[, c])
    }
    
  }
  
  # Remove unneeded columns
  if (type == "sample") {
    cols = sample_cols
  } else if (type == "diagnosis") {
    cols = diagnosis_cols
  } else if (type == "call_log") {
    cols = call_log_cols
  } else if (type == "approve_log") {
    cols = approve_log_cols
  }
  
  if (exists("cols")) {
    initial_dd <- subset(initial_dd, select=cols)
  }
  
  # Reformat and repair any specified columns as dates
  if (!is.null(date_cols)) {
    initial_dd <- dateReformat(initial_dd, date_cols)
    initial_dd <- dateRepair(initial_dd, type, date_cols)
  }
  
  # Reformat SUBMITTERID as character
  if("SUBMITTERID" %in% names(initial_dd)) {
    initial_dd$SUBMITTERID <- as.character(initial_dd$SUBMITTERID)
  }
  
  # For approve_log data:
  if (type == "approve_log") {
    
    # Filter for just results containing 'Critical'
    initial_dd <- initial_dd[grepl("critical", tolower(initial_dd$Task.Appr.Status)), ] 
    
    # Select and rename columns of interest
    initial_dd <- initial_dd %>%
      mutate(EXTERNAL_ID = Control.ID,
             combined = toupper(Test.Name),
             Date.of.Call = Approved.Date) %>%
      select(EXTERNAL_ID, combined, Date.of.Call)
    
    # Restructure data to group by EXTERNAL_ID and Date.of.Call
    initial_dd <- aggregate(combined ~ EXTERNAL_ID + Date.of.Call, data = initial_dd, toString)
    initial_dd$combined <- gsub(" ", "", initial_dd$combined)
    
  }
  
  # For call_log data:
  if (type == "call_log") {
    
    # Combine all analytes called for a sample into a single cell, separated by commas
    initial_dd$combined <- apply(subset(initial_dd, select=Critical.1:Critical.5), 1, function (x) 
      gsub(" ", "," ,sub("\\s+$", "", gsub("NA", "", paste(toupper(x), collapse=" ")))))
    
    # Select columns of interest
    initial_dd <- initial_dd %>%
      select(EXTERNAL_ID, combined, Date.of.Call)
    
  }
  
  # Remove any '-', '/', or ':' characters from combined field from critical data 
  # (to limit the number of different representations of analytes)
  if("combined" %in% names(initial_dd)) {
    initial_dd$combined <- gsub("[/:-]", "", initial_dd$combined)
  }
  
  # Search for any diagnoses combined into a single field
  if (type == "diagnosis") {
    
    # Search for "*:" in DIAGNOSIS (indicating more than one diagnosis is listed)
    initial_dd_sub <- initial_dd[grepl("[*]", initial_dd$DIAGNOSIS),]
    
    # Remove these records from initial_dd
    initial_dd <- initial_dd[!grepl("[*]", initial_dd$DIAGNOSIS),]
    
    # Create copy of initial_dd_sub
    initial_dd_sub2 <- initial_dd_sub
    
    # Retain everything before the asterisk in first df
    initial_dd_sub$DIAGNOSIS <- sub("[*].+", "", initial_dd_sub$DIAGNOSIS)
    
    # Retain everything after the asterisk, colon, space in second df
    initial_dd_sub2$DIAGNOSIS <- gsub("[^*]+[*]:[[:space:]]+", "", initial_dd_sub2$DIAGNOSIS)
    
    # Rbind the dfs back to initial_dd
    initial_dd <- rbind(initial_dd, initial_dd_sub, initial_dd_sub2)
    
  }
  
  # Reformat EXTERNAL_ID as character
  if("EXTERNAL_ID" %in% names(initial_dd)) {
    initial_dd$EXTERNAL_ID <- as.character(initial_dd$EXTERNAL_ID)
  }
  
  # Remove any duplicate rows
  initial_dd <- unique(initial_dd)
  
  return(initial_dd)
  
}