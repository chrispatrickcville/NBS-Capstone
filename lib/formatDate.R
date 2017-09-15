formatDate <- function(strDate, type) {
  
  # Takes a string and converts to date. If 
  # cannot convert to date outputs an error 
  # message and stops the function
  
  # Identify the character used to separate elements of the date (e.g., "-" or "/")
  char = unique(unlist(strsplit((gsub("[0-9]", "", strDate)), "")))
  
  # Reformat the date as date object
  if (grepl(char, substrRight(strDate, 3))) {
    date_obj = as.Date(strDate, paste0("%m", char, "%d", char, "%y"))
  } else {
    date_obj = as.Date(strDate, paste0("%m", char, "%d", char, "%Y"))
  }
  
  # Check that the date object is valid
  if (is.na(date_obj)) {
    date_message = paste0("\nERROR: Your ", type, " date is invalid. Please enter a valid ", 
                          type, " date.\n")
    cat(date_message)
    stopQuietly()
  }
  
  return(date_obj)
  
}