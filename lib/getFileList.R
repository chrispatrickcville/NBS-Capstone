getFileList <- function(folder) {
  
  # Returns list of .txt or .csv files in a folder.
  # Requires that 'slash' character (indicating the
  # character separating folders from file names)
  # is created outside of the function.
  
  files <- list.files(folder, pattern = "csv|txt")
  temp <- paste0(folder, slash, files)
  
  return(temp)
  
}