stopQuietly <- function(remove_ls=NULL) {
  
  # Stops a source file quietly (without printing an error message), used in cases
  # where we have multiple files that need to stop running, but only have one of them
  # throw an error.
  
  # Input:
  #   rm - variable or list of variables that should be removed when stopQuietly
  #        is used (for example, summary_report should be removed when using 
  #        this function in the report card module, as the code works differently
  #        when summary_report exists)
  
  if (!is.null(remove_ls)) {
    suppressWarnings(rm(list=remove_ls, pos = ".GlobalEnv"))
  }
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
  
} 