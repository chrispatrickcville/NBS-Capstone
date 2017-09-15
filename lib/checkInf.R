checkInf <- function(FUN) {
  
  # returns the value of the function if not infinite, otherwise returns NA
  
  suppressWarnings(return(ifelse(is.infinite(FUN) | is.nan(FUN), NA, FUN)))
  
}