substrRight <- function(x, n) {
  
  # Returns a string of specified length counting backwards
  
  return(substr(x, nchar(x)-n+1, nchar(x)))
  
}