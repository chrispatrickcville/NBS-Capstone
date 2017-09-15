trim <- function (x) {
  
  # Remove spaces at the end or the beginning of any elements
  gsub("^\\s+|\\s+$", "", x)
  
}
