chunk <- function(x, n) {
  
  # Splits the sequence of numbers x by the value in n, 
  # used for establishing break points in data for 
  # mapping
  
  # Inputs:
  #   x - sequence of numbers from min value in data to max value in data
  #   n - number of chunks to create
  
  return(split(x, sort(rank(x) %% n)))
  
}