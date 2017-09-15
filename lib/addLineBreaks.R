addLineBreaks <- function(str, char_count=75) {
  
  # Adds newline characters in place of spaces to a string to allow a console
  # message to be more easily readable
  
  # Find indices for all spaces
  all_spaces = unlist(gregexpr(pattern =' ', str))
  
  # Initialize search_val
  search_val = char_count
  
  # Continue moving through string until get to the end
  suppressWarnings(while (search_val < nchar(str)) {
    
    # Get distance from each index to the desired location
    vals = abs(all_spaces - search_val)
    
    # Find index for minimum distance
    best_val = which(vals==min(vals))
    
    # Find the index for the space in the string that matches the ideal location
    best_in = all_spaces[best_val]
    
    # Replace space at the best_in location with a newline character
    substr(str, best_in, best_in) <- "\n"
    
    # Move the search_val forward from the best_in to the desired length
    # of the line
    search_val = best_in + char_count
    
  })
  
  return(str)
  
}