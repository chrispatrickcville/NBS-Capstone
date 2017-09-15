getBreaksAndLabels <- function(df) {
  
  # Returns dataframe with breaks added for use in mapping,
  # a test dataframe so counts can be compared with break
  # points, and a vector of labels to be used in the
  # legend (to replace the breakpoints in the data)
  
  # Determine number of unique values in value column
  unique_vals <- unique(df$value)
  
  # Set number of subsets for mapping based on the number
  # of unique values in data. If there are less than 6
  # unique values, set n to be equal to the number of 
  # unique values present in the data. Otherwise, set n to be 
  # 6 (so the maximum of categories will be 6).
  if (length(unique_vals) < 6) {
    n <- length(unique_vals)
  } else {
    n <- 6    
  }
  
  # Order the unique values for sequencing the breaks
  x <- unique_vals[order(unique_vals)]
  
  # Get chunks
  chunks <- chunk(x,n)
  
  # Find cutoff values for each chunk
  max_vals <- c()
  for (i in 1:length(chunks)) {
    assign(paste0("max", i), checkInf(max(as.numeric(unlist(chunks[i])))))
    max_vals <- c(max_vals, eval(parse(text=paste0("max", i))))
  }
  
  # Get unique values to create breaks
  all_breaks <- unique(c(0, max_vals))
  
  # Create copy of the data with both value and break_value for testing
  test_df <- df
  
  # Create cuts in data
  df$value <- cut(df$value, breaks=all_breaks, right = TRUE, include.lowest = FALSE)
  
  # Add this into test_copy as break_value for comparison testing
  test_df$break_value <- df$value
  
  # Get labels for map (revise cut points to be more viewer-friendly)
  if (n == 6) {
    old_labels <- levels(df$value)[levels(df$value) %in% as.character(df$value)]
    labels <- c()
    for (l in old_labels) {
      min_val <- min(test_df$value[as.character(test_df$break_value)==l], na.rm=TRUE)
      max_val <- max(test_df$value[as.character(test_df$break_value)==l], na.rm=TRUE)
      if (min_val == max_val) {
        new_val = as.character(min_val)
      } else {
        new_val = paste0(min_val, "-", max_val)
      }
      labels <- c(labels, new_val)
    } 
  } else {
    labels = x
  }
  
  # Add labels to test_df and remove break_value
  test_df$map_label <- labels[unlist(lapply(test_df$break_value, function(x) which(levels(x)==x)))]
  test_df$break_value <- NULL

  return_objs <- list(df, test_df, labels)
  return(return_objs)
  
}