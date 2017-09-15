checkString <- function(cell_to_check, check_set) {
  
  # Given a cell to check, returns 1 if any substring in that cell matches an element
  # in a set of values. Assumes that the check_cell has values separated by
  # commas (e.g., T4,TSH,CAH). Used to see whether a sample in the call log 
  # has any analyte listed for a particular category of disorders.
  
  val = ifelse(any(unlist(strsplit(cell_to_check, split=",")) %in% check_set), 1, 0)
  return(val)
  
}