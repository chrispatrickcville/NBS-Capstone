checkCol <- function(df, colString) {
  
  # Given a dataframe and a column expressed as a string, perform checkString on a
  # set of values of interest and add a new column with the results of the check.
  # Used to add the results from checkString across an entire column.
  
  vals <- apply(df, 1, function (x) checkString(x, eval(parse(text = colString))))
  return(vals)
  
}