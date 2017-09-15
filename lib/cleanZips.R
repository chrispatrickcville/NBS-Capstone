cleanZips <- function(df, zip_col) {
  
  # Cleans and converts zip codes for a dataframe
  
  # Arguments:
  #   df      - dataframe with zip code column to be converted
  #   zip_col - column with zip codes
  
  # Clean zips for mapping (using clean.zipcodes from zipcode package)
  df[, zip_col] <- clean.zipcodes(df[, zip_col])
  
  # If any zip codes have more than 4 digits, trim to only include first 5
  df[, zip_col] <- substr(df[, zip_col], 1, 5) 
  
  # Convert P.O. Box Zip Codes to the surrounding area zip code (necessary
  # for choroplethZip package, which does not have these zip codes in its
  # data source, and so they can't be mapped as is)
  df <- convertPOZips(df=df, zip_col=zip_col)
  
  # Return df
  return(df)
  
}