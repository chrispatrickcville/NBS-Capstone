infantsFPFN <- function(df) {
  
  # Counts number of abnormals, criticals, unsats, and pass results
  # at the infant level (LINKID) for a set of analytes. Used in 
  # low birth weight analysis/false positive/false negative module.
  
  if (nrow(df)>0) {
    
    # Get the unique LINKIDs for all samples in df
    ids <- unique(df$LINKID)
    
    # Get all samples associated with these LINKIDs and all results from the analyte list
    df_sub <- df[df$LINKID %in% ids, c("LINKID", a_l_list)]
    
    # Reshape data using melt so that all test results are in a single column
    dup <- reshape2::melt(df_sub, id.var="LINKID", variable.name="TEST")
    
    # Group data by LINKIDs and get count of Abnormal and Critical results - 
    # NOTE: this is a very clumsy way of getting what we want for false negatives,
    # so this information should not be used for other analyses. 
    df_grouped <- dup %>%
      group_by(LINKID) %>%
      summarise(
        ABS = sum(grepl("Abnormal", value)),
        CRITS = sum(grepl("Critical", value)),
        UNSATS = sum(grepl("Unsat", value)),
        PASS = sum(!is.na(value) & value=="Pass" | value=="", na.rm=TRUE) + sum(is.na(value))
      )
    
    return(df_grouped)
    
  } else {
    
    return(data.frame(LINKID = integer(0),
                      ABS = integer(0),
                      CRITS = integer(0),
                      UNSATS = integer(0),
                      PASS = integer(0)))
    
  }
  
}