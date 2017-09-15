getStateMetricsOverSamples <- function(df, transfused) {
  
  # Get state metrics averaged over samples. Takes dataframe and transfused = TRUE or FALSE to indicate
  # whether transfusion information should be included
  
  temp_df = df %>%
    select(TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, RECEIVEDATE, BIRTHDATE, BIRTHTIME, 
           TRANSFUSED, UNSATCODE, TIME_TO_COLL) %>%
    dplyr::summarise(
      total_samples=n(),
      avg_transit_time = checkInf(round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2)),
      min_transit_time = checkInf(round(min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2)),
      max_transit_time = checkInf(round(max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2)),
      transit_over_4 = sum((!is.na(TRANSIT_TIME) & TRANSIT_TIME >= 4) | 
                             (is.na(TRANSIT_TIME) & !is.na(RECEIVEDATE) 
                              & !is.na(COLLECTIONDATE) & RECEIVEDATE - COLLECTIONDATE > 4)),
      percent_over_4 = checkInf(round((transit_over_4 / 
                                         sum((!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) | 
                                               (is.na(TRANSIT_TIME) & !is.na(RECEIVEDATE) & 
                                                  !is.na(COLLECTIONDATE) & RECEIVEDATE - COLLECTIONDATE > 4))) * 100, 2)),
      rec_in_2_days = checkInf(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)),
      percent_rec_in_2_days = checkInf(round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                               sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2)),
      col_less_than_24_hours = checkInf(sum(!is.na(TIME_TO_COLL) & TIME_TO_COLL < 1 & !is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff)),
      percent_less_than_24_hours = checkInf(round(col_less_than_24_hours/sum(!is.na(TIME_TO_COLL) & 
                                                                               !is.na(TRANSIT_TIME) & 
                                                                               TRANSIT_TIME >= cutoff) * 100, 2)),
      trans = sum(TRANSFUSED == 'Y'),
      trans_percent = checkInf(round(trans/total_samples * 100, 2)),
      unsat_count = sum(!is.na(UNSATCODE)),
      unsat_percent = checkInf(round(unsat_count/total_samples * 100, 2))
    )
  
  if (transfused == FALSE) {
    
    temp_df = select(temp_df, -c(trans, trans_percent))
    
  }
  
  return(temp_df)
  
}