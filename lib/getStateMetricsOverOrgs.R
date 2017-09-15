getStateMetricsOverOrgs <- function(df, tot_orgs, transfused, group_by=NULL) {
  
  # Get state metrics averaged over organizations (e.g., hospital or birthcenter). 
  # Takes dataframe of summarized data for each organization and transfused = 
  # TRUE or FALSE to indicate whether transfusion information should be included
  
  if (!is.null(group_by)) {
    df = df %>%
      group_by_at(vars(one_of(group_by)))
  }
  
  temp_df = df %>%
    dplyr::summarise(
      submitters = nrow(df),
      total_samples=sum(total_samples, na.rm=TRUE),
      avg_transit_time = checkInf(round(mean(avg_transit_time, na.rm=TRUE), 2)),
      min_transit_time = checkInf(min(min_transit_time, na.rm=TRUE)),
      max_transit_time = checkInf(max(max_transit_time, na.rm=TRUE)),
      transit_over_4 = sum(transit_over_4, na.rm=TRUE),
      percent_over_4 = checkInf(round(mean(percent_over_4, na.rm=TRUE), 2)),
      rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
      percent_rec_in_2_days = round(mean(percent_rec_in_2_days, na.rm=TRUE), 2),
      met_goal = sum(met_goal, na.rm=TRUE),
      percent_met_goal = round((met_goal / tot_orgs) * 100, 2),
      col_less_than_24_hours = sum(col_less_than_24_hours, na.rm=TRUE),
      percent_less_than_24_hours = round(mean(percent_less_than_24_hours, na.rm=TRUE), 2),
      trans = sum(trans, na.rm=TRUE),
      trans_percent = round(mean(trans_percent, na.rm=TRUE), 2),
      unsat_count = sum(unsat_count, na.rm=TRUE),
      unsat_percent = round(mean(unsat_percent, na.rm=TRUE), 2)
    )
  
  if (transfused == FALSE) {
    
    temp_df = select(temp_df, -c(trans, trans_percent))
    
  }
  
  return(temp_df)
  
}