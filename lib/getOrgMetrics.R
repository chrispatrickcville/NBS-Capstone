getOrgMetrics <- function(df, group_by="SUBMITTERNAME", rankings=TRUE, unsat_cats=TRUE) {
  
  # Returns dataframe of metrics for submitters for use in report cards (includes
  # rank and unsat counts). Default group_by column is SUBMITTERNAME but user can
  # select a different column
  
  cols = c(group_by, "TRANSIT_TIME", "COLLECTIONDATE", "COLLECTIONTIME", "RECEIVEDATE",
           "RECEIVETIME", "BIRTHDATE", "BIRTHTIME", "UNSATCODE", "TRANSFUSED",
           "TIME_TO_COLL")
  
  temp_df = df %>%
    group_by_at(vars(one_of(group_by))) %>%
    select(one_of(cols)) %>%
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
      met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0),
      col_less_than_24_hours = checkInf(sum(!is.na(TIME_TO_COLL) & TIME_TO_COLL < 1 & !is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff)),
      percent_less_than_24_hours = checkInf(round(col_less_than_24_hours/sum(!is.na(TIME_TO_COLL) & 
                                                                               !is.na(TRANSIT_TIME) & 
                                                                               TRANSIT_TIME >= cutoff) * 100, 2)),
      trans = checkInf(sum(TRANSFUSED == 'Y', na.rm=TRUE)),
      trans_percent = checkInf(round(trans/total_samples * 100, 2)),
      unsat_count = checkInf(sum(!is.na(UNSATCODE))),
      unsat_percent = checkInf(round(unsat_count/total_samples * 100, 2)))
  
  ##### ADD RANKINGS #####
  
  if (rankings) {
    
    # Rank submitters by mean transit time (ascending order; e.g., least mean transit time = #1)
    temp_df$rank_transit = rank(temp_df$avg_transit_time, na.last="keep", ties.method="min")
    
    # Rank submitters by transit time > 4 (ascending order; e.g., least number of samples over
    # 4 days in transite = #1)
    temp_df$rank_transit_over_4 = rank(temp_df$percent_over_4, na.last="keep", ties.method="min")
    
    # Rank submitters by percentage of samples recevied within 2 days (descending order; e.g.,
    # greatest percentage of samples received by target time = #1)
    temp_df$rank_percent_within_goal = rank(-temp_df$percent_rec_in_2_days, na.last="keep", ties.method="min")
    
    # Rank submitters  by number of samples collected at less than 24 hours of age (ascending order;
    # e.g., least number of early collections = #1)
    temp_df$rank_early_collection = rank(temp_df$percent_less_than_24_hours, na.last="keep", ties.method="min")
    
    # Rank submitters by number of samples transfused prior to collection (ascending order;
    # e.g., least number of early collections = #1)
    temp_df$rank_transfused = rank(temp_df$trans_percent, na.last="keep", ties.method="min")
    
    # Rank ubmitters by number of unsatisfactory samples (ascending order; e.g., least number of unsats = #1)
    temp_df$rank_unsats = rank(temp_df$unsat_percent, na.last="keep", ties.method="min")
    
  }
  
  ##### ADD UNSAT COUNTS #####
  
  if (unsat_cats) {
    
    # get count of unsat codes for each submitter
    unsat_prep = df[!is.na(df$UNSATCODE),] %>% 
      group_by_(group_by, "UNSATCODE") %>%
      dplyr::summarise(count = n())  
    colnames(unsat_prep) = c("ORG", "UNSATCODE", "count")
    
    # get all possibilities for unsat codes
    unsat_seq = seq(1:nrow(unsats))
    
    # create cross join of all possible unsat codes and all submitter names
    if (nrow(unsat_prep) > 0) {
      
      cross_join = CJ(ORG=unique(temp_df[[group_by]]), UNSATCODE=unsat_seq)
      
      # create left join of unsat counts and cross_join (so we have NAs
      # for each submitter that has no unsats for that particular code)
      unsat_amts = left_join(cross_join, unsat_prep, by=c("ORG", "UNSATCODE"))
      
      # replace UNSATCODE column with 'col' column
      unsat_amts$col = paste("unsat_", str_pad(unsat_amts$UNSATCODE, 2, pad="0"), sep="")
      unsat_amts$UNSATCODE = NULL
      
      # reshape dataframe to have rows as SUBMITTERNAME and columns as col (e.g., unsat_01, unsat_02, etc.)
      unsats_ready = dcast(unsat_amts, ORG ~ col, value.var="count")
      
      # replace column names (unsat_01, etc.) with unsat descriptions
      names(unsats_ready) = c(group_by, unlist(as.list(as.character(unsats$description), sorted = FALSE)))
      
      # left join unsats_ready and temp_df
      temp_df <- left_join(temp_df, unsats_ready, by=group_by)
      
      # otherwise, there are no unsats, so bind the unsats descriptions without any values to temp_df
    } else {
      
      # Create empty dataframe with number of columns equal to the number of unsat descriptions
      unsats_ready <- data.frame(matrix(nrow = nrow(temp_df), ncol = length(unsats$description) + 1))
      
      # Change the column names to match the unsats descriptions
      names(unsats_ready) <- c("SUBMITTERNAME", as.character(unsats$description))
      
      # Bind the columns from unsats_ready to temp_df
      temp_df <- cbind(temp_df, unsats_ready[, 2:ncol(unsats_ready)])
      
    }
    
    return(list(temp_df, unsats_ready))  
    
  }
  
  return(temp_df)
  
}