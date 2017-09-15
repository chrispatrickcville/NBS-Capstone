createSummaryReport <- function(org_df, diagnosis_df, type) {
  
  # Writes summary for organization of interest to csv (used for hospital and birthcenter summaries)
  
  # org_df = dataframe of sample information for organizations of interest
  # diagnosis_df = dataframe of diagnoses for organizations of interest
  # type = either "hospital" or "birthcenter"
  
  # Stop if org_df does not exist and not running test
  if (!exists(deparse(substitute(org_df))) & !exists("testing")) {
    
    message = addLineBreaks(paste0("\nWARNING: A summary report for ", type, 
                                   "s cannot be created, as there are no samples for ", type, 
                                   "s for the time period of interest. In addition, the state summary report will be run without metrics for ",
                                   type, "s.\n"))
    cat(message)
    
  } else {
    
    # create vector for IDs for organizations of interest
    IDs = c()
    
    # get all submitter IDs for each organization of interest
    for (i in 1:nrow(org_df)) {
      IDs[i] <- paste(submitters[submitters$HOSPITALREPORT 
                                 %in% org_df$SUBMITTERNAME[i],]$SUBMITTERID, collapse="; ")
    }
    
    # reorganize columns for report
    org_summary = org_df[, c("SUBMITTERNAME", "total_samples", "avg_transit_time",
                             "rank_transit", "min_transit_time", "max_transit_time",
                             "rec_in_2_days", "percent_rec_in_2_days", "rank_percent_within_goal",
                             "met_goal", "transit_over_4", "percent_over_4", "rank_transit_over_4",
                             "col_less_than_24_hours", "percent_less_than_24_hours", "rank_early_collection",
                             "trans", "trans_percent", "rank_transfused", "unsat_count", "unsat_percent",
                             "rank_unsats", "Improperly Collected", "Scratched or Abraded", "Wet", "Oversaturated",                  
                             "Contaminated", "No Blood", "Insufficient Information", "> 10 days in transit",       
                             "Infant > 6 months old", "Outdated filter paper card", "Insufficient Quantity",         
                             "Interfering Substances Present", "Other")]
    
    # add submitter IDs
    org_summary = cbind(as.data.frame(IDs), org_summary)
    
    # determine number of diagnoses for each organization
    
    # if the number of diagnoses is greater than 0, join diagnoses on organizations;
    # otherwise, create a vector of 0 values for diag_count
    if (nrow(diagnosis_df) > 0) {
      diag_count = diagnosis_df %>%
        group_by(SUBMITTERNAME) %>%
        dplyr::summarise(
          total=sum(Count))
      
      # add diagnosis count to hosp_summary
      org_summary = left_join(org_summary, diag_count, by="SUBMITTERNAME")
      
    } else {
      org_summary$total <- 0
    }
    
    # Rename columns
    org_summary_cols = c("Submitter IDs", "Submitter Name", "Sample Count", "Avg. Transit Time", "Rank: Transit Time",
                         "Min. Transit Time", "Max. Transit Time", "Received within 2 Days", 
                         "% Received within 2 Days","Rank: Received within 2 Days",
                         "Met 95% of Samples Received within 2 Days Goal?", "Samples >= 4 Days in Transit",
                         "% Over 4 Days in Transit", "Rank: Over 4 Days in Transit", "< 24 Hours", 
                         "% < 24 Hours", "Rank: < 24 Hours", "Transfused", "% Transfused", 
                         "Rank: Transfused", "Unsat Count", "Unsat %", "Rank: Unsats", 
                         paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))),
                         "Diagnosis Count")
    names(org_summary) = org_summary_cols
    
    # Replace NAs with 0s
    org_summary[is.na(org_summary)] = 0
    
    # Change 0s to 'NO' and 1s to 'YES' for met goal
    org_summary$`Met 95% of Samples Received within 2 Days Goal?` <- 
      ifelse(org_summary$`Met 95% of Samples Received within 2 Days Goal?` == 0, 'no', 'yes')
    
    # Remove columns relating to transfusion if type === "birthcenter"
    if (type == "birthcenter") {
      org_summary <- subset(org_summary, select=-c(Transfused,`% Transfused`,`Rank: Transfused`))
    }
    
    # Write to csv (if not testing)
    if (!exists("testing")) {
      write.csv(org_summary, paste0(admin_path, slash, type, "_summary_by_", filt_col, 
                                    " for period ", start_date, " to ", end_date, ".csv"))
    }
    
  }
  
}