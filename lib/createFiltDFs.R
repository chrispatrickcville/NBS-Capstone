createFiltDFs <- function(df, type=c("sample","diagnosis"), s_date=start_date, e_date=end_date, period=line_chart) {
  
  # Given a dataframe, start date, and end date, returns 2 data frames filtered by
  # start date and end date:
  #   1) period_df - filtered by period of interest (using start_date and end_date)
  #   2) year_df - filtered for one year prior to end_date (using end of period, either the month or 
  #      the quarter (depending on line_chart value)
  
  # Define column that will be used to filter data
  filt_col <- ifelse(type == "sample", filt_col, "DIAGNOSISDATE")
  
  # Determine end date for period, depending on whether line_plot
  # is defined as "monthly" or "quarterly"
  period_end <- as.Date(ifelse(period == "quarterly", as.Date(as.yearqtr(e_date), frac=1), 
                               as.Date(as.yearmon(e_date), frac=1)))
  
  # Filter df to include one year of data (dated backwards from end date)
  year_df <- df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") > (period_end - years(1)) 
                   & as.Date(filt_col, format="%m/%d/%Y") <= period_end,
                   filt_col=as.name(filt_col)))
  
  # Report and repair differences in duplicated SAMPLEIDs (if type=="sample")
  # Also, calculate TRANSIT_TIME and TIME_TO_COLL
  if (type=="sample") {
    
    # Report and repair differences in duplicated SAMPLELIDs
    sample_cols_RARD <- colnames(year_df)[!colnames(year_df) %in% c("SAMPLEID", "SUBMITTERNAME", "SUBMITTERID", 
                                                          "HOSPITALREPORT", "TYPE")]
    year_df <- reportAndRepairDifferences(df=year_df, id="SAMPLEID", type=type, module="Report Card",
                                          cols=sample_cols_RARD)
    
    # Calculate TIME_TO_COLL (birthdate/time to collectiondate/time)
    year_df <- timeDiff(year_df, "BIRTHDATE", "BIRTHTIME", "COLLECTIONDATE", "COLLECTIONTIME",
                        "BIRTH", "COLLECTION", "TIME_TO_COLL")
    
    # Calculate TRANSIT_TIME (collectiondate/time to receivedate/time)
    year_df <- timeDiff(year_df, "COLLECTIONDATE", "COLLECTIONTIME", "RECEIVEDATE", "RECEIVETIME",
                        "COLLECTION", "RECEIVE", "TRANSIT_TIME")
    
    
    # For any remaining cases of TIME_TO_COLL not being NA where there is no TRANSIT_TIME 
    # but there is a RECEIVEDATE, TIME_TO_COLL should be changed to NA, as the absence of 
    # TRANSIT_TIME in the presence of RECEIVEDATE indicates that the TRANSIT_TIME calculation
    # was negative, meaning that COLLECTIONDATE cannot be trusted.
    year_df$TIME_TO_COLL <- ifelse(is.na(year_df$TRANSIT_TIME) &
                                     !is.na(year_df$RECEIVE), NA, year_df$TIME_TO_COLL)
    
    # For any remaining cases of TRANSIT_TIME not being NA where there is no TIME_TO_COLL
    # but there is a BIRTHDATE, TRANSIT_TIME should be changed to NA, as the absence of 
    # TIME_TO_COLL in the presence of BIRTHDATE indicates that the TIME_TO_COLL calculation
    # was negative, meaning that COLLECTIONDATE cannot be trusted.
    year_df$TRANSIT_TIME <- ifelse(is.na(year_df$TIME_TO_COLL) &
                                     !is.na(year_df$BIRTH), NA, year_df$TRANSIT_TIME)
    
  }
  
  # Filter year_df by start_date and end_date
  period_df <- year_df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") >= s_date
                   & as.Date(filt_col, format="%m/%d/%Y") <= e_date,
                   filt_col=as.name(filt_col)))
  
  # Report and repair differences in duplicated SAMPLEIDs (if type=="diagnosis")
  if (type=="diagnosis") {
    diag_cols_RARD <- "DIAGNOSISDATE"
    diag_cols_eval_RARD <- c("DIAGNOSIS", "SUBMITTERID", "LINKID")
    period_df <- reportAndRepairDifferences(df=period_df, id="SAMPLEID", type=type, module="Report Card",
                                               cols=diag_cols_RARD, eval_cols=diag_cols_eval_RARD)
  }
  
  # Add period information to year dataframe if type == "sample"
  if (type == "sample") {
    if (period == "quarterly") {
      year_df$PERIOD <- as.yearqtr(year_df[[filt_col]], format="%Y%m")
    } else {
      year_df$PERIOD  <- as.yearmon(year_df[[filt_col]], format="%Y%m")
    }
  }
  
  return(list(period_df, year_df))
  
}
