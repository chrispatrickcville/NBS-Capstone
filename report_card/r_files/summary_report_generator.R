# Set a variable to allow us to source main_report_generator.R and diagnosis_report_generator.R
# witout running those reports
summary_report <- TRUE

# If user has entered something other than "RECEIVEDATE" or "BIRTHDATE" for 
# summary_filter, stop the file and report an error
if (summary_filter != "RECEIVEDATE" & summary_filter != "BIRTHDATE") {
  message = addLineBreaks(paste0("\nYou entered '", summary_filter, 
                                 "' for the summary_filter variable. Please change the value for this variable to either 'RECEIVEDATE' or 'BIRTHDATE' to filter the sample data."))
  stop(message)
}

# If user has entered 'BIRTHDATE' for summary_filter, ensure that 
# the user wishes to proceed with this entry (since the output will 
# NOT match the report card data).
if (summary_filter == "BIRTHDATE") {
  cat(addLineBreaks("\nYou entered 'BIRTHDATE' for the summary_filter variable. This will filter the sample data by BIRTHDATE, so the resulting summaries will NOT match the report cards. Do you wish to proceed? (enter 'Y' or 'y' for yes)"))
  ans <- readline(prompt = "> ")
  if (tolower(ans) != 'y') {
    cat("\nStopping per your request.")
    stopQuietly(summary_report)
  } else {
    cat(addLineBreaks("\nContinuing with the report creation using 'BIRTHDATE' as the filter. This will take a few moments.\n"))
  }
}

# Source main_report_generator.R and diagnosis_report_generator.R
report_run <- paste0(wd, slash, "main_report_generator.R")
source(report_run)

diag_run <- paste0(wd, slash, "diagnosis_report_generator.R")
source(diag_run)

##### PREPARE SUMMARY REPORTS - HOSPITAL AND BIRTHCENTER #####

# Write hospital summary report to CSV
if (exists("org_metrics_h")) {
  createSummaryReport(org_metrics_h, diagnoses_h, "hospital")
}

# Write birthcenter summary report to CSV
if (exists("org_metrics_bc")) {
  createSummaryReport(org_metrics_bc, diagnoses_bc, "birthcenter")
}

##### PREPARE SUMMARY REPORT - DIAGNOSES #####

# Test to see if any diagnoses exist for the time period of interest
if (nrow(dd_diag_narr) == 0 & !exists("testing")) {
  
  cat(addLineBreaks("\nWARNING: A summary report for diagnosis counts cannot be created, as there are no diagnoses for the time period of interest.\n"))
  
} else {
  
  # Get all unique disorder/patient combinations for period of interest
  diag_all <- dd_diag_narr %>%
    group_by(DISORDER, LINKID) %>%
    dplyr::summarise()
  
  # Remove all NAs
  diag_all <- diag_all[!is.na(diag_all$LINKID),]
  
  # Get counts of each disorder
  diag_all_count <- diag_all %>%
    dplyr::summarise(`Total Count`=n())
  
  # Join full list of disorders to count of disorders
  diag <- as.data.frame(diag_narr$Disorder, stringsAsFactors = FALSE)
  names(diag) <- "DISORDER"
  diag <- left_join(diag, diag_all_count, by="DISORDER")
  
  # replace NAs with 0s
  diag[is.na(diag)] <- 0
  
  # Publish diagnosis summary to admin folder (if not testing)
  if (!exists("testing")) {
    write.csv(diag, paste0(admin_path, slash, "diagnosis_summary for period ", 
                           start_date, " to ", end_date, ".csv"))
  }
  
}

##########################################
##### PREPARE SUMMARY REPORT - STATE #####
##########################################

### Hospital Stats: columns 1 and 2 ###

# Check to see if there is any data for hospitals; if so, get 
# statistics for hospitals, and if not, create vectors of NAs

if (!exists("state_h")) {
  
  state_summary <- data.frame(matrix(rep(NA, 30), nrow=1, ncol=30))
  state_h_samp <- data.frame(matrix(rep(NA, 30), nrow=1, ncol=30))
  
} else {
  
  # Prep Column 1: HOSPITALS ONLY: Averaged over *hospitals*
  
  state_summary <- state_h
  
  # find counts of each unsat category for hospital submitters
  state_h_unsats <- as.data.frame(org_metrics_hosp[2], check.names=FALSE) %>%
    replace(is.na(.), 0) %>%
    summarise_at(vars(-SUBMITTERNAME), funs(sum))
  
  # add unsat counts to state summary
  state_summary <- cbind(state_summary, state_h_unsats)
  
  # Prep Column 2: HOSPITALS ONLY: Averaged over *samples*
  state_h_samp <- getStateMetricsOverSamples(dd_h, transfused=TRUE)
  
  # Add additional columns
  state_h_samp <- cbind(tot_sub_h, state_h_samp[1:8], state_summary[10:11], 
                        state_h_samp[9:14], state_h_unsats)
  
}

### BirthCenter Stats: columns 3 and 4 ###

# Check to see if there is any data for birthcenters; if so, get 
# statistics for birthcenters, and if not, create 2 rows of NAs

if (!exists("state_bc")) {
  
  state_bc_org <- data.frame(matrix(rep(NA, 30), nrow=1, ncol=30))
  state_bc_samp <- data.frame(matrix(rep(NA, 30), nrow=1, ncol=30))
  
} else {
  
  # Prep Column 3: BIRTHCENTERS ONLY: Averaged over *birthcenters*
  state_bc_org <- getStateMetricsOverOrgs(org_metrics_bc, nrow(org_metrics_bc), transfused=FALSE)
  
  # find counts of each unsat category for birthcenter submitters
  state_bc_unsats <- as.data.frame(org_metrics_birthcenter[2], check.names=FALSE) %>%
    replace(is.na(.), 0) %>%
    summarise_at(vars(-SUBMITTERNAME), funs(sum))
  
  # Add additional columns and unsats to state_bc_org
  state_bc_org <- cbind(state_bc_org[1:13], NA, NA, state_bc_org[14:15], state_bc_unsats)
  
  # Prep Column 4: BIRTHCENTERS ONLY: Averaged over *samples*
  state_bc_samp <- getStateMetricsOverSamples(dd_bc, transfused=FALSE)
  
  # Add additional columns
  state_bc_samp <- cbind(state_bc_org[1], state_bc_samp[1:8], state_bc_org[10:11], state_bc_samp[9:10], NA, NA, 
                         state_bc_samp[11:12], state_bc_unsats)
  
}

### Stats for All Submitters: columns 5 and 6 ###

# Prep Column 5: ALL SUBMITTERS: Averaged over *submitters*

# remove from dd any records missing SUBMITTERID
dd_filt <- dd %>%
  filter(SUBMITTERID != "" | !is.na(SUBMITTERID))

# get count of submitters, including hospital, birthcenter and organizations
# that do not fit into either category (count all the unique IDs in data, subtract 
# the number of IDs that appear in both the submitters CSV and the data - since
# some organizations have multiple IDs - then add back in the number of organizations 
# whose names appear in both submitters and dd_filt)
all_sub <- length(unique(dd_filt$SUBMITTERID)) - 
  length(intersect(dd_filt$SUBMITTERID, submitters$SUBMITTERID)) + 
  length(intersect(submitters$HOSPITALREPORT, dd_filt$SUBMITTERNAME))

# filter data for submitters not in VA NBS file
others <- dd_filt %>%
  filter(!(SUBMITTERID %in% submitters$SUBMITTERID))

# get metrics for others 
other_metrics <- getOrgMetrics(others, group_by="SUBMITTERID")
all_sub_metrics <- as.data.frame(other_metrics[1], check.names=FALSE)

# Bind non-hospital submitter summaries with hospital and birthcenter summaries
# (if they exist)
if (nrow(all_sub_metrics) > 0) {
  all_sub_metrics$SUBMITTERNAME <- NA
}
all_sub_metrics <- all_sub_metrics[,c(ncol(all_sub_metrics),2:(ncol(all_sub_metrics) - 1))]
all_sub_metrics <- rbind(all_sub_metrics, if(exists("org_metrics_h")) org_metrics_h, 
                         if(exists("org_metrics_bc")) org_metrics_bc)

# Get state averages across all submitters (hospital, birthcenter, and others)
state_all_sub <- getStateMetricsOverOrgs(all_sub_metrics, nrow(all_sub_metrics), transfused=TRUE)

# Get counts of unsats by category for all submitters
unsat_all_counts <- data.frame(t(colSums(all_sub_metrics[, as.character(unsats$description)], na.rm=TRUE)), 
                               check.names=FALSE)

# Join state_all_sub to unsat_all_counts
state_all_sub <- cbind(state_all_sub, unsat_all_counts)

###### COLUMN 6 PREP: ALL SUBMITTERS: Averaged over *samples*

# Get averages for state over samples
state_all_samp <- getStateMetricsOverSamples(dd, transfused=TRUE)

# Add additional columns
state_all_samp <- cbind(state_all_sub[1], state_all_samp[1:8], state_all_sub[10:11], state_all_samp[9:14], 
                        unsat_all_counts)

###### BIND COLUMNS AND RENAME

# Create list of dfs for hospitals and birthcenters
dfs <- list(state_summary, state_h_samp, state_bc_org, state_bc_samp, state_all_sub, state_all_samp)

# Create vector of column names
state_cols <- c("Number of Submitters","Sample Count","Avg. Transit Time","Min. Transit Time",
                "Max. Transit Time",">= 4 Days in Transit", "% >= 4 Days in Transit",
                "Received within 2 Days","% Received within 2 Days",
                "Number of Submitters Meeting 95% of Samples Received within 2 Days Goal",
                "% Submitters Meeting 95% of Samples within 2 Days Goal",
                "< 24 Hours","% < 24 Hours","Transfused", "% Transfused", 
                "Unsat Count", "Unsat %", 
                paste("Unsat:", unlist(as.list(as.character(unsats$description), sorted = FALSE))))

# Rename column names of dfs to state_cols
dfs <- lapply(dfs, setNames, state_cols)

# Bind all of the dataframes in the list
state_summary <- data.frame(do.call("rbind", dfs))

# Transform table and rename columns
state_summary <- as.data.frame(t(state_summary))
names(state_summary) <- c("HOSPITALS ONLY: Averaged over hospitals",
                          "HOSPITALS ONLY: Averaged over samples",
                          "BIRTH CENTERS ONLY: Averaged over birth centers",
                          "BIRTH CENTERS ONLY: Averaged over samples",
                          "ALL SUBMITTERS: Averaged over submitters",
                          "ALL SUBMITTERS: Averaged over samples")

# Rename rows
rownames(state_summary) <- state_cols

# Write reports to csv if not running testing
if (!exists("testing")) {
  
  # Publish state summary to admin folder
  write.csv(state_summary, paste0(admin_path, slash, "state_summary_by_", filt_col, 
                                  " for period ", start_date, " to ", end_date, ".csv"))
  
  # Prepare outlier transit time report
  time_outliers <- initial_dd %>%
    left_join(dd[, c("SAMPLEID", "TRANSIT_TIME")], by="SAMPLEID") %>%
    filter(TRANSIT_TIME > 10, eval(parse(text = filt_col)) >= start_date & 
             eval(parse(text = filt_col)) <= end_date) %>%
    arrange(desc(TRANSIT_TIME))
  
  write.csv(time_outliers, paste0(admin_path, slash, "transit_time_outliers_by_", filt_col,
                                  " for period ", start_date, " to ", end_date, ".csv"))
  
  # Indicate that reports are complete
  cat("\nAll summary reports that were able to be generated are now saved to your output folder.")
  
}

# Remove summary_report
rm(summary_report)
