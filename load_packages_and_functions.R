# Load packages and functions for Newborn Screening Hospital Reporting
# Do not access this file directly; "run_file_and_variable_setting.R"
# will automatically run this file.

libs <- c('xtable',
          'data.table',
          'stringr',
          'reshape2',
          'knitr',
          'markdown',
          'zoo',
          'ggplot2',
          'grid',
          'reshape',
          'lubridate',
          'dplyr',
          'pander',
          'shiny',
          'lazyeval',
          'toOrdinal',
          'readxl',
          'rmarkdown')

for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}

# Install RDCOMClient if running on a PC (will not work on 
# Mac as built for older version of R)
if (exists("comp_type") && comp_type == 'PC') {
  if(!is.element('RDCOMClient', .packages(all.available = TRUE)) ) {
    install.packages('RDCOMClient')
  }
  suppressPackageStartupMessages(library('RDCOMClient', character.only=TRUE))
  
}

# Reformat start date and end date as dates
if (exists("start_date")) {
  start_date <- as.Date(start_date, "%m/%d/%Y")
  end_date <- as.Date(end_date, "%m/%d/%Y")
}

check_inf <- function(FUN) {
  
  # returns the value of the function if not infinite, otherwise returns NA
  suppressWarnings(return(ifelse(is.infinite(FUN), NA, FUN)))
  
}

get_file_list <- function(folder) {
  
  # Returns list of files in a folder
  
  # get list of files within folder
  files <- list.files(folder)
  temp <- paste0(folder, slash, files)
  
  return(temp)
  
}

get_file_extension <- function(folder) {
  
  # Returns the file extension of files in a folder. Assumes that all files have
  # the same extension, so do not use with folders that have multiple types of files.
  
  # get list of files within folder
  files <- get_file_list(folder)
  
  # find out what type of data files we have by getting the file (assumes all data files are same file type)
  data_type <- substr(files[1], as.numeric(regexpr("\\.([[:alnum:]]+)$", files[1])[1]), nchar(files[1]))
  
  # return file extension
  return(data_type)
  
}

read_data <- function(folder, ...) {
  
  # Returns dataframe of data. Optional arguments are columns to be reformatted as dates
  # (for use with csv and txt files).
  
  # Make list of columns to be reformatted as dates
  date_reformat = list(...)
  
  # Get file list
  temp <- get_file_list(folder)
  
  # Get file extension
  ext <- get_file_extension(folder)
  
  # read in data using different methods depending on what type of data files we have (e.g., .xls vs. .txt)
  lst <- lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=separator, fileEncoding="latin1"))
  initial_dd <- rbindlist(lst, fill=TRUE)
  
  # read in data second time in order to get character vector of SUBMITTERID
  sub_lst <- lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=separator, fileEncoding="latin1",
                                               colClasses="character"))
  
  sub_dd <- rbindlist(sub_lst, fill=TRUE)
  
  # replace initial_dd SAMPLEID with sub_dd version (to keep leading zeros)
  if ("SAMPLEID" %in% names(initial_dd)) {
    initial_dd$SAMPLEID <- as.character(sub_dd$SAMPLEID)
  }
  
  # Reformat any specified columns as dates
  if (!is.null(date_reformat)) {
    for (i in date_reformat) {
      if (class(initial_dd[,eval(i)][[1]]) != "Date") {
        initial_dd[[i]] <- as.Date(initial_dd[[i]], "%m/%d/%Y", origin = "1900-01-01")
      }
    }
  }
  
  # Reformat SUBMITTERID as character
  if("SUBMITTERID" %in% names(initial_dd)) {
    initial_dd$SUBMITTERID <- as.character(initial_dd$SUBMITTERID)
  }
  
  # Replace 9999 values in transit time column with NA
  if("TRANSITTIME" %in% names(initial_dd)) {
    initial_dd$TRANSIT_TIME[initial_dd$TRANSIT_TIME == 9999] <- NA
  }
  
  # If dataframe has CATEGORY column, remove any records that have category listed as "Proficiency", 
  # "Treatment", or "Treatment - PKU"
  remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
  if (!is.null(initial_dd$CATEGORY)) {initial_dd <- initial_dd[!(initial_dd$CATEGORY %in% remove_cats),]}
  
  return(initial_dd)
  
}

create_filt_dfs <- function(df, type=c("sample","diagnosis"), s_date=start_date, e_date=end_date, period=line_chart) {
  
  # Given a dataframe, start date, and end date, returns 2 data frames filtered by
  # start date and end date:
  #   1) period_df - filtered by period of interest (using start_date and end_date)
  #   2) year_df - filtered for one year prior to end_date (using end of period, either the month or 
  #      the quarter (depending on line_chart value)
  
  # Define column that will be used to filter data
  filt_col <- ifelse(type == "sample", "BIRTHDATE", "DIAGNOSISDATE")
  
  # Obtain first dataframe, filtering data by start_date and end_date
  period_df <- df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") >= s_date
                   & as.Date(filt_col, format="%m/%d/%Y") <= e_date,
                   filt_col=as.name(filt_col)))
  
  # Determine end date for period, depending on whether line_plot
  # is defined as "monthly" or "quarterly"
  period_end <- as.Date(ifelse(period == "quarterly", as.Date(as.yearqtr(e_date), frac=1), 
                               as.Date(as.yearmon(e_date), frac=1)))
  
  # Filter df to include one year of data (dated backwards from end date)
  year_df <- df %>%
    filter_(interp(~ as.Date(filt_col, format="%m/%d/%Y") > (period_end - years(1)) 
                   & as.Date(filt_col, format="%m/%d/%Y") <= period_end,
                   filt_col=as.name(filt_col)))
  
  # Add period information to year dataframe if type == "sample"
  if(type == "sample") {
    if(period == "quarterly") {
      year_df$PERIOD <- as.yearqtr(year_df[[filt_col]], format="%Y%m")
    } else {
      year_df$PERIOD  <- as.yearmon(year_df[[eval(filt_col)]], format="%Y%m")
    }
  }
    
  return(list(period_df, year_df))
  
}

get_org_metrics <- function(df, group_by="SUBMITTERNAME") {
  
  # Returns dataframe of metrics for submitters for use in report cards (includes
  # rank and unsat counts). Default group_by column is SUBMITTERNAME but user can
  # select a different column
  
  cols = c(group_by, "TRANSIT_TIME", "COLLECTIONDATE", "COLLECTIONTIME", "BIRTHDATE",
           "BIRTHTIME","UNSATCODE","TRANSFUSED")
  
  temp_df = df %>%
    group_by_(group_by) %>%
    select(one_of(cols)) %>%
    dplyr::summarise(
      total_samples=n(),
      avg_transit_time = check_inf(round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2)),
      min_transit_time = check_inf(min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE)),
      max_transit_time = check_inf(max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE)),
      rec_in_2_days = check_inf(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)),
      percent_rec_in_2_days = check_inf(round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                                sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2)),
      met_goal = ifelse(percent_rec_in_2_days >= 95, 1, 0),
      col_less_than_24_hours = check_inf(sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                               COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                             na.rm=TRUE)),
      percent_less_than_24_hours = check_inf(round(col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100, 2)),
      trans = check_inf(sum(TRANSFUSED == 'Y', na.rm=TRUE)),
      trans_percent = check_inf(round(trans/total_samples * 100, 2)),
      unsat_count = check_inf(sum(!is.na(UNSATCODE))),
      unsat_percent = check_inf(round(unsat_count/total_samples * 100, 2)))
  
  ##### ADD RANKINGS #####
  
  # Rank submitters by mean transit time (ascending order; e.g., least mean transit time = #1)
  temp_df$rank_transit = rank(temp_df$avg_transit_time, na.last="keep", ties.method="min")
  
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
  
  ##### ADD UNSAT COUNTS #####
  
  # get count of unsat codes for each submitter
  unsat_prep = df[!is.na(df$UNSATCODE),] %>% 
    group_by_(group_by, "UNSATCODE") %>%
    dplyr::summarise(count = n())  
  colnames(unsat_prep) = c("ORG", "UNSATCODE", "count")
  
  # get all possibilities for unsat codes
  unsat_seq = seq(1:nrow(unsats))
  
  # create cross join of all possible unsat codes and all submitter names
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
  
  return(list(temp_df, unsats_ready))
  
}

get_diagnoses <- function(df, rpt) {
  
  # Identifies diagnoses for a set of organizations (either hospital or birth center)
  
  temp_submitters = dplyr::filter(submitters, TYPE == rpt)

  # filter out diagnoses not associated with organizations of interest
  temp_diag = df[df$SUBMITTERID %in% temp_submitters$SUBMITTERID,]
  
  # add submitter name to each record
  temp_diag = left_join(temp_diag, submitters, by="SUBMITTERID")
  colnames(temp_diag)[which(names(temp_diag) == "HOSPITALREPORT")] <- "SUBMITTERNAME"
  
  # Select columns from temp_diag for report, get count of each diagnosis
  # NOTE: if a diagnosis is associated with more than one organization (e.g., 
  # because the infant was born at a particular hospital, transferred
  # to another hospital, and both submitted samples that ended up being
  # associated with a diagnosis), ALL organizations in the dataframe submitting samples 
  # associated with a diagnosis will be 'credited' with this on their reports. For this
  # reason, we will NOT want to use a sum of the diagnoses in this dataframe
  # to count the total number of diagnoses, as this method will count some
  # diagnoses more than once.
  diagnoses_temp <- temp_diag %>%
    select(DISORDER, SUBMITTERNAME, LINKID, Narrative) %>%
    group_by(SUBMITTERNAME, DISORDER, LINKID, Narrative) %>%
    dplyr::summarise()
  
  # Get count of separate diagnoses associated with samples for each organization
  diagnoses <- diagnoses_temp %>%
    group_by(SUBMITTERNAME, DISORDER, Narrative) %>%
    dplyr::summarise(Count=n())
  
  # Rearrange columns
  diagnoses <- diagnoses[,c("SUBMITTERNAME","DISORDER","Count","Narrative")]
  
  return(diagnoses)

}

get_state_metrics_over_samples <- function(df, transfused) {
  
  # Get state metrics averaged over samples. Takes dataframe and transfused = TRUE or FALSE to indicate
  # whether transfusion information should be included
  
  temp_df = df %>%
    select(TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, TRANSFUSED, UNSATCODE) %>%
      dplyr::summarise(
        total_samples=n(),
        avg_transit_time = round(mean(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE), 2),
        min_transit_time = min(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
        max_transit_time = max(TRANSIT_TIME[TRANSIT_TIME >= cutoff], na.rm=TRUE),
        rec_in_2_days = sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE),
        percent_rec_in_2_days = round(sum(TRANSIT_TIME <= 2 & TRANSIT_TIME >= cutoff, na.rm=TRUE)/
                                        sum(!is.na(TRANSIT_TIME) & TRANSIT_TIME >= cutoff) * 100, 2),
        col_less_than_24_hours = sum(COLLECTIONDATE == BIRTHDATE & TRANSIT_TIME >= cutoff | 
                                       COLLECTIONDATE == BIRTHDATE + 1 & COLLECTIONTIME < BIRTHTIME & TRANSIT_TIME >= cutoff, 
                                     na.rm=TRUE),
        percent_less_than_24_hours = round(col_less_than_24_hours/sum(TRANSIT_TIME >= cutoff, na.rm=TRUE) * 100, 2),
        trans = sum(TRANSFUSED == 'Y'),
        trans_percent = round(trans/total_samples * 100, 2),
        unsat_count = sum(!is.na(UNSATCODE)),
        unsat_percent = round(unsat_count/total_samples * 100, 2)
      )
    
  if (transfused == FALSE) {
    
    temp_df = select(temp_df, -c(trans, trans_percent))
    
  }
  
  return(temp_df)
  
}

get_state_metrics_over_orgs <- function(df, transfused) {
  
  # Get state metrics averaged over organizations (e.g., hospital or birthcenter). 
  # Takes dataframe of summarized data for each organization and transfused = 
  # TRUE or FALSE to indicate whether transfusion information should be included
  
  temp_df = df %>%
      dplyr::summarise(
        submitters = nrow(df),
        total_samples=sum(total_samples, na.rm=TRUE),
        avg_transit_time = round(mean(avg_transit_time, na.rm=TRUE), 2),
        min_transit_time = min(min_transit_time, na.rm=TRUE),
        max_transit_time = max(max_transit_time, na.rm=TRUE),
        rec_in_2_days = sum(rec_in_2_days, na.rm=TRUE),
        percent_rec_in_2_days = round(mean(percent_rec_in_2_days, na.rm=TRUE), 2),
        met_goal = sum(met_goal, na.rm=TRUE),
        percent_met_goal = round((met_goal / tot_sub_h) * 100, 2),
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

sendEmail <- function(to, subject, msgBody, file=NULL) {
  
  # Sends emails through Outlook client. Outlook needs to be open and will
  # prompt the user whether to allow an application to send email.
  
  OutApp = COMCreate("Outlook.Application")
  
  OutMail = OutApp$CreateItem(0)
  
  OutMail[["To"]] = to
  
  OutMail[["Subject"]] = subject
  
  OutMail[["Body"]]=msgBody
  
  if(!is.null(file) && file != hospital_path) {
    for (f in file) {
      OutMail[["Attachments"]]$Add(f)
    }
  }
  
  # Send email
  OutMail$Send()
  
}

stopQuietly <- function(...) {
  
  # Stops a source file quietly (without printing an error message), used in cases
  # where we have multiple files that need to stop running, but only have one of them
  # throw an error.
  
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
  
} 


# Read in submitter names as we wish them to appear in the report
temp <- paste(codes_path, slash, "VA NBS Report Card Organization Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp, sep=","))
names(submitters) <- c("SUBMITTERID","HOSPITALREPORT","TYPE")
submitters$SUBMITTERID <- as.character(submitters$SUBMITTERID)

# Stop report if all values in TYPE are not "Hospital" or "BirthCenter"
if (!all(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter"))) {
  
  {stop(sprintf("The 'VA NBS Report Card Organization Names' csv file has '%s' listed in the 'Type' column. Please only use either 'Hospital' or 'BirthCenter' in this column.", 
                levels(submitters$TYPE)[which(!(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter")))])) }
}

# Test for IDs assigned to multiple hospitals in submitters
ID_test <- submitters[(duplicated(submitters$SUBMITTERID) | duplicated(submitters$SUBMITTERID, 
                                                                       fromLast=TRUE)),]

# Stop report if duplicate IDs are discovered
if (nrow(ID_test) != 0){
  e_begin <- ifelse(length(unique(ID_test$SUBMITTERID)) == 1, "One ID", "Several IDs")
  e_verb <- ifelse(length(unique(ID_test$SUBMITTERID)) == 1, "is", "are")
  e_messages <- ""
  for (id in unique(ID_test$SUBMITTERID)) {
    temp_hosps <- ID_test$HOSPITALREPORT[ID_test$SUBMITTERID == id]
    test_hs <- paste0("\nHOSPITALS:     ", paste(temp_hosps, collapse=", "), "\n")
    test_ids <- paste0("DUPLICATED ID: ", id, "\n")
    e_messages <- paste0(e_messages, paste0(test_hs, test_ids))
  }
  {stop(sprintf("%s in 'VA NBS Report Card Organization Names' %s assigned to more than one hospital:\n%s\nPlease correct in 'VA NBS Report Card Organization Names' before running reports.", 
                e_begin, e_verb, e_messages)) }
}
