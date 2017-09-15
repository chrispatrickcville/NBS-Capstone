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
          'rmarkdown',
          'mailR')

for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}

# Install functions
funcs <- c('addLineBreaks',
           'stopQuietly',
           'substrRight',
           'formatDate',
           'formatStartAndEndDates',
           'trim',
           'checkInf',
           'getFileList',
           'dateReformat',
           'dateRepair',
           'colCheck',
           'getDates',
           'dateCompare',
           'dateCompCheck',
           'readData',
           'getDisorders',
           'checkNotInDiagnosisNarratives',
           'reportAndRepairDifferences',
           'timeGet',
           'timeDiff',
           'createFiltDFs',
           'getOrgMetrics',
           'getDiagnoses',
           'getStateMetricsOverSamples',
           'getStateMetricsOverOrgs',
           'createSummaryReport',
           'requestEmail')
funcs <- unlist(lapply(funcs, function(x) paste0(lib, x, ".R")))
for (f in funcs) {
  source(f)
}

# Reformat start date and end date as dates
if (exists("start_date")) {
  
  formatStartAndEndDates(start_date, end_date)
  
}

# Read in submitter names as we wish them to appear in the report
temp <- paste(codes_path, slash, "VA NBS Report Card Organization Names.csv", sep="")
submitters <- as.data.frame(read.csv(temp, sep=",", stringsAsFactors = FALSE))
submitters <- submitters[, c("submitterCode", "Name", "Type")]
names(submitters) <- c("SUBMITTERID","HOSPITALREPORT","TYPE")

# Remove any extra spaces at end of HOSPTIALREPORT values (names of organizations)
submitters$HOSPITALREPORT <- trim(submitters$HOSPITALREPORT)
  
# Test for IDs assigned to multiple organizations in submitters
IDs <- trim(unlist(strsplit(submitters$SUBMITTERID, ";")))
ID_test <- IDs[duplicated(IDs)]

# Stop report if duplicate IDs are discovered
if (length(ID_test) != 0){
  e_begin <- ifelse(length(ID_test) == 1, "One ID", "Several IDs")
  e_verb <- ifelse(length(ID_test) == 1, "is", "are")
  e_messages <- ""
  for (id in ID_test) {
    temp_hosps <- submitters$HOSPITALREPORT[grepl(id, submitters$SUBMITTERID)]
    test_hs <- paste0("\nHOSPITALS:     ", paste(temp_hosps, collapse=", "), "\n")
    test_ids <- paste0("DUPLICATED ID: ", id, "\n")
    e_messages <- paste0(e_messages, paste0(test_hs, test_ids))
  }
  {stop(sprintf("%s in 'VA NBS Report Card Organization Names' %s assigned to more than one organization:\n%s\nPlease correct in 'VA NBS Report Card Organization Names' before running reports.", 
                e_begin, e_verb, e_messages)) }
}

# Create a new dataframe to store the 'exploded' data
submitters_rev <- submitters[0, ]

# Get the HOSPITALREPORT and TYPE for each individual ID
for (id in IDs) {
  vec <- data.frame(t(c(id, 
                        submitters$HOSPITALREPORT[grepl(id, submitters$SUBMITTERID)], 
                        submitters$TYPE[grepl(id, submitters$SUBMITTERID)]
  )))
  names(vec) <- names(submitters_rev)
  submitters_rev <- rbind(submitters_rev, vec)
}

# Replace submitters with submitters_rev
submitters <- submitters_rev
rm(submitters_rev)

# Change submitters to character
for (n in colnames(submitters)) {
  submitters[, n] <- as.character(submitters[, n])
}

# Stop report if all values in TYPE are not "Hospital" or "BirthCenter"
if (!all(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter"))) {
  
  {stop(sprintf("The 'VA NBS Report Card Organization Names' csv file has '%s' listed in the 'Type' column. Please only use either 'Hospital' or 'BirthCenter' in this column.", 
                levels(submitters$TYPE)[which(!(levels(submitters$TYPE) %in% c("Hospital", "BirthCenter")))])) }
}

# If user has entered something other than "H" or "BC" for 
# report_type and is not running the summary report or running email, 
# stop the file and report an error
if (!exists("email_var") & !exists("summary_report")) {
  if (report_type != "H" & report_type != "BC") {
    stop(sprintf("You entered '%s' for the report_type variable. Please change the value for this variable to either 'H' to run reports for hospitals or 'BC' to run reports for birthcenters.",
                 report_type))
  }
}

# Stop report if user has identified BC as the type of report, has requested
# that report cards or diagnoses be run, and has not identified any birthcenters in the
# VA NBS file
if (!exists("email_var") & !exists("summary_report")) {
  if (report_type == "BC" & !"BirthCenter" %in% submitters$TYPE) {
    {stop(sprintf("You have requested that reports for birth centers be generated, but there\nare no birth centers identified in the 'VA NBS Report Card Organization Names'\ncsv file. Please enter one or more birth centers in this file if you wish to\ncreate reports for these organizations."))}
  }
}
