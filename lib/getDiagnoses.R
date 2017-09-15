getDiagnoses <- function(df, rpt) {
  
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