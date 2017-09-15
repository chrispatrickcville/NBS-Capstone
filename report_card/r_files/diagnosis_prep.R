# PREPARE DIAGNOSIS DATA

# Identify columns we need for diagnosis data
diagnosis_cols <- c("SAMPLEID", "DIAGNOSIS", "DIAGNOSISDATE", "SUBMITTERID", "LINKID")

# test to make sure data has the correct columns
colCheck(diag_data_path, "diagnosis")

# read in diagnosis data
initial_dd_diag <- readData(folder=diag_data_path, 
                            type="diagnosis", 
                            separator=separator, 
                            date_cols="DIAGNOSISDATE")

# check that range of DIAGNSOSISDATE dates overlaps the requested start and end date
if (!exists("testing")) {
  dateCompCheck(initial_dd_diag, "diagnosis", "DIAGNOSISDATE")
}

# filter diagnosis data based on start/end date
temp_dd_diag <- createFiltDFs(initial_dd_diag, type="diagnosis")
dd_diag <- as.data.frame(temp_dd_diag[1])

# Identify columns we need to have for sample data
sample_cols = c("SAMPLEID", "CATEGORY")

# read in sample data (need to join on sample data so "Proficiency", "Treatment",
# and "Treatment - PKU" categories can be removed)
sample_for_diag <- readData(folder=sample_data_path, 
                            patt=NULL, 
                            separator=separator, 
                            type="sample")

# join on diagnosis data and remove any samples where CATEGORY is 
# "Proficiency", "Treatment", or "Treatment - PKU"
remove_cats <- c("Proficiency","Treatment","Treatment - PKU")
dd_diag <- left_join(dd_diag, sample_for_diag[, c("SAMPLEID", "CATEGORY")], by="SAMPLEID")
dd_diag <- dd_diag[!(dd_diag$CATEGORY %in% remove_cats),]
dd_diag$CATEGORY <- NULL

# Get DISORDER associated with each diagnosis. If check_diag
# is set to TRUE, will also report out any missing DIAGNOSES
# from diagnosis_narratives file. DIAGNOSIS column will also
# be removed from data, leaving only DISORDERs.
dd_diag <- getDisorders(dd_diag, check_diag=check_diag)

# join diagnoses and narratives
dd_diag_narr <- full_join(dd_diag, diag_narr, by=c("DISORDER"="Disorder"))
