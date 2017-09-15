# Set testing to TRUE to allow for not generating all of the error/warning messages
testing <- TRUE

# Source load_packages file
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# Source diagnosis_prep.R for preparation of diagnosis data and reporting out any issues
check_diag = TRUE # Check for missing diagnoses when getDisorders is run
zero_message = TRUE # Output message if no disorders are missing
source(paste0(wd, slash, "diagnosis_prep.R"))

# Remove variables
rm(testing, check_diag, zero_message)