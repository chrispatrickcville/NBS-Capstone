# LOAD PACKAGES AND FUNCTIONS
load_packages <- paste0(wd, slash, "load_packages_and_functions.R")
source(load_packages)

# If user has entered something other than "H" or "BC" for 
# send_to, stop running the file and report an error
if (send_to != "H" & send_to != "BC") {
  stop(sprintf("You entered '%s' for the send_to variable. Please change the value for this variable to either 'H' to send emails to hospitals or 'BC' to send emails to birthcenters.",
               send_to))
}

# load in email addresses
emails <- read.csv(paste(codes_path, slash, "organization_emails.csv", sep=""), 
                   stringsAsFactors=FALSE, header=TRUE)

# get unique submitters from submitters dataframe
sub_unique <- unique(submitters[, 2:3])

# get type for each organization in hospital_emails
emails <- suppressWarnings(left_join(emails, sub_unique, c("Name" = "HOSPITALREPORT")))

# select addresses of interest depending on whether user has entered
# 'H' or 'BC' for the send_to variable, also set path for location reports
# for sending
if (send_to == "H") {
  emails <- emails[emails$TYPE == "Hospital", ]
  path <- hospital_path
} else {
  emails <- emails[emails$TYPE == "BirthCenter", ]
  path <- center_path
}

# remove rows with no email addresses
emails <- emails[emails$Email != "",]

# set 'email_end' to 1 if user is testing email functionality
# so only a single email is sent, which will have the report
# for the first organization in the list (organized alphabetically)
email_end <- ifelse(email_test == 'Y', 1, nrow(emails))

# loop through organizations, adding attachments that match their names,
# and send email to each set of receipients for the organization
for (i in 1:email_end) {
  attachments <- paste(path, slash, list.files(path, 
                                  pattern=paste0(emails$Name[i], "*")), sep="")
  if(!is.list(attachments) && attachments == path) next
  recipients <- ifelse(email_test == 'Y', test_email_recipient, emails$Email[i])
  sendEmail(recipients, subject, message, attachments)
}
