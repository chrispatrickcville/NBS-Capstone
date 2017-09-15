##### Change the settings below before sending each email #####

# FUNCTIONALITY TEST
# Do you want to run a single email to yourself for testing your message 
# before sending to all hospitals? Enter 'Y' if you want to test the
# email first, and also enter the email address to use for sending the test
# email.

email_test <- "Y"
test_email_recipient <- "Christopher.Patrick@dgs.virginia.gov"

#########

# CHOOSE WHETHER TO SEND THE EMAIL TO HOSPITALS OR TO BIRTHCENTERS
# Enter 'H' for hospitals or 'BC' for birthcenters.

send_to <- "H"

#########

# EMAIL SUBJECT
# This will be what Outlook uses as the subject for the email.

subject <- "Quarterly Report from DCLS: 10/1/2016 - 12/31/2016"

#########

# EMAIL MESSAGE
# This will be what appears in the message body.
# Use '<br/>' for each carriage return, as in this example:
#   message <- "Hi,<br/><br/>Please find attached the new hospital report card from the Division of Consolidated Laboratory Services. If your hospital had any diagnoses for the given period, you will also find a diagnosis report attached.<br/><br/>Sincerely,<br/>DCLS"

# You can use other HTML features as well, such as:
#     <i>italicized words</i> - for italics
#     <strong>bolded words</strong> - for bold

message <- paste0("Good afternoon,<br/><br/>Please find your Newborn Screening Hospital Report Card for Quarter 2 of 2017.", 
                  "<br/><br/>The Virginia Newborn Screening Program has been working to enhance quarterly report card features in an effort to better inform your facility of important newborn screening data we are tracking. A new measurement has been added to the sample table which provides the number of blood spot cards received by the lab after 4 or more days of collection at your facility. This measurement was added because we continue to receive one or more of these ", 
                  "<i>extremely delayed</i>", " samples from many of our hospitals during a quarter. Please remember that delays in newborn screening can have devastating effects on babies with treatable diseases. These effects include severe mental and intellectual disability, and even death. Our courier system runs Sunday-Friday to assist facilities in getting these critical samples to the laboratory for testing within two days. For more information please contact Annie Colfax at 804-648-4480 ext 186.", 
                  "<br/><br/>We appreciate your partnership in this life-saving initiative.", 
                  "<br/><br/>Sincerely,<br/>The Virginia Newborn Screening Timeliness Workgroup")

#########

# SET COMPUTER TYPE
# Enter 'PC' or 'MAC'

comp_type <- 'PC'

#########

# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored

wd <- "C:\\RProjects\\Capstone\\report_card\\report_card"

#########

# CODES FILE PATH
# This should be your directory (e.g., the location on your
# computer) where you have all supporting csv files. Required
# at this location are three files:
#   -- diagnosis_narratives.csv - disorder names for diagnoses and narratives to use for each disorder
#   -- unsat_codes.csv - descriptions for each unsatisfactory code
#   -- VA NBS Report Card Organization Names.csv - hospital and birthcenter SUBMITTERIDs, names,
#      and other needed information for these organizations

codes_path <- "C:\\RProjects\\Capstone\\report_card\\submitter_and_unsat_codes"

#########

# HOSPITAL REPORT FOLDER LOCATION
# Enter the location on your computer where the reports for sending to
# hospitals are stored

hospital_path <- "C:\\RProjects\\Capstone\\report_card\\generated_reports\\report_cards_hospital"

#########

# BIRTHCENTER REPORT FOLDER LOCATION
# Enter the location on your computer where the reports for sending to
# birthcenters are stored

center_path <- "C:\\RProjects\\Capstone\\report_card\\generated_reports\\report_cards_birthcenter"

#########

# IMPORTANT THINGS TO CHECK BEFORE RUNNING THE EMAIL CODE BELOW:
#   1. Change the email_test variable to 'Y' if you wish to test the email.
#   2. Set the send_to variable to 'H' or 'BC' depending on whether you wish to
#      send emails to hospitals or birthcenters.
#   3. Update the subject and message above.
#   4. The folder where you have your hospital reports (set in the hospital_path variable)
#      or the folder where you have your center reports (set in the center_path variable)
#      should ONLY have the reports you want to send. If there are older reports with 
#      the hospital's name at this location, the email function will send them with 
#      the email.
#   5. Run all lines of code before these instructions so all variables
#      are correctly set.

# *** DO NOT CHANGE THIS CODE ***

# SET FILE SEPARATOR
slash <- ifelse(comp_type == 'PC', '\\', '/')

# SET LOCATION FOR FUNCTIONS
lib <- "C:\\RProjects\\Capstone\\lib\\"

# SEND EMAILS
send_emails <- paste0(wd, slash, "email_generator.R")
source(send_emails)
