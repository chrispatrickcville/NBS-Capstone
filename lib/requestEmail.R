requestEmail <- function() {
  
  # Requests an email address and password for sending emails. If
  # email address is NULL, will replace with "donotreply@dgs.virginia.gov".
  # Note that if the user enters a password, this should be removed
  # from the environment at the end of the email module (see the last
  # line of email_generator.R for how to do this).
  
  cat(addLineBreaks("\nEnter the email address from which to send the hospital reports (note that this must be a dgs.virginia.gov account). If you wish to use the default email address of 'donotreply@dgs.virginia.gov', press enter instead:"))
  address <- readline("> ")
  
  if(address == "") {
    
    cat("Using 'donotreply@dgs.virginia.gov' to send reports.")
    return("donotreply@dgs.virginia.gov")
    
  } else {
    
    cat("Enter your email password. If no password is required, press enter instead.")
    password <- readline("> ")
    
    if(password == "") {
      
      return(address)
      
    } else {
      
      cat(addLineBreaks("\nYour password information will be removed from the R environment after emails are sent."))
      return(list(address, password))
      
    }
  }
  
}