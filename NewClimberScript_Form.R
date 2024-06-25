# Install and load necessary packages
#if (!require("googlesheets4")) install.packages("googlesheets4")
#if (!require("sendmailR")) install.packages("sendmailR")
library(googlesheets4)
library(sendmailR)
library(blastula)
library(keyring)

#enter your contact info here
manager_name <- "Jane Doe"
manager_email<- "jane@unitedrocks.org"
manager_phone <- "123-456-7890"


#copy form data from e-mail and paste it here:
form_text <- "Parent/Guardian Details
Name
 	Mark Smith
Email
 	smith@temp.com
Phone
 	(123) 456-7890
Participant Details
Name
 	Kiddo Smith
Date of Birth
 	10/06/2017
Fun fact about your climber
 	Everyting is awesome
Location Details
Preferred Location
 	Anytown, USA
Where did you hear about us?
 	Website"

'''
# Uncomment to Authorize Google Sheets access if you do not have a local token
gs4_auth()

# update for your account, just needs to run once- after that you can comment it out
create_smtp_creds_key(
  id = "unitedrocks_gmail",
  user = "sarah@unitedrocks.org",
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE,
)
'''

# Specify the Google Sheet you want to update by its name or URL
# This link goes to the "TEAM MEMBER WAIT LIST" in the Google Drive.
sheet <- "https://docs.google.com/spreadsheets/d/token_here"


# Parse form input
parse_form_data <- function(form_text) {
  lines <- strsplit(form_text, "\n")[[1]]
  lines <- trimws(lines)
  
  # Helper function to extract values following a specific label
  get_value_after_label <- function(label, start_index = 1) {
    lines_subset <- lines[start_index:length(lines)]
    index <- which(grepl(paste0("^", label, "$"), lines_subset, ignore.case = TRUE))
    if (length(index) > 0) {
      index <- start_index + index[1] - 1
      if (index < length(lines)) {
        return(lines[index + 1])
      }
    }
    return(NA)
  }
  
  parent_name_index <- which(grepl("Parent/Guardian Details", lines))
  participant_index <- which(grepl("Participant Details", lines))
  location_index <- which(grepl("Location Details", lines))
  
  list(
    parent_name = get_value_after_label("Name", parent_name_index),
    email = get_value_after_label("Email", parent_name_index),
    phone = get_value_after_label("Phone", parent_name_index),
    participant_name = get_value_after_label("Name", participant_index),
    dob = get_value_after_label("Date of Birth", participant_index),
    fun_fact = get_value_after_label("Fun fact about your climber", participant_index),
    location = get_value_after_label("Preferred Location", location_index),
    referral = get_value_after_label("Where did you hear about us?", location_index),
    date_registered = Sys.Date()
  )
}



parsed_data <- parse_form_data(form_text)



# Prepare HTML content
# update e-mail body with Gym Manager contact info and location
html_content <- glue::glue(
  "
  <html>
    <body>
      <p>Hello {parsed_data$parent_name},</p>
      <p>I am so excited to receive your registration to join United Rocks! Your climber {parsed_data$participant_name} has been added to the waitlist at {new_entry$location}.</p>
      <p>Currently, we have reached the maximum number of team members at Grapevine, but spots are going to be opening up soon.</p>
      <p>We have placed you on our waitlist so that once a spot opens up, we can reach out and invite you to join our practices. Once a spot opens up, you will be able to start the one-month trial period to allow you and your child to experience a month of practices with United Rocks and see if it is the right fit for you.</p>
      <p>We know this may not be the ideal response but we want to ensure that every climber has the proper support to be successful. We promise the wait will be worth it!</p>
      <p>In the meantime, do not hesitate to reach out to me if you have any questions.</p>
      <p>Sincerely,</p>
      <p>{manager_name}<br>
      United Rocks Gym Manager: {location}<br>
      {manager_phone}<br>
      {manager_email}</p>
    </body>
  </html>
  "
)

# Compose the email with HTML content
email <- compose_email(
  body = md(html_content)  # Ensuring it is treated as Markdown that allows HTML
)



# Assuming smtp_send is properly configured to use credentials stored via create_smtp_creds_key as earlier discussed.

# Send email
smtp_send(
  email = email,
  from = manager_email,
  to = parsed_data$email,
  subject = "United Rocks: Waitlist Confirmation",
  credentials = creds_key(id = "unitedrocks_gmail")
)

#now update the google sheet with the waitlisted climber info
# Read the existing Google Sheet
waitlist_entries <- read_sheet(sheet)

# Create a new row with the new entry
new_row <- data.frame(
  Climber_Name = parsed_data$participant_name,
  Parent_Name = parsed_data$parent_name,
  Team_Location = location,
  Date_Registered = parsed_data$date_registered,
  One_Month_Trial_Start_Date = NA, # Placeholder for manual entry
  One_Month_Trial_Completed = NA, # Placeholder for manual entry
  Paperwork_Submitted_Signed = NA, # Placeholder for manual entry
  Parent_Email = parsed_data$email,
  Parent_Phone = parsed_data$phone,
  Climber_DOB = parsed_data$DOB, # Placeholder for manual entry
  Where_did_you_hear_about_us = parsed_data$referral
)

# Append the new row to the Google Sheet
sheet_append(sheet, data = new_row)
