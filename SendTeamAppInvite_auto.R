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
location = "Anytown"
teamapp_url: "https://TEAMNAME.teamapp.com"


'
# uncomment to set up auth tokens
# Authorize Google Sheets access
gs4_auth()


# STOP- just run once
create_smtp_creds_key(
  id = "unitedrocks_gmail",
  user = manager_email,
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE,
)
'

# file paths assume script is saved in same location as csv files.
# if not, update the file path to the correct location
# import volunteer data csv
volunteers <- read.csv("volunteers.csv")

# remove rows with email address that equal ""
volunteers <- volunteers[volunteers$email != "",]

# loop through volunteers email addresses
for (i in 1:nrow(volunteers)) {
  # Prepare the email
  email_body <- glue::glue(
    "<html>
      <body>
        <p>Hello {volunteers$name[i]},</p>
        <p>I am {manager_name}, the United Rocks:{location} practice location manager and I'm reaching out to all current and previous UR-{location} volunteers to invite you to join us on Team App.</p>
        <p>The app will replace our WhatsApp group message and includes the ability for you to mark which weeks you can attend practice, sync the practice schedule to your calendar, communicate with the leadership team, and earn rewards! Team App will enable us to let you know about events, volunteer perks and updates effectively and will replace the weekly registration for Grapevine volunteers.</p>
        <p>If you sign up with this e-mail ({volunteers$email[i]}), I've already pre-loaded some profile info and approved your membership so creating an account will automatically link to that profile. You can choose to download the app and/or interact with the team page through our team website: <a href='{teamapp_url}'>{teamapp_url}</a>. Your profile info came from the volunteer interest form so please review for accuracy and update any outdated info as needed.</p>
        <p>Here is our Team App page:</p>
        <p><a target='_blank' href='{teamapp_url}'><img width='150' height='50' title='Find us on Team App' alt='Find us on Stack Team App' src='https://assets.teamapp.com/assets/app/social/find-us-on-stackteamapp-68ca0d010e74738f098540b168e050061f1af1e68476b421ba63d440cd6f8b36.png'/></a></p>
        <p>United Rocks is a community of climbers who are passionate about rock climbing and are dedicated to helping individuals with intellectual and developmental disabilities learn and grow in the sport. Our volunteers are a crucial component to help us with our practices and special events.</p>
        <p>Even if you can only volunteer once a month or seasonally, we'd love to have you on Sundays at Movement Gym from 10am-12pm. As a bonus, all volunteers earn a Movement Gym day pass for the day they volunteer. I hope you join us after practice to boulder or climb as a volunteer team! As we grow our volunteer base, we are able to support more even more climbers. Our waitlist of athletes needs dedicated volunteers! You are key to reaching and supporting these athletes that may not otherwise have an inclusive opportunity to grow in the sport of rock climbing.</p>
        <p>If you are no longer interested in volunteering with us, just ignore this e-mail or you can send me a note to be removed from future communication.</p>
        <p>Thank you so much for your continued dedication and care for our athletes. Please reach out if I can answer any questions.</p>
        <p>Sincerely,</p>
        <p>{manager_name}<br>
        United Rocks Gym Manager: {location}<br>
        {manager_phone}</p>
      </body>
    </html>"
  )
  
  email <- compose_email(body = md(email_body)) |> 
    smtp_send(
      from = manager_email,
      to = volunteers$email[i],
      subject = "Team App Invite for United Rocks: Volunteers",
      credentials = creds_key(id = "unitedrocks_gmail")
    )

}

# export your climber member info from team_app
# there are multiple primary guardians available in different columns
# import guardian e-mails for parent invites
parents <- read.csv("climbers.csv")

guardian1 <- parents #make copies
guardian2 <- parents
guardian3 <- parents

# remove rows with email address that equal ""
guardian1 <- guardian1[guardian1$primary_guardian_1_email != "",]
guardian2 <- guardian1[guardian1$primary_guardian_2_email != "",]
guardian3 <- guardian1[guardian1$primary_guardian_3_email != "",]

send_email_to_guardian <- function(df, name_col, email_col) {
  # loop through volunteers email addresses
  for (i in 1:nrow(df)) {
    # Prepare the email
    email_body <- glue::glue(
      "<html>
        <body>
          <p>Hello {df[[name_col]][i]},</p>
          <p>This is {manager_name}, the United Rocks:{location} practice location manager and I'm reaching out to all current United Rocks climber parents/guardians to invite you to join us on Team App.</p>
          <p>The app will replace our WhatsApp parent's group message and includes the ability for you to mark which weeks you can attend practice, sync the practice schedule to your calendar, communicate with the leadership team, and get notified about special events! </p>
          <p>If you sign up with this e-mail ({df[[email_col]][i]}), I've already pre-loaded some profile info and approved your membership so creating an account will automatically link to that profile which is already linked to your child. You can choose to download the app and/or interact with the team page through our team website: <a href='{teamapp_url}>{teamapp_url}</a>. Your profile info came from the climber interest form so please review for accuracy and update any outdated info as needed. We also need you to fill out your climber's waist size and foot length (in cm) so when team gear orders go in, I know what size to get!</p>
          <p>Here is our Team App page:</p>
          <p><a target='_blank' href='{teamapp_url}'><img width='150' height='50' title='Find us on Team App' alt='Find us on Stack Team App' src='https://assets.teamapp.com/assets/app/social/find-us-on-stackteamapp-68ca0d010e74738f098540b168e050061f1af1e68476b421ba63d440cd6f8b36.png'/></a></p>
          <p>United Rocks is a community of climbers who are passionate about rock climbing and are dedicated to helping individuals with intellectual and developmental disabilities learn and grow in the sport. </p>
          <p>Thank you so much for your continued dedication and care for our athletes. Please reach out if I can answer any questions.</p>
          <p>Sincerely,</p>
          <p>{manager_name}<br>
          United Rocks Gym Manager: {location}<br>
          {manager_phone}</p>
        </body>
      </html>"
    )
    
    email <- compose_email(body = md(email_body)) |> 
      smtp_send(
        from = manager_email,
        to = df[[email_col]][i],
        subject = "Team App Invite for United Rocks: Climber Parents/Guardians",
        credentials = creds_key(id = "unitedrocks_gmail")
      )
    
  }
}


send_email_to_guardian(guardian1, "primary_guardian_1", "primary_guardian_1_email")
send_email_to_guardian(guardian2, "primary_guardian_2", "primary_guardian_2_email")
send_email_to_guardian(guardian3, "primary_guardian_3", "primary_guardian_3_email")
