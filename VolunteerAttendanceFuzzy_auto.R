library(googlesheets4)
library(tidyverse)
library(readr)
library(fuzzyjoin)

# Authorize Google Sheets access (just run once)
#gs4_auth()

#update this list with volunteer first names
# Read session names from a text file
session_first_names <- readLines("session_names.txt")

#read in volunteer names
# this is a custom sheet that copies the volunteer info from the master sheet

sheet <- "https://docs.google.com/spreadsheets/d/TOKENHERE
volunteer_info <- read_sheet(sheet, range = "Volunteers!A:B")

# Combine first and last names
volunteer_names <- paste(volunteer_info[[1]], volunteer_info[[2]])


#add a new tab for the practice to google sheets
practice_date <- readline(prompt = "Enter the practice date (MM.DD.YY): ")
sheet_add(ss = sheet, sheet = practice_date, .after = 2)  # Add after the 2nd tab

# Prepare session names for fuzzy matching
session_names_df <- tibble(Volunteers = session_first_names)

# Prepare volunteer names for fuzzy matching
volunteer_names_df <- tibble(Volunteers = volunteer_names)

# Perform fuzzy matching
matched_volunteers <- stringdist_full_join(volunteer_names_df, session_names_df,
                                                                    method = "jw", max_dist = 0.23)

# Filter to retain only matched records
matched_volunteers <- matched_volunteers %>%
  filter(!is.na(Volunteers.y)) %>%
  select(Volunteers = Volunteers.x, MatchedName = Volunteers.y)

View(matched_volunteers)
# Generate attendance data
attendance_data <- matched_volunteers %>%
  mutate(Attend = "X") %>%
  select(Volunteers, Attend)




# Append this data to the practice date tab
write_sheet(ss=sheet, sheet = practice_date, data = attendance_data)



# Read the existing "Summary 2024" data
summary_data <- read_sheet(ss = sheet, sheet = "Summary 2024")

# Rename the attendance_data tibble column to match practice_date
# Dynamically rename the 'Attend' column to practice_date
attendance_data <- attendance_data %>%
  rename(!!practice_date := Attend)


# Merge Data
# Merge the new attendance data with the existing summary data
updated_summary_data <- summary_data %>%
  left_join(attendance_data, by = "Volunteers")

#move the last column up to the 5th position
last_column_name <- names(updated_summary_data)[ncol(updated_summary_data)]

# Rearrange the columns to move the last to the 2nd position
updated_summary_data <- updated_summary_data %>%
  select(1:1, last_column_name, 2:(ncol(updated_summary_data)-1))

#Write the Updated Data Back to the Google Sheet
write_sheet(sheet, data = updated_summary_data, sheet = "Summary 2024")
