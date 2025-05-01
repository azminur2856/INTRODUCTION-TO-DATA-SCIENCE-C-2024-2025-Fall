# Load required libraries
library(rvest)
library(dplyr)

# Define the URL of the AIUB contact page
url <- "https://www.aiub.edu/contact-us"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract all table rows containing contact information
rows <- webpage %>%
  html_nodes("table tr") # Select all table rows

# Initialize a data frame to store the results
contact_info <- data.frame(Field = character(), Name = character(), Email = character(), stringsAsFactors = FALSE)

# Loop through the rows to extract field, name, and email
for (row in rows) {
  field <- row %>%
    html_node("td:nth-child(1)") %>%
    html_text(trim = TRUE)
  
  name <- row %>%
    html_node("td:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  email <- row %>%
    html_node("td:nth-child(3)") %>%
    html_text(trim = TRUE)
  
  # Append to the data frame if relevant data is found
  if (!is.na(field) || !is.na(name) || !is.na(email)) {
    contact_info <- rbind(contact_info, data.frame(
      Field = ifelse(!is.na(field), field, ""),
      Name = ifelse(!is.na(name), name, ""),
      Email = ifelse(!is.na(email), email, ""),
      stringsAsFactors = FALSE
    ))
  }
}

# Additional targeted scraping for emails missed in the main table
extra_emails <- webpage %>%
  html_nodes(xpath = "//td[contains(text(), '@')]") %>% # Target any <td> containing an email
  html_text(trim = TRUE)

# Add missing emails to the corresponding names/fields
for (email in extra_emails) {
  if (!(email %in% contact_info$Email)) {
    contact_info <- rbind(contact_info, data.frame(
      Field = "Extra Email",
      Name = "",
      Email = email,
      stringsAsFactors = FALSE
    ))
  }
}

# Remove duplicates and clean up
contact_info <- contact_info %>%
  distinct()

# Display the final contact information
print(contact_info)

# Save the data to a CSV file
write.csv(contact_info, "aiub_contact_info_full.csv", row.names = FALSE)
