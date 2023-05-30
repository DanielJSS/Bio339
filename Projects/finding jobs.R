library(rvest)

# URL of the webpage you want to scrape
url <- "https://www.finn.no/min-finn/jobbunivers/anbefalinger"

# Function to extract relevant information from a job listing
extractJobInfo <- function(jobNode) {
  title <- jobNode %>% html_node("Biolog") %>% html_text()
  company <- jobNode %>% html_node("") %>% html_text()
  location <- jobNode %>% html_node("Bergen") %>% html_text()
  
  # Check if the title or location contains the relevant information
  isRelevant <- grepl("biolog", title, ignore.case = TRUE) & grepl("Bergen", location, ignore.case = TRUE)
  
  # Return a list of extracted information and the relevance flag
  list(title = title, company = company, location = location, isRelevant = isRelevant)
}

# Scrape the webpage and extract job positions
page <- read_html(url)
jobNodes <- page %>% html_nodes(".ads__unit__content")  # Modify this selector based on the structure of the webpage

relevantJobs <- lapply(jobNodes, extractJobInfo)  # Extract information from each job listing

# Filter relevant job positions
filteredJobs <- Filter(function(job) {
  job$isRelevant
}, relevantJobs)

# Print the filtered job positions
for (job in filteredJobs) {
  cat("biolog:", job$title, "\n")
  cat("Company:", job$company, "\n")
  cat("Location:", job$location, "\n")
  cat("-----------------------------------------\n")
}
