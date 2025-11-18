

# Library -----------------------------------------------------------------

library(tidyverse)
library(httr)

# Function -----------------------------------------------------------------

update_tentwo <- function(SaveDataLocation, url){

# Get today's date in the format YYYY-MM-DD
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Replace "xxxxxx" with today's date
updated_string <- gsub("xxxx", today_date, url)

response <- GET(url, add_headers('User-Agent' = 'Mozilla/5.0'))

if (status_code(response) == 200) {
  # Extract the Content-Disposition header
  content_disposition <- headers(response)$`content-disposition`
  
  if (!is.null(content_disposition)) {
    # Extract the file name from the header
    
    filename <- sub(".*filename=\"([^\"]+)\".*", "\\1", content_disposition)
    message("File name from header: ", filename)
    
    # Save the file
    writeBin(content(response, "raw"), paste(SaveDataLocation, filename, sep = "/"))
    message("File downloaded successfully as: ", filename)
  } else {
    message("Content-Disposition header not found. Saving with default name 'file.xls'")
    parts <- strsplit(url, "/")[[1]]
    filename <- tail(parts, n=1)
    writeBin(content(response, "raw"), filename)
  }
} else {
  message("Failed to download file. HTTP status: ", status_code(response))
}

# wait a random length of time so that the response doesn't look like a bot
Sys.sleep(runif(n=1, min=1, max=4))


}
