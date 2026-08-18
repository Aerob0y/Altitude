

# Updates
source("update/update.R", local = FALSE, echo = TRUE)
updates(what = "", manual = FALSE) # do not fie unless we want to force an update
updates(what = "border_all", manual = TRUE) # do not fie unless we want to force an update
load_data("border_all", refresh_cache = TRUE)

# This file is used to run the interactive app. It is not intended to be sourced by other scripts, but rather to be run directly.
source("app/app.R", local = FALSE, echo = TRUE)
