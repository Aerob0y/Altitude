

source("update/update_utilities/update_libraries.r")
source("update/update_utilities/update_etag.r")
source("update/update_utilities/export_plotly_spec.r")


# Load data utilities + update modules (so functions exist)
source("app/utils/data/data.r", local = FALSE, echo = FALSE)
purrr::walk(
  list.files("update/update_modules", full.names = TRUE, pattern = "\\.r$"),
  ~ source(.x, local = FALSE, echo = FALSE)
)

# Add more entries here as you build modules
registry <- list(
  adp = update_adp_data,
  bond   = update_bond_data,
  #border = update_border,
  ect = download_latest_ect,
  fuel = update_fuel_data,
  rbnz = rbnz_fetch_all
)

updates <- function(targets = "", manual = FALSE) {
  # if not manual and interactive, print a message and return early
  if (targets == "") {
    print("No updates specified. \nAvailable updates: adp, bond, border, ect, fuel, rbnz.\nUse 'All' to run all updates, or specify a comma-separated list of updates to run.")
    return(invisible(NULL))
  }

  if (!(manual || !interactive())) {
    print(paste("Interactive mode detected. Update aborted. Use manual = TRUE to run in interactive mode.\nAvailable updates: adp, bond, border, ect, fuel, rbnz.\nUse 'All' to run all updates, or specify a comma-separated list of updates to run."))
    return(invisible(NULL))
  }
  targets <- strsplit(targets, ",", fixed = TRUE)[[1]] %>% unlist() %>% trimws()
  if (identical(tolower(targets), "all")) {targets <- names(registry)}

  # Validate
  missing <- setdiff(targets, names(registry))
  if (length(missing) > 0) {
    stop("Unknown update(s): ", paste(missing, collapse = ", "),
         "\nKnown updates: ", paste(names(registry), collapse = ", "))
  }

  # Run
  print(paste("Running updates:", paste(targets, collapse = ", ")))
  purrr::walk(targets, \(nm) {
    message("Running update: ", nm)
    registry[[nm]]()
  })

  invisible(NULL)
}
updates(targets = "rbnz", manual = TRUE)
updates(targets = "adp", manual = TRUE)
updates(targets = "fuel", manual = TRUE)
updates(targets = "bond", manual = TRUE)
updates(targets = "ect", manual = TRUE)
