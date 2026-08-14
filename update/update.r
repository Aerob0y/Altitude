


updates <- function(what = "", manual = FALSE) {
  if (!(manual || !interactive())) return(invisible(NULL))

  # Load helper functions
  source("update/update_utilities/update_libraries.r")
  source("update/update_utilities/update_etag.r")
  source("update/update_utilities/export_plotly_spec.r")

  # Load data utils + update modules (so functions exist)
  purrr::walk(
    list.files("app/r/utils", full.names = TRUE, pattern = "utils_data"),
    ~ source(.x, local = FALSE, echo = FALSE)
  )
  purrr::walk(
    list.files("update/update_modules", full.names = TRUE, pattern = "\\.r$"),
    ~ source(.x, local = FALSE, echo = FALSE)
  )

  # ---- Registry of runnable update functions ----
  # Add more entries here as you build modules
  registry <- list(
    adp = update_adp_data,
    bond   = update_bond_data,
    border = update_border,
    ect = download_latest_ect,
    fuel = update_fuel_data,
    rbnz = rbnz_fetch_all
  )

  # Normalise 'what'
  if (length(what) == 1 && is.character(what) && grepl(",", what, fixed = TRUE)) {
    what <- strsplit(what, ",", fixed = TRUE)[[1]]
  }
  what <- trimws(what)
  what <- what[what != ""]

  # Default behaviour:
  # - what == "" -> run "data" (i.e., everything in registry)
  # - what == "data" -> run everything in registry
  # - otherwise -> run only named functions
  if (length(what) == 0 || identical(what, "data")) {
    targets <- names(registry)
  } else {
    targets <- what
  }

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

updates()