# Run the Shiny Application
#guide_rbnz <- openxlsx::read.xlsx("reference/RBNZ_Series.xlsx", detectDates = TRUE, sheet = "Series Definitions", startRow = 1, skipEmptyRows = TRUE)
#saveRDS(guide_rbnz, file = "reference/RBNZ_Series.rds", compress = FALSE)

source("r/run_app.R", local = TRUE, echo = TRUE)
run_app()


