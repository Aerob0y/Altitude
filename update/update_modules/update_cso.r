library(readr)
library(jsonlite)
library(dplyr)


# Fetch recent periods ----------------------------------------------------

cso_fetch <- function(
  matrix,
  periods = 12,
  time_code = "TLIST(M1)",
  language = "en",
  release = 36,
  product = "NDC"
) {

  query <- list(
    query = list(
      list(
        code = time_code,
        selection = list(
          filter = "fluid",
          values = 0:(periods - 1)
        )
      )
    ),
    response = list(
      format = "csv",
      pivot = NULL,
      codes = FALSE
    )
  )

  query_json <- jsonlite::toJSON(
    query,
    auto_unbox = TRUE,
    null = "null"
  )

  query_encoded <- httpuv::encodeURI(query_json)

  url <- paste0(
    "https://ws.cso.ie/public/api.restful/",
    "PxStat.Data.Cube_API.PxAPIv1/",
    language, "/",
    release, "/",
    product, "/",
    matrix,
    "?query=",
    query_encoded
  )

  readr::read_csv(
    url,
    show_col_types = FALSE
  )
}

# Fetch complete history -------------------------------------------------

cso_fetch_all <- function(
  matrix,
  language = "en"
) {

  url <- paste0(
    "https://ws.cso.ie/public/api.restful/",
    "PxStat.Data.Cube_API.ReadDataset/",
    matrix,
    "/CSV/1.0/",
    language
  )
  print(url)
  readr::read_csv(
    url,
    show_col_types = FALSE
  )
}

# Update local CSO dataset ------------------------------------------------

cso_update <- function(
  matrix,
  periods = 12,
  time_code = "TLIST(M1)",
  language = "en",
  release = 36,
  product = "NDC",
  base_dir = "app/data/CSO"
) {

  matrix <- toupper(matrix)

  # File locations
  data_dir <- file.path(base_dir, matrix)

  csv_file <- file.path(
    data_dir,
    paste0(matrix, ".csv")
  )

  rds_file <- file.path(
    data_dir,
    paste0(matrix, ".rds")
  )


  # Create dataset directory
  dir.create(
    data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )


  # First run ------------------------------------------------------------

  if (!file.exists(csv_file)) {

    message(
      matrix,
      ": no local CSV found - downloading full history"
    )

    data <- cso_fetch_all(
      matrix = matrix,
      language = language
    )

  } else {

    # Incremental update -------------------------------------------------

    message(
      matrix,
      ": refreshing latest ",
      periods,
      " periods"
    )

    old <- readr::read_csv(
      csv_file,
      show_col_types = FALSE
    )

    new <- cso_fetch(
      matrix = matrix,
      periods = periods,
      time_code = time_code,
      language = language,
      release = release,
      product = product
    )


    # Checks
    if (!time_code %in% names(new)) {
      stop(
        matrix,
        ": time column '",
        time_code,
        "' not found in downloaded data."
      )
    }

    if (!time_code %in% names(old)) {
      stop(
        matrix,
        ": time column '",
        time_code,
        "' not found in local CSV."
      )
    }

    if (nrow(new) == 0) {
      stop(
        matrix,
        ": CSO returned no data. Local files unchanged."
      )
    }


    # Which periods have been refreshed?
    refresh_periods <- unique(
      new[[time_code]]
    )

    message(
      matrix,
      ": replacing ",
      length(refresh_periods),
      " periods from ",
      min(refresh_periods),
      " to ",
      max(refresh_periods)
    )


    # Remove old versions of refreshed periods
    old <- old |>
      filter(
        !.data[[time_code]] %in% refresh_periods
      )


    # Append refreshed data
    data <- bind_rows(
      old,
      new
    )
  }


  # Sort by period
  data <- data |>
    arrange(.data[[time_code]])


  # Save master CSV
  readr::write_csv(
    data,
    csv_file
  )


  # Save fast R version
  saveRDS(
    data,
    rds_file
  )


  message(
    matrix,
    ": saved ",
    nrow(data),
    " rows"
  )

  invisible(data)
}

# Construction   -------------------------------------------------
update_ndm01 <- function() {
  cso_update(
    matrix = "NDM01",
    release = 36,
    product = "NDC"
  )
}

ndm01 <- readRDS("app/data/CSO/NDM01/NDM01.rds")

#update_ndm01()

# Population   -------------------------------------------------
update_pea01 <- function() {
  cso_update(
    matrix = "PEA01",
    release = 36,
    product = "PME",
    time_code = "Year"
  )
}
update_pea01()

# Population   -------------------------------------------------
update_pea25 <- function() {
  cso_update(
    matrix = "PEA25",
    release = 2,
    product = "PME",
    time_code = "Year"
  )
}
update_pea25()

# Tourism    -------------------------------------------------
update_hta05 <- function() {
  cso_update(
    matrix = "HTA05",
    release = 2,
    product = "PME",
    time_code = "Year"
  )
}
update_hta05()


update_ctm01  <- function() {
  cso_update(
    matrix = "CTM01",
    release = 2,
    product = "PMAP",
    time_code = "Month"
  )
}
update_ctm01()

ITM01-ITM06
ASM01-ASM03
#TAM03 - Passenger, Freight and Commercial Flights\
TAM03-TAM05,TAM07
QLF18 
QLF27 
EHQ03 
CPM02 
CPM20
CPM22
HPQ01 
GFQ03
GFQ12 
QLF50 
QLF51 
GFA03 
GFA04 
NDA08 
FY003B  
F2020  

https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/37/PMAP/CTM01?query=%7B%22query%22:%5B%7B%22code%22:%22TLIST(M1)%22,%22selection%22:%7B%22filter%22:%22fluid%22,%22values%22:%5B0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178%5D%7D%7D%5D,%22response%22:%7B%22format%22:%22csv%22,%22pivot%22:null,%22codes%22:false%7D%7D
https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/CTM01/CSV/1.0/en
https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/37/PMAP/CTM01?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22csv%22,%22pivot%22:null,%22codes%22:false%7D%7D