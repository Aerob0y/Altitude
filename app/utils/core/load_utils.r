# Load shared application utilities in dependency order.


function_registry <- tibble::tibble(
  File = character(),
  Function = character(),
  Description = character()
)

register_function <- function(file, function_name, description) {

  function_registry <<-
    function_registry |>
    dplyr::filter(
      !(File == file & Function == function_name)
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        File = file,
        Function = function_name,
        Description = description
      )
    )
}

functions_by_file <- function(file = NULL, fp = TRUE) {

  x <- function_registry

  if (!is.null(file)) {
    x <- x |>
      dplyr::filter(File == file)
  }

  if (fp) {
    purrr::pwalk(
      x,
      \(File, Function, Description) {
        cat(
          "\n", Function, "\n",
          "  ", Description, "\n",
          sep = ""
        )
      }
    )
  }

  invisible(x)
}


.log <- new.env(parent = emptyenv())

.log$data <- tibble::tibble(
  Time = as.POSIXct(character()),
  Function = character(),
  Value = list()
)

add_log <- function(function_name, value = NULL) {

  .log$data <- dplyr::bind_rows(
    .log$data,
    tibble::tibble(
      Time = Sys.time(),
      Function = function_name,
      Value = list(value)
    )
  )

  invisible(NULL)
}

print_log <- function() {
  print(.log$data, n = Inf, width = Inf)
}

clear_log <- function() {
  .log$data <- .log$data[0, ]
  invisible(NULL)
}


register_function(
  "app/utils/core/load_utils.r",
  "register_function",
  "Registers a function in the function registry"
)
register_function(
  "app/utils/core/load_utils.r",
  "functions_by_file",
  "Lists functions registered in the function registry, optionally filtered by file"
)

register_function(
  "app/utils/core/load_utils.r",
  "add_log",
  "Adds a log entry for a function with an optional message"
)
register_function(
  "app/utils/core/load_utils.r",
  "print_log",
  "Prints the log of function calls and messages"
)

#
# Keep this list explicit: utility files define objects that are consumed by
# files later in the list, so recursively sourcing the directory would make
# startup order depend on file names.
utility_files <- c(
  "app/utils/core/dependencies.r",
  "app/utils/plotting/standards.r",
  "app/utils/shiny/download.r",
  "app/utils/data/data.r",
  "app/utils/plotting/plotly.r",
  "app/utils/plotting/standard_plot.r",
  "app/utils/core/launch_module.r"
)


invisible(lapply(
  utility_files,
  source,
  local = FALSE,
  echo = FALSE
))

rm(utility_files)

# Load modules
available_modules <- list.files("app/modules", full.names = FALSE, pattern = "\\.r$")
purrr::walk(
  list.files("app/modules", full.names = TRUE, pattern = "\\.r$"),
  ~ {
    source(.x, local = FALSE, echo = FALSE)
    add_log("load_utils", paste("Loaded module:", .x))
  }
)
