
####standardise_data ----
pivot_reference <- function(data, reference, dataset) {

  spec <- if (is.character(reference) && length(reference) == 1L) {
    jsonlite::fromJSON(reference, simplifyVector = FALSE)
  } else {
    reference
  }

  ds <- spec$datasets[[dataset]]

  if (is.null(ds)) {
    stop(
      "Dataset not found in reference: ", dataset,
      call. = FALSE
    )
  }

  id_cols <- if (is.null(ds$id_cols)) {
    "Date"
  } else {
    unlist(ds$id_cols, use.names = FALSE)
  }

  out <- purrr::map(
    ds$structures,
    function(structure) {

      ids <- purrr::map_chr(structure$Columns, "ID")

      missing <- setdiff(ids, names(data))

      if (length(missing)) {
        stop(
          dataset, ": IDs not found: ",
          paste(missing, collapse = ", "),
          call. = FALSE
        )
      }

      column_meta <- purrr::map_dfr(
        structure$Columns,
        tibble::as_tibble_row
      )

      structure_meta <- structure[
        setdiff(names(structure), c("Columns", "name"))
      ]

      x <- data |>
        dplyr::select(
          dplyr::all_of(id_cols),
          dplyr::all_of(ids)
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(ids),
          names_to = "ID",
          values_to = "Value"
        ) |>
        dplyr::left_join(
          column_meta,
          by = "ID"
        )

      for (nm in names(structure_meta)) {
        x[[nm]] <- structure_meta[[nm]]
      }

      x
    }
  )

  names(out) <- purrr::map_chr(
    ds$structures,
    \(x) {
      if (!is.null(x$name)) x$name else x$Group
    }
  )

  out
}


hm14_data <- pivot_reference(
  hm14,
  "app/config/structure.json",
  "hm14"
)
hm14_data %>% names()
