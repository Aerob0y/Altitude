to_rgba <- function(colour, alpha = 0.25) {
  rgb <- grDevices::col2rgb(colour)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

assign_series_colours <- function(series) {
  series |>
    dplyr::group_by(Palette, ColourKey) |>
    dplyr::mutate(
      colour = if (dplyr::first(Palette) == "manual") {
        unname(cc[ColourKey])
      } else {
        palette <- palettes[[dplyr::first(Palette)]]
        palette[(dplyr::row_number() - 1) %% length(palette) + 1]
      }
    ) |>
    dplyr::ungroup()
}

normalise_standard_series <- function(series) {
  aliases <- list(
    DatasetColumn = c("ID"),
    DisplayName = c("ColumnName", "Name"),
    Dim_Group = c("Dim"),
    Style = c("style"),
    Palette = c("Pallete", "palette", "pallete"),
    ColourKey = c("colourKey", "colour_key", "colour")
  )

  for (target in names(aliases)) {
    source <- aliases[[target]][aliases[[target]] %in% names(series)][1]
    if (!target %in% names(series) && !is.na(source)) {
      series[[target]] <- series[[source]]
    }
  }

  defaults <- list(
    DisplayName = NA_character_, Dim_Group = "value", Tick = ".2f",
    Prefix = "", Palette = "qual", ColourKey = "teal_base",
    Style = "Line", Stack = FALSE, Rollup = "Sum", Row = NA_real_
  )
  for (column in names(defaults)) {
    if (!column %in% names(series)) series[[column]] <- defaults[[column]]
    # Guide columns may be factors (notably in the legacy RBNZ guide). Convert
    # textual settings before filling blanks so supplied values are retained.
    if (is.character(defaults[[column]])) series[[column]] <- as.character(series[[column]])
    series[[column]][is.na(series[[column]]) | series[[column]] == ""] <- defaults[[column]]
  }

  # Both guides have historically used a mixture of title and lower case.
  # Plot construction uses the title-case style names below.
  series$Style <- tools::toTitleCase(tolower(series$Style))
  series$Palette <- tolower(series$Palette)

  series$DisplayName[is.na(series$DisplayName)] <- series$DatasetColumn[is.na(series$DisplayName)]
  series
}

rollup_standard_series <- function(data, value_col, rollup, group_col) {
  summarise_value <- function(values, weights = NULL) {
    if (all(is.na(values))) return(NA_real_)
    if (is.null(weights)) return(sum(values, na.rm = TRUE))
    keep <- !is.na(values) & !is.na(weights)
    if (!any(keep) || sum(weights[keep]) == 0) return(NA_real_)
    stats::weighted.mean(values[keep], weights[keep])
  }

  summarise_group <- function(group, key) {
    values <- group[[value_col]]
    value <- if (all(is.na(values))) {
      NA_real_
    } else if (rollup == "Min") {
      min(values, na.rm = TRUE)
    } else if (rollup == "Avg") {
      mean(values, na.rm = TRUE)
    } else if (rollup == "Max") {
      max(values, na.rm = TRUE)
    } else if (rollup %in% names(group)) {
      summarise_value(values, group[[rollup]])
    } else {
      summarise_value(values)
    }
    tibble::tibble(value = value)
  }

  data |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::group_modify(summarise_group) |>
    dplyr::ungroup()
}

standard_plot <- function(
  data,
  reference = NULL,
  chart = NULL,
  titles = c("", ""),
  yaxis_titles = c("", ""),
  series,
  split_columns = NULL,
  split_by = NULL,
  k,
  years = 10,
  clean_ui = FALSE,
  area_fill = "tozeroy",
  area_opacity = 0.7,
  smooth_lines = TRUE,
  download = list(format = "png", width = 2000, height = 1200, scale = 2)
) {
  series <- normalise_standard_series(series)
  if (!nrow(series)) return(plotly::plot_ly())

  if (!is.null(split_by)) {
    split_columns <- split_columns %||% series$DatasetColumn
    split_values <- unique(as.character(data[[split_by]]))
    split_series <- series |>
      dplyr::filter(DatasetColumn %in% split_columns) |>
      tidyr::crossing(Split = split_values)
    unsplit_series <- series |>
      dplyr::filter(!DatasetColumn %in% split_columns) |>
      dplyr::mutate(Split = NA_character_)
    series <- dplyr::bind_rows(unsplit_series, split_series)
  } else {
    series$Split <- NA_character_
  }

  series <- series |>
    dplyr::arrange(Row) |>
    dplyr::arrange(factor(Style, levels = c("Fill", "Area", "Bar", "Dashed", "Line"))) |>
    assign_series_colours() |>
    dplyr::mutate(
      Axis = dplyr::if_else(Dim_Group == dplyr::first(Dim_Group), "y", "y2")
    )

  dimensions <- series |>
    dplyr::group_by(Axis, Dim_Group) |>
    dplyr::summarise(Tick = dplyr::first(Tick), Prefix = dplyr::first(Prefix), colour = dplyr::first(colour), .groups = "drop") |>
    dplyr::arrange(Axis)

  p <- plotly::plot_ly()
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]
    if (!is.na(s$Split)) {
      trace_data <- data |> dplyr::filter(as.character(.data[[split_by]]) == s$Split)
      x <- trace_data[[k]]
      y <- trace_data[[s$DatasetColumn]]
      trace_name <- paste0(s$DisplayName, " (", s$Split, ")")
    } else {
      trace_data <- rollup_standard_series(data, s$DatasetColumn, s$Rollup, k)
      x <- trace_data[[k]]
      y <- trace_data$value
      trace_name <- s$DisplayName
    }

    common <- list(p = p, x = x, y = y, name = trace_name, yaxis = s$Axis)
    if (s$Style == "Bar") {
      common$marker <- list(color = s$colour)
      p <- do.call(plotly::add_bars, common)
    } else {
      common$type <- "scatter"
      common$mode <- if (s$Style %in% c("Fill", "Area")) "none" else "lines"
      common$line <- list(
        color = s$colour,
        dash = if (s$Style == "Dashed") "dot" else "solid",
        shape = if (isTRUE(smooth_lines)) "spline" else "linear"
      )
      if (isTRUE(s$Stack)) common$stackgroup <- paste0(s$Axis, "_stack")
      if (s$Style %in% c("Fill", "Area")) {
        common$fillcolor <- to_rgba(s$colour, area_opacity)
        if (!isTRUE(s$Stack)) common$fill <- area_fill
      }
      p <- do.call(plotly::add_trace, common)
    }
  }

  if (nrow(dimensions) == 1) dimensions$colour <- "black"
  p <- p |>
    standard_layout(years = years, titles = titles, clean_ui = clean_ui) |>
    x_yaxis(dimensions, yaxis_titles)

  download <- list(
    format = as.character(download$format %||% "png"),
    width = as.numeric(download$width %||% 2000),
    height = as.numeric(download$height %||% 1200),
    scale = as.numeric(download$scale %||% 2)
  )
  print("standard plot used")
  plotly::config(p, displayModeBar = TRUE, toImageButtonOptions = download)
}
