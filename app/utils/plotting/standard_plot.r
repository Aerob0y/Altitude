to_rgba <- function(col, alpha = 0.25) {
  rgb <- grDevices::col2rgb(col)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

### assign_series_colours: Assigns colours to series based on the specified palette and colour key ----
assign_series_colours <- function(series) {

  if (!"Palette" %in% names(series)) {
    series$Palette <- "qual"
  }

  if (!"ColourKey" %in% names(series)) {
    series$ColourKey <- "teal_base"
  }

  series |>
    dplyr::group_by(Palette) |>
    dplyr::mutate(
      colour = if (dplyr::first(Palette) == "manual") {

        unname(cc[ColourKey])

      } else {

        pal <- palettes[[dplyr::first(Palette)]]

        key_no <- match(
          ColourKey,
          unique(ColourKey)
        )

        pal[(key_no - 1) %% length(pal) + 1]
      }
    ) |>
    dplyr::ungroup()
}

rollup_series <- function(data, value_col, rollup, group_col) {
  data |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::group_modify(function(.x, .y) {
      x <- .x[[value_col]]
      value <- if (is.na(rollup) || rollup == "" || rollup == "Sum") { sum(x, na.rm = TRUE)
      } else if (rollup == "Min") {
        min(x, na.rm = TRUE)
      } else if (rollup == "Avg") {
        mean(x, na.rm = TRUE)
      } else if (rollup == "Max") {
        max(x, na.rm = TRUE)
      } else if (rollup %in% names(.x)) {
        sum(x * .x[[rollup]], na.rm = TRUE) /  sum(.x[[rollup]], na.rm = TRUE)
      } else {
        sum(x, na.rm = TRUE)
      }
      tibble::tibble(value = value)
    }) |>
    dplyr::ungroup()
}

standard_plot_defaults <- list(
  title = "",
  subtitle = "",
  x = "Date",
  format = "wide",
  value = NULL,
  series = NULL,
  style = "line",
  stack = FALSE,
  years = 10,
  y_title = "",
  y2_title = "",
  tickformat = ".2f",
  prefix = "",
  download = list(format = "png", width = 2000, height = 1200, scale = 2)
)

standard_plot <- function(
  data,
  reference,
  chart = NULL,
  titles       = c("", ""),
  yaxis_titles = c("", ""),
  series,
  split_columns = NULL,
  split_by = NULL,
  k,
  years = 10,
  clean_ui = FALSE,
  download = list(format = "png", width = 2000, height = 1200, scale = 2)
) {

  print(split_by)



  if (!"Palette" %in% names(series))   series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"
  series <- series |>
    dplyr::mutate(
      Palette = dplyr::if_else(is.na(Palette) | Palette == "", "qual", Palette),
      ColourKey = dplyr::if_else(is.na(ColourKey) | ColourKey == "", "teal_base", ColourKey)
    )
  #if a split is specified, cut the data

  if ((!is.null(split_by))) {



    split_series <- series |>
      dplyr::filter(DatasetColumn %in% split_columns) |>
      tidyr::crossing(Split = unique(data[[split_by]])) |>
      dplyr::mutate(
        ColourKey = paste0(ColourKey, "_", Split)
      )

    non_split_series <- series |>
      dplyr::filter(!DatasetColumn %in% split_columns) |>
      dplyr::mutate(Split = NA_character_)

    series <- dplyr::bind_rows(
      non_split_series,
      split_series
    )
  }

  # Add Style to series if not present
  if (!"Style" %in% names(series)) series$Style <- "Line"
  series <- series[order(factor(series$Style, levels = c("Fill", "Bar", "Line"))), ]

  # asign colours to series
  series <- assign_series_colours(series)


  # asign sides to series
  dims <- unique(series$Dim_Group)
  unique_dims <- series %>% group_by(Dim_Group, Tick, Prefix) %>% summarise(colour  = first(colour), .groups = "keep")
  series <- series %>% mutate(Axis = if_else(Dim_Group == dims[1], "y", "y2"))

  # Always guarantee this column exists
if (!"Split" %in% names(series)) {
  series$Split <- NA_character_
}

  # Create plotly object
  p <- plotly::plot_ly()
  # Add traces to plotly object
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]

    if (!is.null(split_by) && !is.na(s$Split)) {

      d <- data |>
        dplyr::filter(.data[[split_by]] == s$Split)

    } else {

      d <- rollup_series(
        data      = data,
        value_col = s$DatasetColumn,
        rollup    = s$Rollup,
        group_col = k
      )
    }
    common <- list(
      p     = p,
      x = d[[k]],
      y = if ("value" %in% names(d)) d$value else d[[s$DatasetColumn]],
      name = if (!is.na(s$Split)) {
        paste0(s$DatasetColumn, " (", s$Split, ")")
      } else {
        s$DatasetColumn
      },
      yaxis = s$Axis,
      line = list(
        color = s$colour,
        dash = if (s$Style == "Dashed") "dot" else "solid"
      )
    )

    if (series$Style[i] == "Bar") {
      common$marker <- list(color = series$colour[i])
      common$line <- NULL
      p <- do.call(plotly::add_bars, common)
    } else {

      common$type <- "scatter"
      common$mode <- if (s$Style == "Area") "none" else "lines"

      if (isTRUE(s$Stack)) {
        common$stackgroup <- paste0(s$Axis, "_stack")
      }

      if (s$Style == "Area" && !isTRUE(s$Stack)) {
        common$fill <- "tozeroy"
      }

      p <- do.call(plotly::add_trace, common)
    }
  }

  # 4. Apply one consistent layout and download configuration --------------
  p <- p |> standard_layout(years = years, titles = titles, clean_ui = clean_ui) |> x_yaxis(unique_dims, yaxis_titles)

  plotly::config(p, displayModeBar = TRUE, toImageButtonOptions = download)
}