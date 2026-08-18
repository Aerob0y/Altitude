to_rgba <- function(col, alpha = 0.25) {
  rgb <- grDevices::col2rgb(col)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

assign_series_colours <- function(series) {
  if (!"Palette"   %in% names(series)) series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"

  s <- series
  s <- s %>% group_by(Palette, ColourKey)
  s <- s %>%
    mutate(
      .idx = row_number(),
      colour = case_when(
        Palette == "manual" ~ unname(cc[ColourKey]),

        Palette == "qual" ~
          pal_qual_main[(.idx - 1) %% length(pal_qual_main) + 1],

        Palette == "navy" ~
          palettes$navy[(.idx - 1) %% length(palettes$navy) + 1],

        Palette == "teal" ~
          palettes$teal[(.idx - 1) %% length(palettes$teal) + 1],

        Palette == "ruby" ~
          palettes$ruby[(.idx - 1) %% length(palettes$ruby) + 1],

        Palette == "gold" ~
          palettes$gold[(.idx - 1) %% length(palettes$gold) + 1],

        TRUE ~ palettes$grey["base"]
      )
    ) %>%
    ungroup() %>%
    select(-.idx)
  s
}

### Standard Plot ----------------------------
add_series <- function(p, data, s, axis_side, split = NULL) {
  stack_id <- "Test"
  if (!is.null(split)) {
    data <- dplyr::filter(data, .data[[split]] == s$Split)
    if (s$Style %in% c("Line")){
      stack_id <- paste0("one_", s$Split)  # keep split groups separate
    } else {
      stack_id <- "one"
    }
    nm <- s$Split
  } else {
    nm <- s$Name
  }
  if (s$Style == "Bar") {
    p <- p |> plotly::add_bars(
      y      = data[[s$ID]],
      name = nm,
      marker = list(color = to_rgba(s$colour, 1)),
      yaxis  = axis_side
    )
  } else if (s$Style == "Line" || s$Style == "Dashed") {

    print(stack_id)
    p <- p |> plotly::add_trace(
      data = data,
      y = ~.data[[s$ID]],
      name = nm,
      type = "scatter",
      mode = "lines",
      stackgroup = stack_id,
      line = list(
        #color = to_rgba(s$colour, 1),
        color = s$colour,
        dash  = if (s$Style == "Dashed") "dot" else "solid",
        shape = "spline"
      ),
      fill = "none",
      stackgroup = NULL,
      yaxis = axis_side
    )
  } else if (s$Style %in% c("Fill", "Area")) {
    p <- p |> plotly::add_trace(
      data = data,
      y = ~.data[[s$ID]],
      name = nm,
      type = "scatter",
      mode = "none",
      stackgroup = stack_id,
      fill = "tonexty",
      fillcolor = to_rgba(s$colour, 0.7),
      yaxis = axis_side
    )
  }
  p
}

standard_layout <- function(p, years = 10, titles = c("", ""), clean_ui = FALSE) {
  p |>
    plotly::layout(
      title     = standard_title(titles[1], titles[2]),
      legend    = standard_legend(),
      hovermode = "x unified",
      barmode   = "stack",
      margin    = standard_margin,
      xaxis     = standard_date_xaxis(y = years, clean_ui = clean_ui)
    )
}

x_yaxis <- function(p, unique_dims, yaxis_titles) {
  p <- p |> plotly::layout(
    yaxis  = standard_yaxis(
      list(
        tickformat = unique_dims$Tick[1],
        tickprefix = unique_dims$Prefix[1],
        title = yaxis_titles[1],
        c = unique_dims$colour[1]
      )
    ),
    yaxis2 = standard_yaxis(
      list(
        tickformat = unique_dims$Tick[2],
        tickprefix = unique_dims$Prefix[2],
        title = yaxis_titles[2],
        y = "secondary",
        c = unique_dims$colour[2]
      )
    )
  )
}



x_plotly <- function(
  data,
  titles       = c("", ""),
  yaxis_titles = c("", ""),
  series,
  split = NULL,
  k,
  years = 10,
  clean_ui = FALSE,
  download = list(format="png", width=2000, height=1200, scale=2)
) {
  # 1) Colour assignment ----------------------------------------------------
  series <- series |>
    dplyr::arrange(Style)
  ord <- order(factor(series$Style, levels = c("Fill", "Bar", "Line"), ordered = TRUE))
  series <- series[ord, ]

  if (!is.null(split)) {
    v <- data[[split]] %>% unique()
    series <- series |> tidyr::crossing(Split = v)
    series <- assign_series_colours(series)
  } else {
    series <- assign_series_colours(series)
  }

  unique_dims <- series %>% group_by(Dim, Tick, Prefix, Style) %>% summarise(colour  = first(colour), .groups = "keep")
  ord <- order(factor(unique_dims$Style, levels = c("Fill", "Bar", "Line"), ordered = TRUE))
  unique_dims <- unique_dims[ord, ]

  if (nrow(unique_dims) == 1) {unique_dims$colour <- c("black")}

  # 2) Base plot (one-liner split handling) --------------------------------
  p <- plotly::plot_ly(data, x  = ~ get(k), split = if (!is.null(split)) ~ get(split) else NULL)
  p <- p |> standard_layout(years = years, titles = titles, clean_ui = clean_ui) |> x_yaxis(unique_dims, yaxis_titles)

  # 4) Add series traces ----------------------------------------------------
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]
    axis_side <- if (unique_dims$Dim[1] == s$Dim[1]) "y" else "y2"

    p <- p %>% add_series(data, s, axis_side, split)
  }

  # Assuming each trace has $name that matches guide_rbnz$Names (or similar)
  trace_names <- vapply(p$x$data, function(tr) tr$name %||% NA_character_, character(1))
  trace_style <- guide_rbnz$Style[match(trace_names, guide_rbnz$Name)]
  # Default anything unknown to "Line" so it stays on top
  trace_style[is.na(trace_style)] <- "Line"
  ord <- order(factor(trace_style, levels = c("Fill", "Bar", "Line"), ordered = TRUE))
  p$x$data <- p$x$data[ord]

  dl <- list(
    format = as.character(download$format),
    width  = as.numeric(download$width),
    height = as.numeric(download$height),
    scale  = as.numeric(download$scale)
  )

  p <- plotly::config(
    p,
    displayModeBar = TRUE,
    toImageButtonOptions = dl
  )
  p
}
