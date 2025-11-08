

.cache$series <- openxlsx::read.xlsx("reference/RBNZ_Series.xlsx", detectDates = TRUE, sheet = "Series Definitions", startRow = 1, skipEmptyRows = TRUE)

generic_plotly <- function(
    data,
    t1 = "Example Plotly Chart",
    t2 = "",
    series,
    dim = NULL,
    k) {
  p <- plotly::plot_ly(data, x = ~get(k))
  p <- custom_download_button(p)
  tick_format_1 <- ".2f"
  unique_dims <- series %>% select(Dim) %>% unique()
  axis_color_1 <- NULL
  if (length(unique_dims$Dim) >= 2) {
    f <- series[series$Dim == unique_dims$Dim[2], ][1, ]
    tick_format_2 <- f$Tick
    axis_color_1 <- f$Primary
    axis_color_2 <- f$Secondary
  }

  for (key in series$Series.Id) {
    s <- series[series$Series.Id == key, ]
    axis_side <- if (is.null(dim) || dim[1] == s$Dim[1]) "y" else "y2"
    if (!is.null(s)) {
      p <- p %>% add_trace(
        y    = data[[s$Series.Id]],
        name = s$Names,
        type = "scatter",
        mode = "lines",
        line = if(axis_side == "y") list(color = cc[[s$Primary]]) else list(color = cc[[s$Secondary]]),
        yaxis = axis_side
      )
    }
  }
    p <- p %>%
      plotly::layout(
        title  = standard_title(t1, t2),
        hovermode = "x unified",
        margin = standard_margin,
        xaxis  = standard_date_xaxis(y = 10),
        yaxis  = standard_yaxis(
          title      = dim[1],
          #tickprefix = ,
          c = axis_color_1,
          tickformat = s$Tick
        )
      )
  if (length(unique_dims$Dim) >= 2) {
    p <- p %>%
      plotly::layout(
        yaxis2 = standard_yaxis(
          title      = dim[2],
          #tickprefix = #s2$prefix,
          tickformat = ".2f",
          y = "secondary",
          c = axis_color_2
        ),
        legend = standard_legend()
      )
  }
  p
}

rbnz_series <- function(col = NULL, filter_graph, filter_group = NULL, filter_series = NULL, split = NULL, dim = NULL) {
  if (!is.null(col) && !(col %in% names(.cache$series))) {
    stop(sprintf("Unknown column '%s'. Try one of: %s",
                 col, paste(names(.cache$series), collapse = ", ")))
  }
  .cache$series %>%
    filter(Graph == filter_graph) %>%
    filter(if (!is.null(filter_group)) Grouping == filter_group else TRUE) %>%
    filter(if (!is.null(filter_series)) Series.Id == filter_series else TRUE) %>%
    filter(if (!is.null(split)) Split == split else TRUE) %>%
    filter(if (!is.null(dim)) Dim %in% dim else TRUE) %>%
    select(all_of(col)) %>%
    unique() %>%
    unlist() %>%
    as.vector()
}

rbnz_plotly <- function(g, grouping = NULL, split = NULL, dim = NULL, title = NULL, subtitle = NULL) {
  d <- .load_data(g, refresh = FALSE)
  s <- .cache$series

  # Apply filters
  if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
  if (!is.null(grouping)) {s <- s %>% filter(Grouping %in% grouping)}
  if (!is.null(split))    {s <- s %>% filter(Split %in% split)}
  if (!is.null(dim))      {s <- s %>% filter(Dim %in% dim)}

  unique_dims <- s %>% select(Dim) %>% unique()

  generic_plotly(
    data = d,
    series = s,
    k = "Date",
    t1 = title,
    t2 = subtitle,
    dim = unique_dims$Dim
  )
}
