


generic_plotly <- function(
    data,
    t1 = "Example Plotly Chart",
    t2 = "",
    series,
    dim = NULL,
    fallback = FALSE,
    k) {
  p <- plotly::plot_ly(data, x = ~get(k))
  p <- custom_download_button(p)
  tick_format_1 <- ".2f"
  unique_dims <- series %>% select(Dim) %>% unique()
  unique_groups <- series %>% select(Grouping, Dim) %>% unique()
  title_1 <- ""
  title_2 <- ""
  if (length(unique(unique_groups$Grouping)) == 2) {
    print("X")
    title_1 <- unique_groups$Grouping[1]
    title_2 <- unique_groups$Grouping[2]
  } else if (length(unique(unique_dims$Dim)) == 2) {
    print("Y")
    title_1 <- unique_dims$Dim[1]
    title_2 <- unique_dims$Dim[2]
  } else if (length(unique_groups$Dim) == 2) {
    print("W")
    title_1 <- paste(unique_groups$Grouping[1],": (", unique_dims$Dim[1], ")", sep = "")
    title_2 <- paste(unique_groups$Grouping[2],": (", unique_dims$Dim[2], ")", sep = "")
  } else {
    print("Z")
    title_1 <- if (is.na(unique_groups$Grouping[1])) {
      unique_dims$Dim[1]
    } else {
      print("ZZ")
      paste(unique_groups$Grouping[1],": (", unique_dims$Dim[1], ")", sep = "")
    }
  }


  axis_color_1 <- NULL

  if (length(unique_dims$Dim) >= 2) {
    f <- series[series$Dim == unique_dims$Dim[1], ][1, ]
    axis_color_1 <- f$Primary
    tick_format_1 <- f$Tick
    f <- series[series$Dim == unique_dims$Dim[2], ][1, ]
    tick_format_2 <- f$Tick
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
          #title      = if (group_count > 1) dim[1] else series[series$Dim == unique_dims$Dim[1], ][['Grouping']][1],
          title = title_1,
          #tickprefix = ,
          c = axis_color_1,
          tickformat = s$Tick
        )
      )
    p <- p %>%
      plotly::layout(legend = standard_legend())
  if (length(unique_dims$Dim) >= 2) {
    p <- p %>%
      plotly::layout(
        yaxis2 = standard_yaxis(
          #title      = dim[2],
          title = title_2,
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

rbnz_series <- function(col = NULL, filter_graph, filter_group = NULL, filter_series = NULL, split = NULL, dim = NULL, adj = NULL) {
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
    filter(if (!is.null(adj)) Adj == adj else TRUE) %>%
    select(all_of(col)) %>%
    unique() %>%
    unlist() %>%
    as.vector()
}







rbnz_plotly <- function(g, group = NULL, group_fallback = NULL,  split = NULL, split_fallback = NULL, adj = NULL, dim = NULL, dim_fallback = NULL, title = NULL, subtitle = NULL, names = NULL, names_fallback = NULL) {
  d <- .load_data(g)
  s <- .cache$series
  fallback <- FALSE
  # Apply filters
  if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
  if (!is.null(group)) {s <- s %>% filter(Grouping %in% group)}
  if (!is.null(split))    {s <- s %>% filter(Split %in% split)}
  if (!is.null(dim))      {s <- s %>% filter(Dim %in% dim)}
  if (!is.null(names))      {s <- s %>% filter(Names %in% names)}
  if (!is.null(adj))      {s <- s %>% filter(Adj %in% adj)}
  if (nrow(s) == 0) {
    s <- .cache$series
    fallback <- TRUE
    if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
    if (!is.null(group_fallback)) {s <- s %>% filter(Grouping %in% group_fallback)}
    if (!is.null(split_fallback))    {s <- s %>% filter(Split %in% split_fallback)}
    if (!is.null(dim_fallback))      {s <- s %>% filter(Dim %in% dim_fallback)}
    if (!is.null(names_fallback))      {s <- s %>% filter(Names %in% names_fallback)}
  }

  unique_dims <- s %>% select(Dim) %>% unique()
  generic_plotly(
    data = d,
    series = s,
    k = "Date",
    t1 = title,
    t2 = subtitle,
    dim = unique_dims$Dim,
    fallback = fallback
  )
}
