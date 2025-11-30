

generic_plotly2 <- function(
    flags = FALSE,
    data,
    t1 = "Example Plotly Chart",
    t2 = "",
    series,
    dim = NULL,
    fallback = FALSE,
    split = NULL,
    k) {

  if (is.null(split)) {p <- plotly::plot_ly(data, x = ~get(k))}
  else {p <- plotly::plot_ly(data, x = ~get(k), split = ~get(split))}
  p <- custom_download_button(p)
  tick_format_1 <- ".2f"
  unique_dims <- series |> select(Dim) |> unique()
  unique_groups <- series |> select(Grouping, Dim) |> unique()
  title_1 <- ""
  title_2 <- ""
  if (length(unique(unique_groups$Grouping)) == 2) {
    title_1 <- unique_groups$Grouping[1]
    title_2 <- unique_groups$Grouping[2]
  } else if (length(unique(unique_dims$Dim)) == 2) {
    title_1 <- unique_dims$Dim[1]
    title_2 <- unique_dims$Dim[2]
  } else if (length(unique_groups$Dim) == 2) {
    title_1 <- paste(unique_groups$Grouping[1],": (", unique_dims$Dim[1], ")", sep = "")
    title_2 <- paste(unique_groups$Grouping[2],": (", unique_dims$Dim[2], ")", sep = "")
  } else {
    title_1 <- if (is.na(unique_groups$Grouping[1])) {
      unique_dims$Dim[1]
    } else {
      paste(unique_groups$Grouping[1],": (", unique_dims$Dim[1], ")", sep = "")
    }
  }

  cx <- c('teal_base','navy_base','ruby_base', 'gold_base', 'grey_base')
  navy_spread <- c('navy_dark','teal_base','navy_base','navy_lite','navy_snow')


  axis_color_1 <- "black"
  axis_color_2 <- NULL
  prefix1 <- ""
  prefix2 <- ""
  

  f <- series[series$Dim == unique_dims$Dim[1], ][1, ]
  if (length(unique_dims$Dim) >= 2) {
    axis_color_1 <- f$Primary
    if (axis_color_1 == "number") {axis_color_1 <- cx[1]}
  }
  if(f$Tick == "comma"){
    print("here")
  }
  #switch(f$Tick,
  #  ".0f" = {tick_format_1 <- ".0f"},
  #  ".1f" =  {tick_format_1 <- ".1f"},
   # ".2f" =  {tick_format_1 <- ".2f"},
  #  "comma" = {tick_format_1 <- "comma"},
  #  {tick_format_1 <- ".2f"}
  #)

  tick_format_1 <- f$Tick
  prefix1 <- f$Prefix

  if (length(unique_dims$Dim) >= 2) {
    f <- series[series$Dim == unique_dims$Dim[2], ][1, ]
    tick_format_2 <- f$Tick
    axis_color_2 <- f$Secondary
    if (axis_color_2 == "number") {axis_color_2 <- cx[2]}
  }

  i <- 1
  for (key in series$Series.Id) {
    print(key)

    s <- series[series$Series.Id == key, ]
    axis_side <- if (is.null(unique_dims$Dim[1]) || unique_dims$Dim[1] == s$Dim[1]) "y" else "y2"
    switch(s$Primary,
      "number" = {col_use <- cc[[cx[i]]]},
      "navyspread" =  {col_use <- cc[[navy_spread[i]]]},
      {col_use <- if (axis_side == "y") cc[[s$Primary]] else cc[[s$Secondary]]}
    )

    #if (s$Primary == "number"){
    #  col_use <- cc[[cx[i]]]
    #} else {
    #  col_use <- if (axis_side == "y") cc[[s$Primary]] else cc[[s$Secondary]]
    #}

    if (!is.null(s)) {

      if (s$Style == "Bar") {
        # BAR / STACKED BAR
        p <- p |>
          add_bars(
            y     = data[[s$Series.Id]],
            name  = s$Names,
            marker = list(color = col_use),
            yaxis = axis_side
          )

      } else {
        # LINES / FILLED AREAS
        #print("lines")
        #print(data[[s$Series.Id]])
        #print(col_use)
        p <- p |>
          add_trace(
            y    = data[[s$Series.Id]],
            name = s$Names,
            type = "scatter",
            mode = "lines",
            line = list(
              color = col_use,
              dash  = if (s$Style == "Dashed") "dot" else "solid",
              shape = "spline"
            ),
            stackgroup = if (s$Style == "Fill") "one" else NULL,
            fillcolor  = if (s$Style == "Fill") col_use else NULL,
            yaxis = axis_side
          )
      }
    }
    p <- p |> layout(barmode = "stack")

    #if (!is.null(s)) {
     # p <- p |> add_trace(
     #   y    = data[[s$Series.Id]],
     #   name = s$Names,
     #   type = "scatter",
     #   mode = "lines",
     #   line = list(color = col_use, dash = if(s$Style == "Dashed") "dot" else "solid", shape = "spline"),
     #   stackgroup = if (s$Style == "Fill") "one" else NULL,
     #   fillcolor = if(s$Style == "Fill") col_use else NULL,
     #   yaxis = axis_side
     # )
    #}
    i <- i + 1
  }
    p <- p |>
      plotly::layout(
        title  = standard_title(t1, t2),
        hovermode = "x unified",
        margin = standard_margin,
        xaxis  = standard_date_xaxis(y = 10),
        yaxis  = standard_yaxis(
          #title      = if (group_count > 1) dim[1] else series[series$Dim == unique_dims$Dim[1], ][['Grouping']][1],
          title = title_1,
          tickprefix = prefix1,
          c = axis_color_1,
          tickformat = tick_format_1
        )
      )
    p <- p |>
      plotly::layout(legend = standard_legend())
  if (length(unique_dims$Dim) >= 2) {
    p <- p |>
      plotly::layout(
        yaxis2 = standard_yaxis(
          #title      = dim[2],
          title = title_2,
          tickprefix = prefix2,
          tickformat = tick_format_2,
          y = "secondary",
          c = axis_color_2
        ),
        legend = standard_legend()
      )
  }
  p
}