cc <- list(
  teal_base  = "#53868B",
  teal_dark  = "#436D71",
  teal_lite  = "#709BA1",
  teal_snow  = "#A3BDC1",
  navy_base  = "#141F52",
  navy_dark  = "#0F183F",
  navy_lite  = "#3F4B74",
  navy_snow  = "#858CAF",
  ruby_base  = "#8B2323",
  ruby_dark  = "#6E1B1B",
  ruby_lite  = "#A84848",
  ruby_snow  = "#C77C7C",
  gold_base  = "#F9C31F",
  gold_dark  = "#CFA218",
  gold_lite  = "#FCD45B",
  gold_snow  = "#FDEB9C",
  grey_dark  = "#333333", # gridlines
  grey_base  = "#666666", # text and lines
  grey_lite  = "#999999" # light grey for text on dark backgrounds
)

custom_download_button <- function(p) {
  p <- p |> config(
    modeBarButtonsToRemove = list("toImage"),
    modeBarButtonsToAdd = list(
      list(
        name  = "Download (clean)",
        title = "Download without range buttons",
        icon  = htmlwidgets::JS("Plotly.Icons.camera"),
        click = htmlwidgets::JS(
          "function(gd){
             var opts = {format:'png', width:2000, height:1200, scale:3};
             var off = {
               'xaxis.rangeselector.visible': false,
               'xaxis.rangeslider.visible': false
             };
             Plotly.relayout(gd, off)
               .then(function(){ return Plotly.downloadImage(gd, opts); })
               .then(function(){
                 // restore whatever you had before
                 Plotly.relayout(gd, {
                   'xaxis.rangeselector.visible': true
                 });
               });
           }")
      )
    )
  )
  p
}

button_years_list <- list(
  list(count = 10, label = "10 yr", step = "year", stepmode = "backward"),
  list(count = 5,  label = "5 yr",  step = "year", stepmode = "backward"),
  list(count = 1,  label = "1 yr",  step = "year", stepmode = "backward"),
  list(count = 1,  label = "YTD",   step = "year", stepmode = "todate"),
  list(step = "all")
)
rangeselector_top <- list(
  x = 1, xanchor = "right", y = 1, yanchor = "top",
  buttons = button_years_list
)

standard_margin <- list(t = 90, r = 30, b = 70, l = 60)

standard_title <- function(main, subtitle = "") {
  list(
    text = paste0(
      "<b>", main, "</b><br>",
      "<span style='font-size:0.8em; color:gray;'>", subtitle, "</span>"
    ),
    x = 0,
    xanchor = "left",
    xref = "paper",
    yanchor = "top",
    y = 0.95,
    font = list(size = 16)
  )
}

standard_legend <- function() {
  list(
    orientation = "h", 
    xanchor = "center",
    yanchor = "top",
    x = 0.5,
    y = -0.1,
    yref = "paper"
  )
}

standard_yaxis <- function(
  c = NULL,
  title = "",
  showgrid = TRUE,
  showline = TRUE,
  zeroline = TRUE,
  gridcolor = "lightgray",
  ticks = "outside",
  tickformat = ".2f",
  rangemode = "tozero",
  y = "primary",
  tickprefix = "",
  title_standoff = 12,        # NEW: spacing between title and tick labels
  auto_margin = TRUE          # NEW: let plotly grow margins if needed
) {
  # title as an object so we can set standoff
  title_obj <- list(text = title, standoff = title_standoff)

  x <- list(
    title = title_obj,
    showline = showline,
    zeroline = zeroline,
    gridcolor = gridcolor,
    ticks = ticks,
    tickformat = tickformat,
    tickprefix = tickprefix,
    automargin = auto_margin,     # NEW
    ticklabelposition = "outside" # keeps labels outside plotting area
  )
  if (!is.null(c)) {
    x <- c(
      x,
      list(tickfont = list(color = cc[[c]]), titlefont = list(color = cc[[c]]))
    )
  }
  if (y == "secondary") {
    x <- c(
      x,
      list(
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        rangemode = rangemode,
        anchor = "x"              # NEW: be explicit
      )
    )
  } else {
    x <- c(
      x,
      list(
        showgrid = showgrid,
        rangemode = rangemode
      )
    )
  }
  x
}

standard_date_xaxis <- function(title = list(text = NULL), y = 10) {
  list(
    title = title,
    type = "date",
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    gridcolor = "lightgray",
    tickmode = "auto",
    ticks = "outside",
    range = c(Sys.Date() - lubridate::years(y), Sys.Date()),
    rangeselector = rangeselector_top
  )
}

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

  cx <- c('teal_base', 'ruby_base')
  axis_color_1 <- NULL
  axis_color_2 <- NULL
  prefix1 <- ""
  prefix2 <- ""

  f <- series[series$Dim == unique_dims$Dim[1], ][1, ]
  axis_color_1 <- f$Primary
  if (axis_color_1 == "number") {axis_color_1 <- cx[1]}
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

    s <- series[series$Series.Id == key, ]
    axis_side <- if (is.null(unique_dims$Dim[1]) || unique_dims$Dim[1] == s$Dim[1]) "y" else "y2"
    if (s$Primary == "number"){
      col_use <- cc[[cx[i]]]
    } else {
      col_use <- if (axis_side == "y") cc[[s$Primary]] else cc[[s$Secondary]]
    }

    if (!is.null(s)) {
      p <- p |> add_trace(
        y    = data[[s$Series.Id]],
        name = s$Names,
        type = "scatter",
        mode = "lines",
        line = list(color = col_use, dash = if(s$Style == "Dashed") "dot" else "solid"),
        stackgroup = if (s$Style == "Fill") "one" else NULL,
        fillcolor = if(s$Style == "Fill") col_use else NULL,
        yaxis = axis_side
      )
    }
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
