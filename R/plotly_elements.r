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
  p <- p %>% config(
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