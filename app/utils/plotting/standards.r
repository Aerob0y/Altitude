
### Colour definitions and plotly custom elements ----------------------------
cc <- c(
  black      = "#000000",
  teal_base  = "#53868B",
  teal_dark  = "#254346",
  teal_lite  = "#709BA1",
  teal_snow  = "#b4cccf",
  navy_base  = "#0d173f",
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
  grey_dark  = "#333333",
  grey_base  = "#666666",
  grey_lite  = "#999999",
  grey_snow  = "#CCCCCC"
)
palettes <- list(
  teal = unlist(cc[c("teal_dark", "teal_base", "teal_lite", "teal_snow")]),
  navy = unlist(cc[c("navy_dark", "navy_base", "navy_lite", "navy_snow")]),
  ruby = unlist(cc[c("ruby_dark", "ruby_base", "ruby_lite", "ruby_snow")]),
  gold = unlist(cc[c("gold_dark", "gold_base", "gold_lite", "gold_snow")]),
  grey = unlist(cc[c("grey_dark", "grey_base", "grey_lite")]),
  qual = unlist(cc[c("teal_base", "navy_base", "ruby_base", "gold_base", "grey_base")])
)

### Sizing Definitions ----------------------------
standard_margin <- list(
  t = 90,
  r = 30,
  b = 70,
  l = 60
)
button_years_list <- list(
  list(count = 20, label = "20 yr", step = "year", stepmode = "backward"),
  list(count = 15, label = "15 yr", step = "year", stepmode = "backward"),
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

### Standards ----------------------------
standard_title <- function(main, subtitle = "", overrides = list()) {
  modifyList(
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
    ),
    overrides
  )
}

standard_legend <- function(overrides = list()) {
  modifyList(
    list(
      orientation = "h",
      xanchor = "center",
      yanchor = "top",
      x = 0.5,
      y = -0.1,
      yref = "paper"
    ),
    overrides
  )
}

standard_yaxis <- function(formatting = list()) {
  f <- modifyList(
    list(
      title = "",
      c = NULL,
      y = "primary",
      standoff = 12,        # NEW: spacing between title and tick labels
      title = list(text = title, standoff = 12), # NEW: spacing between title and tick labels
      showline = TRUE,
      zeroline = TRUE,
      gridcolor = "lightgray",
      ticks = "outside",
      tickformat = ".2f",
      tickprefix = "",
      automargin = TRUE,     # NEW
      ticklabelposition = "outside" # keeps labels outside plotting area
    ),
    formatting
  )
  if (!is.null(f$c)) {
    #f$color <- cc[f$c]
    f$color <- f$c
    #f$tickfont <- list(color = cc[f$c])
    #f$titlefont <- list(color = cc[f$c])
    #f$tickfont <- list(color = "green")
  }

  if (f$y == "secondary" || f$y == 2) {
    f$overlaying <- "y"
    f$side <- "right"
    f$showgrid <- FALSE
    f$rangemode <- "tozero"
    f$anchor <- "x"              # NEW: be explicit
  } else {
    f$showgrid <- TRUE
    f$rangemode <- "tozero"
  }
  f
}

standard_date_xaxis <- function(title = list(text = NULL), y = 10, clean_ui = FALSE, overrides = list()) {
  rs <- rangeselector_top
  if (clean_ui) rs$visible <- FALSE

  modifyList(
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
      rangeselector = rs
    ), overrides
  )
}

### Misc Plotly Elenents ----------------------------


short_title <- function(elements = c()) {
  element_length <- length(elements)
  if (element_length == 0) {return("")}
  if (element_length == 1) {return(elements[1])}
  if (element_length == 2) {return(paste(elements, collapse = " and "))}
  if (element_length > 2) {
    return(paste0(
      paste(elements[1:(element_length - 1)], collapse = ", "),
      ", and ",
      elements[element_length]
    ))
  }
}
