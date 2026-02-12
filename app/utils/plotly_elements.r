`%||%` <- function(x, y) if (is.null(x)) y else x

mod_download_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "dl-toolbar",
    tags$div(
      class = "dl-format",
      selectInput(ns("format"), NULL, c("png","jpeg","svg"),
                  selected = "png", width = "70px", selectize = FALSE)
    ),
    tags$div(
      class = "dl-width",
      numericInput(ns("width"), NULL, 1920, min = 400, step = 1, width = "80px")
    ),
    tags$div(
      class = "dl-height",
      numericInput(ns("height"), NULL, 1080, min = 400, step = 1, width = "80px")
    ),
    tags$div(
      class = "dl-clean",
      checkboxInput(ns("clean"), "Clean", FALSE)
    )
  )
}



mod_download_server <- function(id,
                                defaults = list(format="png", width=1920, height=1080, scale=2, clean=FALSE)) {
  moduleServer(id, function(input, output, session) {

    # set defaults once
    observeEvent(TRUE, {
      updateSelectInput(session, "format", selected = defaults$format)
      updateNumericInput(session, "width",  value = defaults$width)
      updateNumericInput(session, "height", value = defaults$height)
      updateCheckboxInput(session, "clean", value = defaults$clean)
    }, once = TRUE)

    download <- reactive(list(
      format = input$format %||% defaults$format,
      width  = input$width  %||% defaults$width,
      height = input$height %||% defaults$height,
      scale  = defaults$scale
    ))

    clean_export <- reactive(isTRUE(input$clean))

    list(download = download, clean_export = clean_export)
  })
}


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
  grey = unlist(cc[c("grey_dark", "grey_base", "grey_lite")])
)
pal_qual_main <- c(
  cc["teal_base"],
  cc["navy_base"],
  cc["ruby_base"],
  cc["gold_base"],
  cc["grey_base"]
)
pal_navy_seq <- palettes$navy  # dark → snow

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









download_buttonx <- function(p) {
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


test <- function(p) {
  p <- p |> config(
    modeBarButtonsToRemove = list("toImage"),
    modeBarButtonsToAdd = list(
      list(
        name  = "Download (clean)",
        title = "Download without range buttons",
        icon  = htmlwidgets::JS("Plotly.Icons.camera"),
        click = htmlwidgets::JS(
          "function(gd){
             var fmt = (prompt('File type: png / jpeg / svg', 'png') || 'png')
                .toLowerCase().trim();
             console.log('Hello, JavaScript!');
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


download_button <- function(p) {
  p |> plotly::config(
    #modeBarButtonsToRemove = list("toImage"),
    modeBarButtonsToAdd = list(
      list(
        name  = "Download…",
        title = "Choose file type & resolution",
        icon  = htmlwidgets::JS("Plotly.Icons.camera"),
        click = htmlwidgets::JS(
"function(gd){
  var fmt = (prompt('File type: png / jpeg / svg', 'png') || 'png')
              .toLowerCase().trim();
  if (fmt === 'jpg') fmt = 'jpeg';
  if (['png','jpeg','svg'].indexOf(fmt) === -1) fmt = 'png';

  var w = parseInt(prompt('Width (px)', '2000') || '2000', 10);
  var h = parseInt(prompt('Height (px)', '1200') || '1200', 10);
  var s = parseFloat(prompt('Scale (e.g., 1, 2, 3)', '2') || '2');

  if (!isFinite(w) || w < 100) w = 2000;
  if (!isFinite(h) || h < 100) h = 1200;
  if (!isFinite(s) || s <= 0) s = 2;

  var opts = {format: fmt, width: w, height: h, scale: s};

  var orig = {
    rs: gd.layout.xaxis && gd.layout.xaxis.rangeselector ? gd.layout.xaxis.rangeselector.visible : true,
    rsl: gd.layout.xaxis && gd.layout.xaxis.rangeslider ? gd.layout.xaxis.rangeslider.visible : false
  };

  Plotly.relayout(gd, {
    'xaxis.rangeselector.visible': false,
    'xaxis.rangeslider.visible': false
  })
  .then(function(){ return Plotly.downloadImage(gd, opts); })
  .then(function(){
    return Plotly.relayout(gd, {
      'xaxis.rangeselector.visible': orig.rs,
      'xaxis.rangeslider.visible': orig.rsl
    });
  });
}"
        )
      )
    )
  )
}



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

assign_series_colours <- function(series) {
  if (!"Palette"   %in% names(series)) series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"

  s <- series %>%
    group_by(Palette, ColourKey) %>%  # or just by Palette if you prefer
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

  split_colors <- NULL
  if (!is.null(split)) {
    split_colors <- assign_series_colours(as_tibble(data[[split]])) %>% pull(colour)
    print(split_colors)
  }

  if (s$Style == "Bar") {
    p <- p |> plotly::add_bars(
      y      = data[[s$ID]],
      name   = s$Name,
      marker = list(color = s$colour),
      yaxis  = axis_side
    )
  } else {
    p <- p |>
      plotly::add_trace(
        y    = data[[s$ID]],
        name = if (is.null(split)) s$Name else data[[split]],
        type = "scatter",
        mode = "lines",
        line = list(
          color = if (is.null(split)) s$colour else split_colors,
          split = if (is.null(split)) NULL else data[[split]],
          dash  = if (s$Style == "Dashed") "dot" else "solid",
          shape = "spline"
        ),
        stackgroup = if (s$Style == "Fill") "one" else NULL,
        fillcolor  = if (s$Style == "Fill") s$colour else NULL,
        yaxis      = axis_side
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
  series <- assign_series_colours(series)
  #print(series[, c("Name", "Style", "colour")])



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





generic_plotly <- function(
  data,
  titles       = c("", ""),
  yaxis_titles = c("", ""),
  series,
  split = NULL,
  k,
  years = 10
) {
  # 1) Colour assignment ----------------------------------------------------
  series <- assign_series_colours(series)

  # 2) Base plot (one-liner split handling) --------------------------------
  p <- plotly::plot_ly(
    data,
    x     = ~ get(k),
    split = if (!is.null(split)) ~ get(split) else NULL
  )

  # 3) Axis metadata --------------------------------------------------------
  unique_dims <- unique(series[c("Dim", "Tick", "Prefix")])

  # helper to grab axis colour from first series on that dimension
  get_axis_colour_for_dim <- function(dim_value) {
    rows <- which(series$Dim == dim_value)
    if (!length(rows)) return(NULL)
    series$colour[rows[1]]
  }

  axis_color_1 <- if (nrow(unique_dims) >= 1) {
    get_axis_colour_for_dim(unique_dims$Dim[1])
  } else {
    "black"
  }

  axis_color_2 <- if (nrow(unique_dims) >= 2) {
    get_axis_colour_for_dim(unique_dims$Dim[2])
  } else {
    NULL
  }

  # 4) Add series traces ----------------------------------------------------
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]

    # Decide which y-axis
    axis_side <- if (unique_dims$Dim[1] == s$Dim[1]) "y" else "y2"

    col_use <- s$colour
    is_bar  <- s$Style == "Bar"
    is_fill <- s$Style == "Fill"
    is_dash <- s$Style == "Dashed"

    if (is_bar) {
      p <- p |>
        plotly::add_bars(
          y      = data[[s$ID]],
          name   = s$Name,
          marker = list(color = col_use),
          yaxis  = axis_side
        )
    } else {
      p <- p |>
        plotly::add_trace(
          y    = data[[s$ID]],
          name = s$Name,
          type = "scatter",
          mode = "lines",
          line = list(
            color = col_use,
            dash  = if (is_dash) "dot" else "solid",
            shape = "spline"
          ),
          stackgroup = if (is_fill) "one" else NULL,
          fillcolor  = if (is_fill) col_use else NULL,
          yaxis      = axis_side
        )
    }
  }

  # 5) Layout ---------------------------------------------------------------
  p |>
    plotly::layout(
      title     = standard_title(titles[1], titles[2]),
      legend    = standard_legend(),
      hovermode = "x unified",
      barmode   = "stack",
      margin    = standard_margin,
      xaxis     = standard_date_xaxis(y = years),

      yaxis  = standard_yaxis_new(
        list(
          title      = yaxis_titles[1],
          tickprefix = unique_dims$Prefix[1],
          c          = axis_color_1,
          tickformat = unique_dims$Tick[1]
        )
      ),
      


      yaxis2 = standard_yaxis_new(
        list(
          title      = yaxis_titles[2],
          tickprefix = unique_dims$Prefix[2],
          tickformat = unique_dims$Tick[2],
          y          = "secondary",
          c          = axis_color_2
        )
      )
      #)
    )
}
plot_ts_by_region <- function(
  data,
  date_col   = "Date",
  region_col = "Region",
  value_col  = "Guest arrivals",
  titles      = c("", ""),
  subtitle   = "",
  years      = 10,
  palette    = pal_qual_main,  # reuse your main qualitative palette
  series
) {
  # Ensure the columns exist
  stopifnot(all(c(date_col, region_col, value_col) %in% names(data)))
  
  # Set up factors and colours ----------------------------------------------
  reg <- sort(unique(data[[region_col]]))
  n_reg <- length(reg)
  # If more regions than palette colours, interpolate
  if (n_reg <= length(palette)) {
    cols <- palette[seq_len(n_reg)]
  } else {
    cols <- grDevices::colorRampPalette(palette)(n_reg)
  }
  name(cols) <- reg
  # Build plot --------------------------------------------------------------
  p <- plotly::plot_ly(
    data  = data,
    x     = ~ .data[[date_col]],
    color = ~ .data[[region_col]],
    colors = cols
  ) |>
    plotly::add_lines(
      y           = ~ .data[[value_col]],
      text        = ~ .data[[region_col]],
      hovertemplate = paste0(
        "%{x}<br>",
        value_col, ": %{y:,.0f}<br>",
        region_col, ": %{text}<extra></extra>"
      ),
      connectgaps = TRUE  # <-- important
    )

  p |>
    plotly::layout(
      title     = standard_title(titles[1], titles[2]),
      legend    = standard_legend(),
      hovermode = "x unified",
      margin    = standard_margin,
      xaxis     = standard_date_xaxis(y = years),
      yaxis     = standard_yaxis(
        title = value_col,
        c     = cc[["grey_base"]]
      )
    )
}
generic_plotly2 <- function(
  data,
  titles       = c("", ""),
  yaxis_titles = c("", ""),
  series,
  split = NULL,
  k,
  years = 10,
  shape = "wide"
) {
      # Set up factors and colours ----------------------------------------------
  if (!is.null(split)) {
  reg <- sort(unique(data[[split]]))
  n_reg <- length(reg)
  # If more regions than palette colours, interpolate
  if (n_reg <= length(pal_qual_main)) {
    cols <- pal_qual_main[seq_len(n_reg)]
  } else {
    cols <- grDevices::colorRampPalette(pal_qual_main)(n_reg)
  }
  names(cols) <- reg
}
  # 1) Colour assignment ----------------------------------------------------

  if(shape == "wide") {series <- assign_series_colours(series)}

  # 2) Base plot (one-liner split handling) --------------------------------
  p <- plotly::plot_ly(
    data,
    x     = ~ get(k),
    color = if (!is.null(split)) ~ .data[[split]] else NULL,
    colors = if (!is.null(split)) cols else NULL
  )

  # 3) Axis metadata --------------------------------------------------------
  unique_dims <- unique(series[c("Dim", "Tick", "Prefix")])



  # helper to grab axis colour from first series on that dimension
  get_axis_colour_for_dim <- function(dim_value) {
    rows <- which(series$Dim == dim_value)
    if (!length(rows)) return(NULL)
    series$colour[rows[1]]
  }

  axis_color_1 <- if (nrow(unique_dims) >= 1 && shape == "wide") {
    get_axis_colour_for_dim(unique_dims$Dim[1])
  } else {
    "black"
  }

  axis_color_2 <- if (nrow(unique_dims) >= 2 && shape == "wide") {
    get_axis_colour_for_dim(unique_dims$Dim[2])
  } else {
    "black"
  }
  # 4) Add series traces ----------------------------------------------------
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]

    # Decide which y-axis
    axis_side <- if (unique_dims$Dim[1] == s$Dim[1]) "y" else "y2"

    col_use <- s$colour
    is_bar  <- s$Style == "Bar"
    is_fill <- s$Style == "Fill"
    is_dash <- s$Style == "Dashed"
    if (is_bar) {
      p <- p |>
        plotly::add_bars(
          y      = data[[s$ID]],
          name   = if(shape == "wide") s$Name,
          #marker = if (shape == "wide") list(color = col_use),
          yaxis  = axis_side
        )
    } else {
      p <- p |>
        plotly::add_trace(
          y    = data[[s$ID]],
          name = if(shape == "wide") s$Name,
          type = "scatter",
          mode = "lines",
          line = list(
            color = if (shape == "wide") col_use,
            dash  = if (is_dash) "dot" else "solid",
            shape = "spline"
          ),
          stackgroup = if (is_fill) "one" else NULL,
          #fillcolor  = if (is_fill) (if (shape == "wide") col_use) else NULL,
          yaxis      = axis_side
        )
    }
  }

  # 5) Layout ---------------------------------------------------------------
  p |>
    plotly::layout(
      title     = standard_title(titles[1], titles[2]),
      legend    = standard_legend(),
      hovermode = "x unified",
      barmode   = "stack",
      margin    = standard_margin,
      xaxis     = standard_date_xaxis(y = years),
      yaxis  = standard_yaxis(
        list(
          title      = yaxis_titles[1],
          c          = axis_color_1,
          tickprefix = unique_dims$Prefix[1],
          tickformat = unique_dims$Tick[1]
        )
      ),
      yaxis2 = standard_yaxis(
        list(
          title      = yaxis_titles[2],
          c          = axis_color_2,
          tickprefix = unique_dims$Prefix[2],
          tickformat = unique_dims$Tick[2],
          y          = "secondary"
        )
      )
    )
}
plot_long <- function(
  data,
  k   = "Date",
  split      = "",
  series,
  titles      = c("", ""),
  years      = 10,
  palette    = pal_qual_main  # reuse your main qualitative palette
) {
  # Ensure the columns exist
  

  metric    <- series$Name[1]
  stopifnot(all(c(k) %in% names(data)))
  stopifnot(all(c(split) %in% names(data)))
  stopifnot(all(c(metric) %in% names(data)))

  stopifnot(all(c(k, split, metric) %in% names(data)))
  # Set up factors and colours ----------------------------------------------
  reg <- sort(unique(data[[split]]))
  n_reg <- length(reg)
  # If more regions than palette colours, interpolate
  if (n_reg <= length(palette)) {
    cols <- palette[seq_len(n_reg)]
  } else {
    cols <- grDevices::colorRampPalette(palette)(n_reg)
  }
  names(cols) <- reg
  # Build plot --------------------------------------------------------------
  p <- plotly::plot_ly(
    data  = data,
    x     = ~ .data[[k]],
    color = ~ .data[[split]],
    colors = cols
  ) |>
    plotly::add_lines(
      y           = ~ .data[[metric]],
      text        = ~ .data[[split]],
      hovertemplate = paste0(
        "%{x}<br>",
        metric, ": %{y:,.0f}<br>",
        split, ": %{text}<extra></extra>"
      ),
      connectgaps = TRUE  # <-- important
    )

  p |>
    plotly::layout(
      title     = standard_title(titles[1], titles[2]),
      legend    = standard_legend(),
      hovermode = "x unified",
      margin    = standard_margin,
      xaxis     = standard_date_xaxis(y = years),
      yaxis     = standard_yaxis(
        title = metric,
        c     = cc[["grey_base"]]
      )
    )
}