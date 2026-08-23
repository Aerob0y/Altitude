# A self-contained, configuration-driven alternative to x_plotly().
#
# The public entry point is reference_plot().  It deliberately has a small
# interface: supply data, a JSON reference (or an equivalent R list), and an
# optional chart name when the reference contains more than one chart.

reference_plot <- function(data, reference, chart = NULL) {
  or_default <- function(value, default) if (is.null(value)) default else value

  # 1. Read and select the chart specification -----------------------------
  spec <- if (is.character(reference) && length(reference) == 1L) {
    jsonlite::fromJSON(reference, simplifyVector = FALSE)
  } else {
    reference
  }

  if (!is.null(spec$charts)) {
    if (is.null(chart) || !chart %in% names(spec$charts)) {
      stop(
        "Choose `chart` from: ", paste(names(spec$charts), collapse = ", "),
        call. = FALSE
      )
    }
    spec <- spec$charts[[chart]]
  }
  if (!is.list(spec)) stop("`reference` must resolve to a chart list.", call. = FALSE)

  defaults <- list(
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
  spec <- utils::modifyList(defaults, spec)
  spec$format <- tolower(spec$format)

  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!spec$format %in% c("wide", "long")) {
    stop("`format` must be either 'wide' or 'long'.", call. = FALSE)
  }
  required <- spec$x
  if (spec$format == "long") required <- c(required, spec$value, spec$series)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Missing data columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  # 2. Turn either data shape into one explicit trace table ----------------
  palette <- c("#53868B", "#0D173F", "#8B2323", "#F9C31F", "#666666")
  traces <- list()

  if (spec$format == "wide") {
    if (is.null(spec$traces) || !length(spec$traces)) {
      stop("Wide data requires a non-empty `traces` list.", call. = FALSE)
    }
    for (i in seq_along(spec$traces)) {
      trace <- spec$traces[[i]]
      if (is.null(trace$y) || !trace$y %in% names(data)) {
        stop("Trace ", i, " has an unknown `y` column.", call. = FALSE)
      }
      traces[[i]] <- list(
        data = data,
        y = trace$y,
        name = or_default(trace$name, trace$y),
        style = tolower(or_default(trace$style, spec$style)),
        axis = or_default(trace$axis, "y"),
        stack = or_default(trace$stack, spec$stack),
        colour = or_default(trace$colour, palette[(i - 1L) %% length(palette) + 1L])
      )
    }
  } else {
    series_values <- unique(data[[spec$series]])
    series_values <- series_values[!is.na(series_values)]
    if (!is.null(spec$include)) {
      series_values <- intersect(unlist(spec$include, use.names = FALSE), series_values)
    }

    for (i in seq_along(series_values)) {
      value <- series_values[[i]]
      trace_data <- data[data[[spec$series]] == value & !is.na(data[[spec$series]]), , drop = FALSE]
      traces[[i]] <- list(
        data = trace_data,
        y = spec$value,
        name = as.character(value),
        style = spec$style,
        axis = "y",
        stack = spec$stack,
        colour = palette[(i - 1L) %% length(palette) + 1L]
      )
    }
  }

  valid_styles <- c("line", "dashed", "bar", "area")
  styles <- vapply(traces, `[[`, character(1), "style")
  if (any(!styles %in% valid_styles)) {
    stop("Trace styles must be line, dashed, bar, or area.", call. = FALSE)
  }

  # 3. Add traces in the order written in the reference --------------------
  p <- plotly::plot_ly()
  for (trace in traces) {
    common <- list(
      p = p,
      x = trace$data[[spec$x]],
      y = trace$data[[trace$y]],
      name = trace$name,
      yaxis = trace$axis,
      marker = list(color = trace$colour),
      line = list(
        color = trace$colour,
        dash = if (trace$style == "dashed") "dot" else "solid"
      )
    )

    if (trace$style == "bar") {
      p <- do.call(plotly::add_bars, common)
    } else {
      common$type <- "scatter"
      common$mode <- if (trace$style == "area") "none" else "lines"
      if (isTRUE(trace$stack)) common$stackgroup <- paste0(trace$axis, "_stack")
      if (trace$style == "area" && !isTRUE(trace$stack)) common$fill <- "tozeroy"
      p <- do.call(plotly::add_trace, common)
    }
  }

  # 4. Apply one consistent layout and download configuration --------------
  title <- paste0(
    "<b>", spec$title, "</b>",
    if (nzchar(spec$subtitle)) paste0("<br><span style='font-size:0.8em;color:gray;'>", spec$subtitle, "</span>") else ""
  )
  axis <- function(title, overlay = FALSE) {
    out <- list(
      title = list(text = title, standoff = 12),
      tickformat = spec$tickformat,
      tickprefix = spec$prefix,
      showline = TRUE,
      zeroline = TRUE,
      ticks = "outside",
      gridcolor = "lightgray",
      rangemode = "tozero",
      automargin = TRUE
    )
    if (overlay) utils::modifyList(out, list(overlaying = "y", side = "right", showgrid = FALSE)) else out
  }

  p <- plotly::layout(
    p,
    title = list(text = title, x = 0, xanchor = "left"),
    hovermode = "x unified",
    barmode = if (isTRUE(spec$stack)) "stack" else "group",
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
    margin = list(t = 90, r = 60, b = 70, l = 60),
    xaxis = list(
      type = "date",
      showgrid = FALSE,
      range = c(Sys.Date() - lubridate::years(spec$years), Sys.Date()),
      rangeselector = list(buttons = list(
        list(count = 20, label = "20 yr", step = "year", stepmode = "backward"),
        list(count = 10, label = "10 yr", step = "year", stepmode = "backward"),
        list(count = 5, label = "5 yr", step = "year", stepmode = "backward"),
        list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
        list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
        list(step = "all")
      ))
    ),
    yaxis = axis(spec$y_title),
    yaxis2 = axis(spec$y2_title, overlay = TRUE)
  )

  plotly::config(p, displayModeBar = TRUE, toImageButtonOptions = spec$download)
}
