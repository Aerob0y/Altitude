#### Reference Plot

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

#### plotly2
library(datapasta)
### to_rgba: Returns a colour in rgba format, with the specified alpha value ----
to_rgba <- function(col, alpha = 0.25) {
  rgb <- grDevices::col2rgb(col)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

### assign_series_colours: Assigns colours to series based on the specified palette and colour key ----
assign_series_colours <- function(series) {

  if (!"Palette" %in% names(series))   series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"

  series %>%
    group_by(Palette, ColourKey) %>%
    mutate(
      colour = if (first(Palette) == "manual") {
        unname(cc[ColourKey])
      } else {
        pal <- palettes[[first(Palette)]]
        pal[(row_number() - 1) %% length(pal) + 1]
      }
    ) %>%
    ungroup()
}

### defaults to use ----
spec_defaults <- list(
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

# Current Use
#x_plotly(
#  data = hm14_data(),
#  titles = c("Perceptions and Expectations", ""),
#  series = filter_series(guide_rbnz, apply_filters = list(Data = c("hm14"), Name = x)),
#  k = "Date"
#)

### new use ----

load_data("hm14") %>% head()

#not using
hm14_data <- pivot_reference(
  load_data("hm14"),
  "app/config/structure.json",
  "hm14"
)
hm14_data <- load_data("hm14")

bond_data <- load_data("bond")
bond_data %>%head()

structure_table <- imap_dfr(
  hm14_data,
  ~ .x |>
      distinct(Period) |>
      mutate(List = .y, .before = 1)
)
structure_table
structure_table %>% print(n = 50)


datapasta::tribble_paste(structure_table)

x <- tibble::tribble(
                                      ~List,                  ~Period,      ~Dim,  ~Tick,  ~Prefix, ~Stack, 
  "Perception of monetary conditions (net)", "End of current quarter", "Total %",  ".2f",       "", "Never",
  "Perception of monetary conditions (net)",           "Next quarter", "Total %",  ".2f",       "", "Never",
  "Perception of monetary conditions (net)",             "1 year out", "Total %", ".2f",        "", "Never",
                        "Annual CPI growth",             "1 year out", "Total %", ".2f",        "", "Never",
                        "Annual CPI growth",            "2 years out", "Total %", ".2f",        "", "Never",
                        "Annual CPI growth",            "5 years out", "Total %", ".2f",        "", "Never",
                        "Annual CPI growth",           "10 years out", "Total %", ".2f",        "", "Never",
                 "Official Cash Rate (OCR)",             "1 year out", "Total %", ".2f",        "", "Never",
                 "Official Cash Rate (OCR)",            "2 years out", "Total %", ".2f",        "", "Never",
                 "Official Cash Rate (OCR)",           "10 years out", "Total %", ".2f",        "", "Never",
                 "Official Cash Rate (OCR)",   "10 years out average", "Total %", ".2f",        "", "Never",
           "10 year government bond yields", "End of current quarter", "Total %", ".2f",        "", "Never",
           "10 year government bond yields",             "1 year out", "Total %", ".2f",        "", "Never",
                        "Annual GDP growth",             "1 year out", "Total %", ".2f",        "", "Never",
                        "Annual GDP growth",            "2 years out", "Total %", ".2f",        "", "Never",
                "Annual hourly wage growth",             "1 year out", "Total %", ".2f",        "", "Never",
                "Annual hourly wage growth",            "2 years out", "Total %", ".2f",        "", "Never",
                        "Unemployment rate",             "1 year out", "Total %", ".2f",        "", "Never",
                        "Unemployment rate",            "2 years out", "Total %", ".2f",        "", "Never",
                  "US dollar exchange rate",           "Next quarter", "Total %", ".2f",        "", "Never",
                  "US dollar exchange rate",             "1 year out", "Total %", ".2f",        "", "Never",   
          "Australian dollar exchange rate",           "Next quarter", "Total %", ".2f",        "", "Never",
          "Australian dollar exchange rate",             "1 year out", "Total %", ".2f",        "", "Never",
                        "House price index",             "1 year out", "Total %", ".2f",        "", "Never",
                        "House price index",            "2 years out", "Total %", ".2f",        "", "Never",
  )



reference
reference$traces <- reference$traces[test_trace_ids2 %in% test_series$ID]

#### reference_plot: Creates a plotly plot based on the provided data and reference specification ----
#reference_plot2 <- function(data, reference, chart = NULL) {


# 1. Get the specs, apply defaults, check trace styles -----------------------------
# Read both '{"title":"Investment","years":10}' vs list(title = "Investment", years = 10)
# if there are multiple charts, select the one specified by `chart`
# use the defaults unless overridden by the reference specification
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
spec <- utils::modifyList(spec_defaults, spec)

if (any(!(vapply(spec$traces, `[[`, character(1), "style") %in% c("line", "dashed", "bar", "area")))) {
  stop("Trace styles must be line, dashed, bar, or area.", call. = FALSE)
}

dims <- vapply(spec$traces, `[[`, character(1), "Dim")
dim_id <- match(dims, unique(dims))

spec$traces <- Map(
  function(trace, id) {
    trace$DimID <- id
    trace
  },
  spec$traces,
  dim_id
)
spec


cat(jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE))

# 1. Get the specs, apply defaults, check trace styles -----------------------------

# 1. Validate the reference specification
or_default <- function(value, default) if (is.null(value)) default else value




#}


load_data("hs32") %>% head()



###  Working setup ----

temp_data <- load_data("hm3")
temp_choices <- filter_series(
    guide_rbnz,
    column = "Class_1",
    apply_filters = list(Data = "hm3")
  )$Class_1
temp_choices
# "Residential consents" 
test_series <- filter_series(
  guide_rbnz,
  apply_filters = list(Data = "hm3", Class_1 = temp_choices[1])
)
test_series
test_reference <- jsonlite::fromJSON(
  "app/config/hb3_test.json",
  simplifyVector = FALSE
)
test_reference
test_trace_ids <- vapply(test_reference$traces, `[[`, character(1), "y")
test_trace_ids
test_reference$traces <- test_reference$traces[test_trace_ids %in% test_series$ID]
test_reference$traces
test_reference$subtitle <- paste("RBNZ:", short_title(temp_choices[1]))

test_reference2 <- jsonlite::fromJSON(
  "app/config/hb3_test2.json",
  simplifyVector = FALSE
)
test_reference2
test_trace_ids2 <- vapply(test_reference2$traces, `[[`, character(1), "ID")
test_trace_ids2
test_reference2$traces <- test_reference2$traces[test_trace_ids2 %in% test_series$ID]
test_reference2$traces
test_reference2$subtitle <- paste("RBNZ:", short_title(temp_choices[1]))
test_reference2


reference_plot(temp_data, test_reference)
reference_plot2(temp_data, test_reference2)