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