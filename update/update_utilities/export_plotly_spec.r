library(plotly)
library(jsonlite)


source("app/utils/utils_data.r") # data sources, caching, filtering
source("app/utils/plotly_elements.r") # plotly customisations
source("app/ui/ui_elements.r") # common UI elements

# Convert htmlwidgets::JS objects into plain strings (or drop them)
sanitize_for_json <- function(x) {
  if (inherits(x, "JS_EVAL") || inherits(x, "htmlwidget")) return(NULL)

  if (is.list(x)) {
    out <- lapply(x, sanitize_for_json)
    # keep names
    if (!is.null(names(x))) names(out) <- names(x)
    # drop NULL entries
    out <- out[!vapply(out, is.null, logical(1))]
    return(out)
  }

  # convert weird scalars safely
  if (is.factor(x)) return(as.character(x))
  x
}

plotly_to_spec_json <- function(p, file) {
  pb <- plotly_build(p)

  spec <- list(
    data   = pb$x$data,
    layout = pb$x$layout,
    config = pb$x$config
  )

  spec <- sanitize_for_json(spec)

  writeLines(
    jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null", digits = NA),
    con = file
  )
}

# Example usage:
# plotly_to_spec_json(p, "site/hb1.json")
