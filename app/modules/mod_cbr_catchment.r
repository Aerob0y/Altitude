# =============================================================================
# Canberra Airport catchment map module
# =============================================================================
#
# The app reads one prepared sf RDS at startup. Moving the slider only changes
# which polygons are highlighted; it does not read drive_times.csv or call a
# routing API.
# =============================================================================

cbr_catchment_cache_path <- getOption(
  "altitude.cbr_catchment_cache",
  file.path("app", "data", "CBR", "cbr_catchment_map.rds")
)

.cbr_cache_columns <- c(
  "sa2_code",
  "sa2_name",
  "state_name",
  "population_2025",
  "area_sq_km",
  "cbr_drive_minutes",
  "syd_drive_minutes",
  "mel_drive_minutes",
  "competitor_drive_minutes",
  "competitor_airport",
  "cbr_time_advantage",
  "closest_airport"
)

.cbr_airports <- tibble::tribble(
  ~name, ~longitude, ~latitude,
  "CBR", 149.1950, -35.3069,
  "SYD", 151.1772, -33.9399,
  "MEL", 144.8430, -37.6733
)

.cbr_towns <- tibble::tribble(
  ~name, ~longitude, ~latitude,
  "Yass", 148.91, -34.84,
  "Goulburn", 149.72, -34.75,
  "Cooma", 149.13, -36.24,
  "Batemans Bay", 150.18, -35.71,
  "Bowral", 150.42, -34.48,
  "Young", 148.30, -34.31,
  "Tumut", 148.22, -35.30,
  "Wagga Wagga", 147.37, -35.12,
  "Bega", 149.84, -36.68,
  "Albury", 146.92, -36.08
)

.cbr_require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "The 'sf' package is required for the Canberra catchment map.",
      call. = FALSE
    )
  }
}

# Convert any POLYGON / MULTIPOLYGON / GEOMETRYCOLLECTION mixture into a
# homogeneous MULTIPOLYGON layer. This also repairs an existing cache that was
# written as sfc_GEOMETRY, which ggplot can display but ggplotly cannot fortify.
.cbr_polygonize <- function(data) {
  .cbr_require_sf()

  if (!inherits(data, "sf")) {
    stop("The Canberra catchment data must be an sf object.", call. = FALSE)
  }

  data <- sf::st_make_valid(data)
  geometry_types <- unique(
    as.character(sf::st_geometry_type(data, by_geometry = TRUE))
  )

  if (
    length(geometry_types) > 1L ||
      any(geometry_types %in% c("GEOMETRY", "GEOMETRYCOLLECTION"))
  ) {
    data <- suppressWarnings(
      sf::st_collection_extract(data, "POLYGON", warn = FALSE)
    )
  }

  data <- data[!sf::st_is_empty(data), , drop = FALSE]

  if (nrow(data) == 0L) {
    stop("The Canberra catchment cache contains no polygon geometry.", call. = FALSE)
  }

  # st_collection_extract() can create one row per polygon part. Recombine those
  # parts so population and area remain represented once per SA2.
  geometry_column <- attr(data, "sf_column")
  attribute_columns <- setdiff(names(data), geometry_column)

  data <- data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(attribute_columns))
    ) |>
    dplyr::summarise(
      do_union = FALSE,
      .groups = "drop"
    )

  suppressWarnings(
    sf::st_cast(data, "MULTIPOLYGON", warn = FALSE)
  )
}

.cbr_load_cache <- function(path = cbr_catchment_cache_path) {
  .cbr_require_sf()

  if (!file.exists(path)) {
    return(NULL)
  }

  data <- readRDS(path)

  if (!inherits(data, "sf")) {
    stop("The Canberra catchment cache is not an sf object: ", path, call. = FALSE)
  }

  missing_columns <- setdiff(.cbr_cache_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "The Canberra catchment cache is missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data |>
    .cbr_polygonize() |>
    sf::st_transform(4326)
}

# Run this only when the source geography, population or cached drive times
# change. It does not calculate drive times.
write_cbr_catchment_cache <- function(
  catchment_sa2,
  output_path = cbr_catchment_cache_path,
  simplify_metres = 250
) {
  .cbr_require_sf()

  if (!inherits(catchment_sa2, "sf")) {
    stop("catchment_sa2 must be an sf object.", call. = FALSE)
  }

  required_source_columns <- c(
    "SA2_CODE21",
    "SA2_NAME21",
    "STE_NAME21",
    "ERP 2025",
    "AREASQKM21",
    "CBR_drive_minutes",
    "SYD_drive_minutes",
    "MEL_drive_minutes"
  )

  missing_columns <- setdiff(required_source_columns, names(catchment_sa2))

  if (length(missing_columns) > 0L) {
    stop(
      "catchment_sa2 is missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  to_numeric <- function(x) {
    if (is.numeric(x)) {
      return(as.numeric(x))
    }

    readr::parse_number(as.character(x))
  }

  map_data <- catchment_sa2 |>
    dplyr::mutate(
      .cbr_minutes = to_numeric(.data[["CBR_drive_minutes"]]),
      .syd_minutes = to_numeric(.data[["SYD_drive_minutes"]]),
      .mel_minutes = to_numeric(.data[["MEL_drive_minutes"]]),
      .competitor_minutes = pmin(.syd_minutes, .mel_minutes, na.rm = TRUE),
      .competitor_minutes = dplyr::if_else(
        is.infinite(.competitor_minutes),
        NA_real_,
        .competitor_minutes
      ),
      .competitor_airport = dplyr::case_when(
        is.na(.syd_minutes) & is.na(.mel_minutes) ~ NA_character_,
        is.na(.mel_minutes) ~ "SYD",
        is.na(.syd_minutes) ~ "MEL",
        .syd_minutes <= .mel_minutes ~ "SYD",
        TRUE ~ "MEL"
      ),
      .cbr_advantage = .competitor_minutes - .cbr_minutes,
      .closest_airport = dplyr::case_when(
        is.na(.cbr_minutes) ~ .competitor_airport,
        is.na(.competitor_minutes) ~ "CBR",
        .cbr_minutes <= .competitor_minutes ~ "CBR",
        TRUE ~ .competitor_airport
      ),
      .population_2025 = to_numeric(.data[["ERP 2025"]]),
      .area_sq_km = to_numeric(.data[["AREASQKM21"]])
    ) |>
    dplyr::select(
      sa2_code = SA2_CODE21,
      sa2_name = SA2_NAME21,
      state_name = STE_NAME21,
      population_2025 = .population_2025,
      area_sq_km = .area_sq_km,
      cbr_drive_minutes = .cbr_minutes,
      syd_drive_minutes = .syd_minutes,
      mel_drive_minutes = .mel_minutes,
      competitor_drive_minutes = .competitor_minutes,
      competitor_airport = .competitor_airport,
      cbr_time_advantage = .cbr_advantage,
      closest_airport = .closest_airport
    ) |>
    sf::st_transform(3577) |>
    .cbr_polygonize()

  if (
    is.numeric(simplify_metres) &&
      length(simplify_metres) == 1L &&
      is.finite(simplify_metres) &&
      simplify_metres > 0
  ) {
    map_data <- sf::st_simplify(
      map_data,
      dTolerance = simplify_metres,
      preserveTopology = TRUE
    ) |>
      .cbr_polygonize()
  }

  map_data <- sf::st_transform(map_data, 4326)

  attr(map_data, "cache_created_at") <- format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  )
  attr(map_data, "advantage_definition") <-
    "min(SYD drive minutes, MEL drive minutes) - CBR drive minutes"

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(map_data, output_path, compress = "gzip")

  message(
    "Wrote Canberra catchment cache: ",
    normalizePath(output_path, winslash = "/", mustWork = FALSE)
  )

  invisible(map_data)
}

# Read once when the module file is sourced at app startup.
.cbr_catchment_cache <- local({
  result <- list(data = NULL, error = NULL)

  tryCatch(
    {
      result$data <- .cbr_load_cache(cbr_catchment_cache_path)
    },
    error = function(e) {
      result$error <- conditionMessage(e)
    }
  )

  result
})

.cbr_catchment_slider_max <- local({
  data <- .cbr_catchment_cache$data

  if (is.null(data)) {
    return(180)
  }

  maximum <- suppressWarnings(max(data$cbr_time_advantage, na.rm = TRUE))

  if (!is.finite(maximum)) {
    180
  } else {
    max(120, min(360, ceiling(maximum / 30) * 30))
  }
})

if (exists("module_notes", inherits = TRUE)) {
  module_notes$cbr_catchment <- list(
    "Canberra catchment",
    "Cached OpenRouteService drive times at SA2 level",
    paste(
      "Highlights areas where Canberra Airport is closer by road than the",
      "nearer of Sydney or Melbourne. Increase the threshold to require a",
      "larger Canberra drive-time advantage."
    )
  )
}

mod_cbr_catchment_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    class = "module-page",
    module_note("cbr_catchment"),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        class = "csv-sidebar",
        position = "left",
        shiny::sliderInput(
          inputId = ns("threshold"),
          label = "Minimum CBR drive-time advantage",
          min = 0,
          max = .cbr_catchment_slider_max,
          value = 0,
          step = 5,
          post = " minutes",
          width = "100%"
        ),
        shiny::checkboxInput(
          inputId = ns("show_towns"),
          label = "Show reference towns",
          value = TRUE
        ),
        shiny::helpText(
          paste(
            "At zero, the map shows every SA2 where CBR is at least as quick",
            "to reach as both SYD and MEL."
          )
        ),
        shiny::tags$hr(),
        tags$div(class = "dl-compact dl-row", download_settings_ui(ns)),
        shiny::uiOutput(ns("summary")),
        style = "height: 100%;"
      ),
      bslib::card(
        class = "module-chart-card",
        full_screen = TRUE,
        shiny::plotOutput(
          outputId = ns("plot"),
          height = "720px",
          width = "100%"
        ),
        width = "100%",
        height = "100%",
        fill = TRUE
      )
    )
  )
}

mod_cbr_catchment_server <- function(id, selected_tab, activate_on) {
  shiny::moduleServer(id, function(input, output, session) {
    enabled <- shiny::reactive(
      identical(selected_tab(), activate_on)
    )

    threshold <- shiny::reactive({
      shiny::req(input$threshold)
      as.numeric(input$threshold)
    })

    cache_error_message <- function() {
      if (!is.null(.cbr_catchment_cache$error)) {
        return(paste(
          "The Canberra catchment cache could not be loaded:",
          .cbr_catchment_cache$error
        ))
      }

      paste0(
        "Canberra catchment cache not found at '",
        cbr_catchment_cache_path,
        "'. Run write_cbr_catchment_cache() once, then restart the app."
      )
    }

    map_data <- shiny::reactive({
      shiny::req(enabled())
      shiny::validate(
        shiny::need(
          !is.null(.cbr_catchment_cache$data),
          cache_error_message()
        )
      )

      .cbr_catchment_cache$data |>
        dplyr::mutate(
          meets_threshold =
            !is.na(.data$cbr_time_advantage) &
            .data$cbr_time_advantage >= threshold(),
          highlighted_advantage = dplyr::if_else(
            .data$meets_threshold,
            .data$cbr_time_advantage,
            NA_real_
          )
        )
    })

    catchment_summary <- shiny::reactive({
      data <- map_data() |>
        dplyr::filter(.data$meets_threshold)

      list(
        sa2_count = nrow(data),
        population = sum(data$population_2025, na.rm = TRUE),
        area_sq_km = sum(data$area_sq_km, na.rm = TRUE)
      )
    })

    output$summary <- shiny::renderUI({
      shiny::req(enabled())

      if (is.null(.cbr_catchment_cache$data)) {
        return(
          shiny::tags$p(
            class = "text-danger",
            cache_error_message()
          )
        )
      }

      summary <- catchment_summary()

      shiny::tags$div(
        style = paste(
          "padding: 10px 12px;",
          "border-radius: 8px;",
          "background: rgba(255,255,255,0.55);",
          "font-size: 0.92rem;"
        ),
        shiny::tags$strong("Highlighted catchment"),
        shiny::tags$br(),
        scales::comma(summary$population, accuracy = 1),
        " people (2025)",
        shiny::tags$br(),
        scales::comma(summary$sa2_count, accuracy = 1),
        " SA2 areas",
        shiny::tags$br(),
        scales::comma(summary$area_sq_km, accuracy = 1),
        " km2"
      )
    })

    output$plot <- shiny::renderPlot({
      shiny::req(enabled())

      data <- map_data()
      selected_threshold <- threshold()

      subtitle <- if (selected_threshold == 0) {
        paste(
          "Gold areas are those where CBR is at least as quick to reach as",
          "both SYD and MEL"
        )
      } else {
        paste0(
          "Gold areas are at least ",
          selected_threshold,
          " minutes closer to CBR than the nearer of SYD or MEL"
        )
      }

      plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = data,
          ggplot2::aes(fill = .data$highlighted_advantage),
          colour = "white",
          linewidth = 0.08
        ) +
        ggplot2::scale_fill_gradient(
          low = "#F8EBD1",
          high = "#EBBF7B",
          limits = c(0, .cbr_catchment_slider_max),
          oob = scales::squish,
          na.value = "#E5E7E9",
          name = "CBR advantage\n(minutes)"
        ) +
        ggplot2::geom_point(
          data = .cbr_airports,
          ggplot2::aes(x = .data$longitude, y = .data$latitude),
          inherit.aes = FALSE,
          shape = 21,
          fill = "white",
          colour = "#1F2933",
          size = 3.5,
          stroke = 1
        ) +
        ggplot2::geom_text(
          data = .cbr_airports,
          ggplot2::aes(
            x = .data$longitude,
            y = .data$latitude,
            label = .data$name
          ),
          inherit.aes = FALSE,
          fontface = "bold",
          vjust = -0.8,
          size = 3.7,
          colour = "#1F2933"
        ) +
        ggplot2::coord_sf(
          crs = sf::st_crs(4326),
          default_crs = sf::st_crs(4326),
          datum = NA,
          expand = FALSE
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          panel.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          legend.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          legend.key = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          plot.title = ggplot2::element_text(
            face = "bold",
            size = 16,
            colour = "#1F2933"
          ),
          plot.subtitle = ggplot2::element_text(
            size = 10.5,
            colour = "#52606D"
          )
        ) +
        ggplot2::labs(
          title = "Canberra Airport drive-time catchment",
          subtitle = subtitle
        )

      if (isTRUE(input$show_towns)) {
        plot <- plot +
          ggplot2::geom_point(
            data = .cbr_towns,
            ggplot2::aes(x = .data$longitude, y = .data$latitude),
            inherit.aes = FALSE,
            size = 1.2,
            colour = "#1F2933"
          ) +
          ggplot2::geom_text(
            data = .cbr_towns,
            ggplot2::aes(
              x = .data$longitude,
              y = .data$latitude,
              label = .data$name
            ),
            inherit.aes = FALSE,
            nudge_y = 0.08,
            size = 2.8,
            colour = "#1F2933",
            check_overlap = TRUE
          )
      }

      plot
    }, res = 96)
  })
}
