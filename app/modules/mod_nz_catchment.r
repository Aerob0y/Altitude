# New Zealand airport road-access catchment module.
# All spatial and routing data are prepared by update_nz_catchment().

nz_catchment_cache_path <- getOption(
  "altitude.nz_catchment_cache",
  file.path("app", "data", "NZ_CATCHMENT", "nz_airport_catchment.rds")
)

.nz_add_straight_line_fallback <- function(cache, fallback_speed_kmh = 60) {
  access <- cache$access
  missing_drive_time <- !is.finite(access$drive_minutes)

  if (any(missing_drive_time)) {
    points <- suppressWarnings(
      sf::st_point_on_surface(sf::st_transform(cache$sa2, 2193))
    )
    airports <- cache$airports |>
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      ) |>
      sf::st_transform(2193)
    distances <- sf::st_distance(points, airports)

    distance_lookup <- tibble::tibble(
      sa2_code = rep(points$sa2_code, each = nrow(airports)),
      airport = rep(airports$airport, times = nrow(points)),
      fallback_straight_line_km = as.numeric(t(distances)) / 1000
    )

    access <- access |>
      dplyr::left_join(distance_lookup, by = c("sa2_code", "airport")) |>
      dplyr::mutate(
        straight_line_km = dplyr::coalesce(
          .data$straight_line_km,
          .data$fallback_straight_line_km
        )
      ) |>
      dplyr::select(-.data$fallback_straight_line_km)
  }

  access <- access |>
    dplyr::mutate(
      travel_method = dplyr::if_else(
        is.finite(.data$drive_minutes),
        "Road route",
        paste0("Straight-line estimate at ", fallback_speed_kmh, " km/h")
      ),
      travel_minutes = dplyr::if_else(
        is.finite(.data$drive_minutes),
        .data$drive_minutes,
        .data$straight_line_km / fallback_speed_kmh * 60
      ),
      travel_km = dplyr::if_else(
        is.finite(.data$drive_km),
        .data$drive_km,
        .data$straight_line_km
      )
    )

  major_codes <- cache$airports$airport[cache$airports$major_competitor]
  competitors <- access |>
    dplyr::filter(
      .data$airport %in% major_codes,
      is.finite(.data$travel_minutes)
    ) |>
    dplyr::select(
      .data$sa2_code,
      competitor_airport = .data$airport,
      competitor_minutes = .data$travel_minutes,
      competitor_method = .data$travel_method
    )

  access |>
    dplyr::select(-dplyr::any_of(c(
      "competitor_airport", "competitor_minutes", "advantage_minutes"
    ))) |>
    dplyr::left_join(competitors, by = "sa2_code", relationship = "many-to-many") |>
    dplyr::filter(.data$airport != .data$competitor_airport) |>
    dplyr::group_by(.data$sa2_code, .data$airport) |>
    dplyr::slice_min(.data$competitor_minutes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      advantage_minutes = .data$competitor_minutes - .data$travel_minutes
    )
}

.nz_catchment_cache <- local({
  result <- list(data = NULL, error = NULL)
  tryCatch({
    if (file.exists(nz_catchment_cache_path)) {
      cache <- readRDS(nz_catchment_cache_path)
      required <- c("metadata", "airports", "sa2", "access")
      missing <- setdiff(required, names(cache))
      if (length(missing)) stop("Cache is missing: ", paste(missing, collapse = ", "))
      if (!inherits(cache$sa2, "sf")) stop("Cache $sa2 object is not an sf layer.")
      cache$access <- .nz_add_straight_line_fallback(cache)
      result$data <- cache
    }
  }, error = function(e) result$error <- conditionMessage(e))
  result
})

.nz_default_airport <- local({
  cache <- .nz_catchment_cache$data
  airports <- cache$airports
  if (is.null(airports)) return("ZQN")
  selectable <- intersect(
    airports$airport[airports$selectable],
    unique(cache$access$airport)
  )
  if ("ZQN" %in% selectable) "ZQN" else selectable[[1]]
})

if (exists("module_notes", inherits = TRUE)) {
  module_notes$nz_catchment <- list(
    "New Zealand airport catchments",
    "SA2 population and cached OpenRouteService drive times",
    paste(
      "Select an airport and the minimum time advantage it must have over",
      "the nearest other major airport. Routing is limited to populated SA2s",
      "within 400 km of each airport."
    )
  )
}

mod_nz_catchment_ui <- function(id) {
  ns <- shiny::NS(id)
  cache <- .nz_catchment_cache$data
  airports <- cache$airports

  choices <- if (is.null(airports)) {
    c("Queenstown Airport" = "ZQN")
  } else {
    selectable_airports <- airports |>
      dplyr::filter(
        .data$selectable,
        .data$airport %in% unique(cache$access$airport)
      ) |>
      dplyr::arrange(.data$airport_name)
    stats::setNames(
      selectable_airports$airport,
      paste0(selectable_airports$airport_name, " (", selectable_airports$airport, ")")
    )
  }

  shiny::tags$div(
    class = "module-page",
    module_note("nz_catchment"),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        class = "csv-sidebar",
        position = "left",
        shiny::selectInput(ns("airport"), "Airport", choices, selected = .nz_default_airport),
        shiny::sliderInput(
          ns("threshold"),
          "Required time saving versus nearest major airport",
          min = 0,
          max = 180,
          value = 0,
          step = 5,
          post = " minutes",
          width = "100%"
        ),
        shiny::checkboxInput(ns("show_density"), "Show population-density heatmap", TRUE),
        #shiny::actionButton(ns("zoom"), "Zoom to selected airport", icon = bsicons::bs_icon("geo-alt")),
        shiny::helpText(
          paste(
            "An SA2 is included when the selected airport is at least this many minutes",
            "quicker than its nearest other major airport. At zero, it only needs to be",
            "the quickest or tied. Missing road routes use a straight-line estimate at 60 km/h."
          )
        ),
        shiny::tags$hr(),
        shiny::uiOutput(ns("summary")),
        shiny::downloadButton(ns("download_sa2"), "Download included SA2s", class = "btn-sm"),
        style = "height: 100%;"
      ),
      bslib::layout_columns(
        bslib::card(
          class = "module-chart-card",
          full_screen = TRUE,
          leaflet::leafletOutput(ns("map"), height = "720px", width = "100%")
        ),
        bslib::card(
          class = "module-chart-card",
          full_screen = TRUE,
          plotly::plotlyOutput(ns("waterfall"), height = "720px", width = "100%")
        ),
        col_widths = c(8, 4)
      )
    )
  )
}

mod_nz_catchment_server <- function(id, selected_tab, activate_on) {
  shiny::moduleServer(id, function(input, output, session) {
    enabled <- shiny::reactive(identical(selected_tab(), activate_on))

    cache_message <- function() {
      if (!is.null(.nz_catchment_cache$error)) {
        return(paste("The NZ catchment cache could not be loaded:", .nz_catchment_cache$error))
      }
      paste0(
        "NZ catchment cache not found at '", nz_catchment_cache_path,
        "'. Run update_nz_catchment() and restart the app."
      )
    }

    selected_data <- shiny::reactive({
      shiny::req(enabled(), input$airport, input$threshold)
      shiny::validate(shiny::need(!is.null(.nz_catchment_cache$data), cache_message()))
      cache <- .nz_catchment_cache$data

      access <- cache$access |>
        dplyr::filter(
          .data$airport == input$airport,
          .data$eligible_within_radius
        )

      cache$sa2 |>
        dplyr::left_join(access, by = "sa2_code") |>
        dplyr::mutate(
          included = !is.na(.data$advantage_minutes) &
            .data$advantage_minutes >= as.numeric(input$threshold),
          popup = paste0(
            "<strong>", .data$sa2_name, "</strong><br>",
            "Population: ", scales::comma(.data$population_2025), "<br>",
            "Density: ", scales::comma(.data$population_density, accuracy = 0.1), " people/km²<br>",
            input$airport, " travel time: ", round(.data$travel_minutes), " min<br>",
            "Method: ", .data$travel_method, "<br>",
            "Nearest major alternative: ", .data$competitor_airport, " (",
            round(.data$competitor_minutes), " min)<br>",
            "Alternative method: ", .data$competitor_method, "<br>",
            "Time saving: ", round(.data$advantage_minutes), " min"
          )
        )
    })

    output$map <- leaflet::renderLeaflet({
      shiny::req(enabled())
      shiny::validate(shiny::need(!is.null(.nz_catchment_cache$data), cache_message()))
      cache <- .nz_catchment_cache$data
      map_sa2 <- cache$sa2 |>
        dplyr::mutate(
          map_population_density = dplyr::if_else(
            is.finite(.data$population_density) & .data$population_density >= 0,
            .data$population_density,
            NA_real_
          )
        )
      density_values <- log1p(map_sa2$map_population_density)
      finite_density_values <- density_values[is.finite(density_values)]
      shiny::validate(shiny::need(
        length(finite_density_values) > 0L,
        "The catchment cache contains no finite population-density values. Rebuild the cache and restart the app."
      ))
      density_palette <- leaflet::colorNumeric(
        palette = "YlGnBu",
        domain = finite_density_values,
        na.color = "#E5E7E9"
      )

      map <- leaflet::leaflet(map_sa2, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addTiles(group = "Base map") |>
        leaflet::addPolygons(
          group = "Population density",
          stroke = FALSE,
          fillColor = ~density_palette(log1p(map_population_density)),
          fillOpacity = 0.62,
          smoothFactor = 0.5
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          pal = density_palette,
          values = finite_density_values,
          title = "Population density<br>(log scale)",
          group = "Population density"
        ) |>
        leaflet::addCircleMarkers(
          data = cache$airports,
          lng = ~longitude,
          lat = ~latitude,
          label = ~paste0(airport_name, " (", airport, ")"),
          radius = 4,
          color = "#1F2933",
          fillColor = "white",
          fillOpacity = 1,
          weight = 1,
          group = "Airports"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("Population density", "Catchment", "Airports"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::setView(lng = 172.5, lat = -41.2, zoom = 5)

      if (requireNamespace("leaflet.extras2", quietly = TRUE)) {
        map <- leaflet.extras2::addEasyprint(
          map,
          options = leaflet.extras2::easyprintOptions(
            title = "Download map",
            position = "topleft",
            exportOnly = TRUE,
            filename = "new_zealand_airport_catchment"
          )
        )
      }
      map
    })

    shiny::observeEvent(selected_data(), {
      data <- selected_data()
      highlighted <- data |>
        dplyr::filter(.data$included)

      proxy <- leaflet::leafletProxy("map", session = session) |>
        leaflet::clearGroup("Catchment")

      if (nrow(highlighted)) {
        proxy |>
          leaflet::addPolygons(
            data = highlighted,
            group = "Catchment",
            color = "#C17D11",
            weight = 1.5,
            opacity = 0.95,
            fillColor = "#F4C86A",
            fillOpacity = 0.34,
            label = lapply(highlighted$popup, htmltools::HTML),
            highlightOptions = leaflet::highlightOptions(weight = 3, bringToFront = TRUE)
          )
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$show_density, {
      proxy <- leaflet::leafletProxy("map", session = session)
      if (isTRUE(input$show_density)) {
        proxy |> leaflet::showGroup("Population density")
      } else {
        proxy |> leaflet::hideGroup("Population density")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$zoom, {
      shiny::req(input$airport, .nz_catchment_cache$data)
      airport <- .nz_catchment_cache$data$airports |>
        dplyr::filter(.data$airport == input$airport)
      leaflet::leafletProxy("map", session = session) |>
        leaflet::setView(airport$longitude[[1]], airport$latitude[[1]], zoom = 7)
    })

    output$summary <- shiny::renderUI({
      if (is.null(.nz_catchment_cache$data)) {
        return(shiny::tags$p(class = "text-danger", cache_message()))
      }
      included <- selected_data() |>
        dplyr::filter(.data$included)
      shiny::tags$div(
        shiny::tags$strong("Highlighted catchment"),
        shiny::tags$br(),
        scales::comma(sum(included$population_2025, na.rm = TRUE)), " people (2025)",
        shiny::tags$br(),
        scales::comma(nrow(included)), " SA2 areas",
        shiny::tags$br(),
        scales::comma(sum(included$area_sq_km, na.rm = TRUE), accuracy = 1), " km²"
      )
    })

    waterfall_data <- shiny::reactive({
      data <- selected_data() |>
        sf::st_drop_geometry() |>
        dplyr::filter(.data$included, is.finite(.data$travel_minutes)) |>
        dplyr::mutate(band_end = pmax(30, ceiling(.data$travel_minutes / 30) * 30)) |>
        dplyr::group_by(.data$band_end) |>
        dplyr::summarise(population = sum(.data$population_2025, na.rm = TRUE), .groups = "drop")
      if (!nrow(data)) return(data)
      data |>
        tidyr::complete(band_end = seq(30, max(.data$band_end), by = 30), fill = list(population = 0)) |>
        dplyr::arrange(.data$band_end) |>
        dplyr::mutate(
          band_label = paste0(.data$band_end - 30, "–", .data$band_end, " min")
        )
    })

    output$waterfall <- plotly::renderPlotly({
      data <- waterfall_data()
      shiny::validate(shiny::need(nrow(data), "No SA2s meet this threshold."))
      plotly::plot_ly(
        type = "waterfall",
        measure = c(rep("relative", nrow(data)), "total"),
        x = c(data$band_label, "Total"),
        y = c(data$population, 0),
        text = scales::comma(c(data$population, sum(data$population))),
        textposition = "outside",
        hovertemplate = "%{x}<br>%{text} people<extra></extra>",
        connector = list(line = list(color = "#A7B0BA")),
        increasing = list(marker = list(color = "#F4C86A")),
        totals = list(marker = list(color = "#C17D11"))
      ) |>
        plotly::layout(
          title = list(text = "Catchment population by drive time"),
          xaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = c(data$band_label, "Total")
          ),
          yaxis = list(title = "Estimated residents", tickformat = ",d"),
          showlegend = FALSE,
          margin = list(b = 110)
        ) |>
        plotly::config(
          displaylogo = FALSE,
          toImageButtonOptions = list(filename = paste0("nz_catchment_", input$airport))
        )
    })

    output$download_sa2 <- shiny::downloadHandler(
      filename = function() paste0("nz_catchment_", input$airport, "_", input$threshold, "min.csv"),
      content = function(file) {
        selected_data() |>
          sf::st_drop_geometry() |>
          dplyr::filter(.data$included) |>
          dplyr::select(
            .data$sa2_code, .data$sa2_name, .data$population_2025,
            .data$population_density, .data$area_sq_km, .data$drive_minutes,
            .data$drive_km, .data$straight_line_km, .data$travel_minutes,
            .data$travel_km, .data$travel_method, .data$competitor_airport,
            .data$competitor_minutes, .data$competitor_method,
            .data$advantage_minutes
          ) |>
          readr::write_csv(file)
      }
    )
  })
}
