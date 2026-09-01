# Build the offline data cache used by mod_nz_catchment.r.
#
# This updater deliberately does not download Stats NZ files. Supply matching
# SA2 2025 polygons and 30 June 2025 ERP data downloaded from Stats NZ. Routing
# is restricted to populated SA2s whose representative point is no more than
# `radius_km` from an airport, measured in a projected NZ coordinate system.

nz_catchment_airports_path <- file.path(
  "app", "data", "NZ_CATCHMENT", "nz_airports.csv"
)

nz_catchment_cache_path <- file.path(
  "app", "data", "NZ_CATCHMENT", "nz_airport_catchment.rds"
)

.nz_read_spatial <- function(x) {
  if (inherits(x, "sf")) return(x)
  sf::st_read(x, quiet = TRUE)
}

.nz_read_population <- function(x) {
  if (is.data.frame(x)) return(tibble::as_tibble(x))

  extension <- tolower(tools::file_ext(x))
  switch(
    extension,
    csv = readr::read_csv(x, show_col_types = FALSE),
    xlsx = readxl::read_excel(x),
    xls = readxl::read_excel(x),
    stop("Population input must be a data frame, CSV, XLSX or XLS.", call. = FALSE)
  )
}

.nz_polygonize <- function(x) {
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON", warn = FALSE))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  suppressWarnings(sf::st_cast(x, "MULTIPOLYGON", warn = FALSE))
}

.nz_representative_points <- function(sa2) {
  # point_on_surface is preferable to a centroid for coastal and multipart SA2s:
  # the routing API receives a point that is inside the polygon.
  suppressWarnings(sf::st_point_on_surface(sa2)) |>
    sf::st_transform(4326)
}

.nz_candidate_pairs <- function(points, airports, radius_km) {
  airport_sf <- sf::st_as_sf(
    airports,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    sf::st_transform(2193)

  point_nztm <- sf::st_transform(points, 2193)
  distances <- sf::st_distance(point_nztm, airport_sf)

  indices <- which(
    matrix(as.numeric(distances), nrow = nrow(points)) <= radius_km * 1000,
    arr.ind = TRUE
  )

  tibble::tibble(
    sa2_code = points$sa2_code[indices[, "row"]],
    airport = airports$airport[indices[, "col"]],
    straight_line_km = as.numeric(distances[indices]) / 1000
  )
}

.nz_route_batch <- function(origins, airport) {
  origin_coordinates <- sf::st_coordinates(origins)
  destination_coordinates <- as.matrix(airport[, c("longitude", "latitude")])
  locations <- rbind(origin_coordinates, destination_coordinates)
  n_origins <- nrow(origin_coordinates)

  response <- openrouteservice::ors_matrix(
    locations = locations,
    profile = "driving-car",
    sources = 0:(n_origins - 1L),
    destinations = n_origins,
    metrics = c("duration", "distance")
  )

  tibble::tibble(
    sa2_code = origins$sa2_code,
    airport = airport$airport[[1]],
    drive_minutes = as.numeric(response$durations[, 1]) / 60,
    drive_km = as.numeric(response$distances[, 1]) / 1000
  )
}

.nz_route_candidates <- function(
  points,
  airports,
  candidates,
  batch_dir,
  batch_size = 3000,
  pause_seconds = 1
) {
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

  jobs <- candidates |>
    dplyr::arrange(.data$airport, .data$sa2_code) |>
    dplyr::group_by(.data$airport) |>
    dplyr::mutate(batch = ceiling(dplyr::row_number() / batch_size)) |>
    dplyr::ungroup() |>
    dplyr::group_split(.data$airport, .data$batch)

  purrr::map_dfr(jobs, function(job) {
    airport_code <- job$airport[[1]]
    batch_number <- job$batch[[1]]
    first_sa2 <- min(job$sa2_code)
    last_sa2 <- max(job$sa2_code)
    batch_path <- file.path(
      batch_dir,
      sprintf(
        "%s_%03d_n%04d_%s_%s.rds",
        airport_code,
        batch_number,
        nrow(job),
        first_sa2,
        last_sa2
      )
    )

    if (file.exists(batch_path)) return(readRDS(batch_path))

    origin_batch <- points |>
      dplyr::filter(.data$sa2_code %in% job$sa2_code)
    airport <- airports |>
      dplyr::filter(.data$airport == airport_code)

    result <- .nz_route_batch(origin_batch, airport) |>
      dplyr::left_join(
        dplyr::select(job, .data$sa2_code, .data$straight_line_km),
        by = "sa2_code"
      )

    saveRDS(result, batch_path)
    if (pause_seconds > 0) Sys.sleep(pause_seconds)
    result
  })
}

.nz_read_route_batches <- function(batch_dir) {
  files <- list.files(
    batch_dir,
    pattern = "_n[0-9]{4}_.+\\.rds$",
    full.names = TRUE
  )

  if (!length(files)) {
    return(tibble::tibble(
      sa2_code = character(),
      airport = character(),
      drive_minutes = double(),
      drive_km = double(),
      straight_line_km = double()
    ))
  }

  purrr::map_dfr(files, readRDS) |>
    dplyr::distinct(.data$sa2_code, .data$airport, .keep_all = TRUE)
}

.nz_complete_airports <- function(routes, required_routes) {
  required_routes |>
    dplyr::anti_join(
      dplyr::select(routes, .data$sa2_code, .data$airport),
      by = c("sa2_code", "airport")
    ) |>
    dplyr::count(.data$airport, name = "missing_routes") |>
    dplyr::right_join(
      dplyr::distinct(required_routes, .data$airport),
      by = "airport"
    ) |>
    dplyr::mutate(missing_routes = dplyr::coalesce(.data$missing_routes, 0L))
}

.nz_add_competitors <- function(access, airports) {
  major_codes <- airports$airport[airports$major_competitor]

  competitor <- access |>
    dplyr::filter(.data$airport %in% major_codes) |>
    dplyr::select(
      .data$sa2_code,
      competitor_airport = .data$airport,
      competitor_minutes = .data$drive_minutes
    )

  access |>
    dplyr::left_join(competitor, by = "sa2_code", relationship = "many-to-many") |>
    dplyr::filter(.data$airport != .data$competitor_airport) |>
    dplyr::group_by(.data$sa2_code, .data$airport) |>
    dplyr::slice_min(.data$competitor_minutes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      advantage_minutes = .data$competitor_minutes - .data$drive_minutes
    )
}

update_nz_catchment <- function(
  sa2,
  population,
  sa2_code_column,
  sa2_name_column,
  population_code_column,
  population_column,
  area_column = NULL,
  airports_path = nz_catchment_airports_path,
  output_path = nz_catchment_cache_path,
  batch_dir = file.path("app", "data", "NZ_CATCHMENT", "routing_batches"),
  progress_path = file.path("app", "data", "NZ_CATCHMENT", "routing_progress.rds"),
  route_airports = NULL,
  ors_url = NULL,
  radius_km = 400,
  batch_size = 3000,
  simplify_metres = 100,
  pause_seconds = 1
) {
  if (!requireNamespace("openrouteservice", quietly = TRUE)) {
    stop("Install 'openrouteservice' before refreshing NZ drive times.", call. = FALSE)
  }

  use_local_ors <- !is.null(ors_url) && grepl(
    "^https?://(localhost|127\\.0\\.0\\.1)(:|/|$)",
    ors_url,
    ignore.case = TRUE
  )

  if (!is.null(ors_url)) {
    previous_ors_url <- getOption("openrouteservice.url")
    options(openrouteservice.url = sub("/+$", "", ors_url))
    on.exit(options(openrouteservice.url = previous_ors_url), add = TRUE)
  }

  if (!use_local_ors && !nzchar(Sys.getenv("ORS_API_KEY"))) {
    stop(
      "Set ORS_API_KEY in .Renviron, or provide ors_url for a local ORS instance.",
      call. = FALSE
    )
  }
  if (!use_local_ors) {
    openrouteservice::ors_api_key(Sys.getenv("ORS_API_KEY"))
  }

  airports <- readr::read_csv(airports_path, show_col_types = FALSE) |>
    dplyr::mutate(
      selectable = as.logical(.data$selectable),
      major_competitor = as.logical(.data$major_competitor)
    )

  polygons <- .nz_read_spatial(sa2)
  population_data <- .nz_read_population(population)

  required_sa2 <- c(sa2_code_column, sa2_name_column)
  required_population <- c(population_code_column, population_column)
  if (!all(required_sa2 %in% names(polygons))) {
    stop("SA2 input is missing: ", paste(setdiff(required_sa2, names(polygons)), collapse = ", "), call. = FALSE)
  }
  if (!all(required_population %in% names(population_data))) {
    stop("Population input is missing: ", paste(setdiff(required_population, names(population_data)), collapse = ", "), call. = FALSE)
  }

  population_lookup <- population_data |>
    dplyr::transmute(
      sa2_code = as.character(.data[[population_code_column]]),
      population_2025 = readr::parse_number(as.character(.data[[population_column]]))
    ) |>
    dplyr::group_by(.data$sa2_code) |>
    dplyr::summarise(population_2025 = sum(.data$population_2025, na.rm = TRUE), .groups = "drop")

  polygons <- polygons |>
    dplyr::transmute(
      sa2_code = as.character(.data[[sa2_code_column]]),
      sa2_name = as.character(.data[[sa2_name_column]]),
      source_area_sq_km = if (is.null(area_column)) NA_real_ else as.numeric(.data[[area_column]])
    ) |>
    dplyr::left_join(population_lookup, by = "sa2_code") |>
    dplyr::filter(!is.na(.data$population_2025), .data$population_2025 > 0) |>
    sf::st_transform(2193) |>
    .nz_polygonize()

  calculated_area_sq_km <- as.numeric(sf::st_area(polygons)) / 1e6
  polygons <- polygons |>
    dplyr::mutate(
      area_sq_km = dplyr::if_else(
        is.na(.data$source_area_sq_km),
        calculated_area_sq_km,
        .data$source_area_sq_km
      ),
      population_density = .data$population_2025 / .data$area_sq_km
    )

  points <- .nz_representative_points(polygons) |>
    dplyr::select(.data$sa2_code)
  eligible_pairs <- .nz_candidate_pairs(points, airports, radius_km)

  # Eligibility is capped at 400 km, but each eligible SA2 still needs a route
  # to every major competitor so that a distant alternative is not silently
  # treated as missing. Distinct() avoids repeating the same route where the
  # SA2 is eligible for several selectable airports.
  major_codes <- airports$airport[airports$major_competitor]
  required_routes <- dplyr::bind_rows(
    eligible_pairs,
    eligible_pairs |>
      dplyr::select(.data$sa2_code) |>
      dplyr::distinct() |>
      tidyr::crossing(airport = major_codes) |>
      dplyr::mutate(straight_line_km = NA_real_)
  ) |>
    dplyr::arrange(.data$sa2_code, is.na(.data$straight_line_km)) |>
    dplyr::distinct(.data$sa2_code, .data$airport, .keep_all = TRUE)

  if (is.null(route_airports)) route_airports <- airports$airport
  unknown_airports <- setdiff(route_airports, airports$airport)
  if (length(unknown_airports)) {
    stop("Unknown airport(s): ", paste(unknown_airports, collapse = ", "), call. = FALSE)
  }

  routes_to_run <- required_routes |>
    dplyr::filter(.data$airport %in% route_airports)

  if (nrow(routes_to_run)) {
    .nz_route_candidates(
      points = points,
      airports = airports,
      candidates = routes_to_run,
      batch_dir = batch_dir,
      batch_size = batch_size,
      pause_seconds = pause_seconds
    )
  }

  saved_routes <- .nz_read_route_batches(batch_dir)
  completion <- .nz_complete_airports(saved_routes, required_routes)
  complete_airports <- completion$airport[completion$missing_routes == 0L]
  major_codes <- airports$airport[airports$major_competitor]
  missing_major_airports <- setdiff(major_codes, complete_airports)

  progress <- list(
    metadata = list(
      updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      radius_km = radius_km
    ),
    completion = completion,
    routes = saved_routes
  )
  dir.create(dirname(progress_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(progress, progress_path, compress = "gzip")

  if (length(missing_major_airports)) {
    message(
      "Saved routing progress. Run the remaining major airport(s): ",
      paste(missing_major_airports, collapse = ", ")
    )
    return(invisible(progress))
  }

  access <- saved_routes |>
    dplyr::filter(.data$airport %in% complete_airports) |>
    dplyr::left_join(
      eligible_pairs |>
        dplyr::transmute(
          .data$sa2_code,
          .data$airport,
          eligible_within_radius = TRUE
        ),
      by = c("sa2_code", "airport")
    ) |>
    dplyr::mutate(eligible_within_radius = dplyr::coalesce(.data$eligible_within_radius, FALSE)) |>
    .nz_add_competitors(airports)

  map_polygons <- polygons |>
    dplyr::select(
      .data$sa2_code,
      .data$sa2_name,
      .data$population_2025,
      .data$population_density,
      .data$area_sq_km
    ) |>
    sf::st_simplify(dTolerance = simplify_metres, preserveTopology = TRUE) |>
    .nz_polygonize() |>
    sf::st_transform(4326)

  cache <- list(
    metadata = list(
      created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      population_reference_date = as.Date("2025-06-30"),
      geography = "SA2 2025",
      routing_source = "OpenRouteService",
      radius_km = radius_km,
      advantage_definition = "nearest other major-airport minutes minus selected-airport minutes"
    ),
    airports = airports,
    sa2 = map_polygons,
    access = access
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cache, output_path, compress = "gzip")
  message(
    "Wrote NZ airport catchment cache for: ",
    paste(sort(unique(access$airport[access$eligible_within_radius])), collapse = ", "),
    "\n",
    normalizePath(output_path, winslash = "/", mustWork = FALSE)
  )
  invisible(cache)
}
