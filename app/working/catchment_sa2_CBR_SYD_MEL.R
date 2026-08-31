# =============================================================================
# Canberra Airport Catchment Analysis
# =============================================================================
#
# Purpose
# -------
# Estimate the road-access catchment for Canberra Airport (CBR), compare
# accessibility with Sydney Airport (SYD), and summarise the population within
# different drive-time / airport-access bands.
#
# Main inputs
# -----------
# Catchment Estimates/
# |-- Geography/
# |   `-- SA2_2021_AUST_SHP_GDA2020/
# |       `-- SA2_2021_AUST_GDA2020.shp
# |-- Population/
# |   `-- population.csv
# `-- R Code/
#
# Main outputs
# ------------
# R Code/drive_times.csv
# R Code/cbr_syd_drive_advantage.png
#
# Notes
# -----
# - SA2 geography is based on the 2021 ASGS boundaries.
# - Population is joined using SA2 code.
# - Representative SA2 points use st_point_on_surface(), so the point always
#   falls inside the polygon.
# - The initial 400 km straight-line filter is only used to reduce the number
#   of routing API calls. It is NOT the final aviation catchment definition.
# - Drive times are calculated from each SA2 representative point to CBR and
#   SYD using openrouteservice.
# - CBR_time_advantage is defined as:
#
#       SYD drive time - CBR drive time
#
#   Therefore:
#       positive = CBR is closer
#       zero     = equal drive time
#       negative = SYD is closer
#
# =============================================================================


# =============================================================================
# 1. Libraries
# =============================================================================

library(sf)
library(tidyverse)
library(openrouteservice)
library(ggrepel)


# =============================================================================
# 2. File paths
# =============================================================================

# The script assumes the working directory is "Catchment Estimates".
#
# If necessary, set it explicitly here. It is better to avoid choose.dir()
# because it makes the script less reproducible.
#
 setwd(
   "C:/Users/MichaelHawley/OneDrive - Queenstown Airport Corporation/Documents - Team Analytics/Analysis and Reporting/Business Cases/CBR/Catchment Estimates"
 )

shp_path <- file.path(
  "app",
  "data",
  "Geography",
  "Australia",
  "SA2_2021_AUST_SHP_GDA2020",
  "SA2_2021_AUST_GDA2020.shp"
)

population_path <- file.path(
  "app",
  "data",
  "Population",
  "Australia",
  "SA2",
  "population.csv"
)

drive_times_path <- file.path(
  "app",
  "data",
  "Drive",
  "cbr_drive_times.csv"
)

map_output_path <- file.path(
  "app",
  "data",
  "misc",
  "cbr_syd_drive_advantage.png"
)


# =============================================================================
# 3. Load geography and population
# =============================================================================
getwd()
sa2 <- st_read(
  shp_path,
  quiet = TRUE
)

population <- read_delim(
  population_path,
  delim = ",",
  show_col_types = FALSE
)


# =============================================================================
# 4. Join population to SA2 geography
# =============================================================================

# Geographic codes should be treated as text rather than numbers.
population <- population %>%
  mutate(
    `SA2 code` = as.character(`SA2 code`)
  )

sa2 <- sa2 %>%
  mutate(
    SA2_CODE21 = as.character(SA2_CODE21)
  )

sa2_population <- sa2 %>%
  left_join(
    population,
    by = c("SA2_CODE21" = "SA2 code")
  )


# Check population join.
# Investigate any rows returned here before relying on catchment totals.
join_errors <- sa2_population %>%
  st_drop_geometry() %>%
  filter(is.na(`ERP 2025`)) %>%
  select(
    SA2_CODE21,
    SA2_NAME21,
    STE_NAME21
  )

message(
  "SA2s without an ERP 2025 match: ",
  nrow(join_errors)
)


# =============================================================================
# 5. Restrict geography to a broad Canberra study area
# =============================================================================

# Canberra Airport coordinates in WGS84.
cbr <- st_sfc(
  st_point(c(149.195, -35.306)),
  crs = 4326
)

# Australian Albers is appropriate for national-scale distance calculations.
catchment_sa2 <- sa2_population %>%
  st_transform(3577)

cbr <- cbr %>%
  st_transform(3577)

# Keep SA2s within 400 km straight-line distance of CBR.
# This is a computational filter only.
catchment_sa2 <- catchment_sa2 %>%
  mutate(
    distance_cbr_km = as.numeric(
      st_distance(
        st_centroid(geometry),
        cbr
      )
    ) / 1000
  ) %>%
  filter(
    distance_cbr_km <= 400
  )


# Keep only fields required later in the analysis.
catchment_sa2 <- catchment_sa2 %>%
  select(
    SA2_CODE21,
    SA2_NAME21,
    SA3_CODE21,
    SA3_NAME21,
    SA4_CODE21,
    SA4_NAME21,
    STE_CODE21,
    STE_NAME21,
    AREASQKM21,
    `ERP 2024`,
    `ERP 2025`,
    Density,
    distance_cbr_km,
    geometry
  )


# =============================================================================
# 6. Create representative SA2 origin points
# =============================================================================

# st_point_on_surface() is preferred to a simple centroid because it guarantees
# the representative point lies inside each SA2 polygon.
sa2_points <- catchment_sa2 %>%
  st_point_on_surface() %>%
  st_transform(4326)

coordinates <- st_coordinates(sa2_points)

sa2_points <- sa2_points %>%
  mutate(
    longitude = coordinates[, "X"],
    latitude = coordinates[, "Y"]
  )

origins <- sa2_points %>%
  st_drop_geometry() %>%
  select(
    SA2_CODE21,
    SA2_NAME21,
    longitude,
    latitude
  )


# Optional QA plot: check representative points visually.
p_points <- ggplot() +
  geom_sf(
    data = catchment_sa2,
    fill = NA,
    linewidth = 0.2
  ) +
  geom_sf(
    data = sa2_points,
    size = 0.7
  ) +
  theme_minimal() +
  labs(
    title = "SA2 representative points"
  )


# =============================================================================
# 7. Define competing airports
# =============================================================================

# Airports included in the surface-access comparison.
#
# CBR is the airport whose catchment we are estimating.
# SYD and MEL are treated as the major competing airports.
airports <- tibble(
  airport = c("CBR", "SYD", "MEL"),
  longitude = c(
    149.1950,
    151.1772,
    144.8430
  ),
  latitude = c(
    -35.3069,
    -33.9399,
    -37.6733
  )
)


# =============================================================================
# 8. OpenRouteService setup
# =============================================================================

# Store the ORS API key outside the script in .Renviron:
#
# ORS_API_KEY=your_actual_key_here
#
# Then restart R. The key is retrieved below.
ors_key <- Sys.getenv("ORS_API_KEY")

if (nzchar(ors_key)) {
  ors_api_key(ors_key)
}


# =============================================================================
# 9. Drive-time function
# =============================================================================

# Calculate road drive time and distance from every SA2 representative point
# to CBR, SYD and MEL.
#
# The destination columns are generated from the airport order above:
#   1 = CBR
#   2 = SYD
#   3 = MEL

get_drive_times <- function(data, airports) {

  origin_coords <- as.matrix(
    data[, c("longitude", "latitude")]
  )

  airport_coords <- as.matrix(
    airports[, c("longitude", "latitude")]
  )

  locations <- rbind(
    origin_coords,
    airport_coords
  )

  n_origins <- nrow(origin_coords)

  result <- ors_matrix(
    locations = locations,
    profile = "driving-car",
    sources = 0:(n_origins - 1),
    destinations = n_origins:(n_origins + nrow(airports) - 1),
    metrics = c("duration", "distance")
  )

  data %>%
    mutate(
      CBR_drive_minutes = result$durations[, 1] / 60,
      SYD_drive_minutes = result$durations[, 2] / 60,
      MEL_drive_minutes = result$durations[, 3] / 60,

      CBR_drive_km = result$distances[, 1] / 1000,
      SYD_drive_km = result$distances[, 2] / 1000,
      MEL_drive_km = result$distances[, 3] / 1000
    )
}


# =============================================================================
# 10. Load or refresh drive-time results
# =============================================================================

# Set TRUE once to refresh the routing results and overwrite drive_times.csv.
# After the MEL results have been written successfully, change back to FALSE.
refresh_drive_times <- FALSE

if (refresh_drive_times) {

  if (!nzchar(ors_key)) {
    stop(
      "ORS_API_KEY is not set. Add it to .Renviron before refreshing drive times."
    )
  }

  drive_times <- get_drive_times(
    origins,
    airports
  )

  write_csv(
    drive_times,
    drive_times_path
  )

} else {

  drive_times <- read_csv(
    drive_times_path,
    show_col_types = FALSE
  )
}

drive_times <- drive_times %>%
  mutate(
    SA2_CODE21 = as.character(SA2_CODE21)
  )


# =============================================================================
# 11. Add airport-comparison measures
# =============================================================================

# "Competitor" means the closer of SYD and MEL.
#
# CBR_time_advantage is therefore:
#
#   drive time to closest competing airport - drive time to CBR
#
# Interpretation:
#   positive = CBR is quicker to reach
#   zero     = CBR and the closest competitor are equally accessible
#   negative = a competing airport is quicker to reach

drive_times <- drive_times %>%
  mutate(
    competitor_drive_minutes = pmin(
      SYD_drive_minutes,
      MEL_drive_minutes,
      na.rm = TRUE
    ),

    competitor_airport = case_when(
      SYD_drive_minutes <= MEL_drive_minutes ~ "SYD",
      MEL_drive_minutes < SYD_drive_minutes ~ "MEL"
    ),

    CBR_time_advantage =
      competitor_drive_minutes - CBR_drive_minutes,

    closest_airport = case_when(
      CBR_drive_minutes <= SYD_drive_minutes &
        CBR_drive_minutes <= MEL_drive_minutes ~ "CBR",

      SYD_drive_minutes <= CBR_drive_minutes &
        SYD_drive_minutes <= MEL_drive_minutes ~ "SYD",

      TRUE ~ "MEL"
    )
  )


# =============================================================================
# 12. Join drive times back to SA2 polygons
# =============================================================================

catchment_sa2 <- catchment_sa2 %>%
  left_join(
    drive_times %>%
      select(
        SA2_CODE21,
        CBR_drive_minutes,
        SYD_drive_minutes,
        MEL_drive_minutes,
        CBR_drive_km,
        SYD_drive_km,
        MEL_drive_km,
        competitor_drive_minutes,
        competitor_airport,
        CBR_time_advantage,
        closest_airport
      ),
    by = "SA2_CODE21"
  )


# QA: identify SA2s without complete routing results.
routing_errors <- catchment_sa2 %>%
  st_drop_geometry() %>%
  filter(
    is.na(CBR_drive_minutes) |
      is.na(SYD_drive_minutes) |
      is.na(MEL_drive_minutes)
  )

message(
  "SA2s without complete drive-time results: ",
  nrow(routing_errors)
)


# =============================================================================
# 13. Create CBR drive-time bands
# =============================================================================

catchment_sa2 <- catchment_sa2 %>%
  mutate(
    CBR_drive_band = case_when(
      CBR_drive_minutes <= 45  ~ "0-45 min",
      CBR_drive_minutes <= 90  ~ "45-90 min",
      CBR_drive_minutes <= 150 ~ "90-150 min",
      CBR_drive_minutes <= 240 ~ "150-240 min",
      TRUE                     ~ "240+ min"
    ),

    CBR_drive_band = factor(
      CBR_drive_band,
      levels = c(
        "0-45 min",
        "45-90 min",
        "90-150 min",
        "150-240 min",
        "240+ min"
      )
    )
  )

cbr_summary <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(CBR_drive_band) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(CBR_drive_band) %>%
  mutate(
    cumulative_population = cumsum(population_2025)
  )
cbr_summary
# =============================================================================
# 14. Create CBR accessibility bands
# =============================================================================

# These bands compare CBR against whichever of SYD or MEL is closer.
catchment_sa2 <- catchment_sa2 %>%
  mutate(
    airport_access = case_when(
      CBR_time_advantage >= 120  ~ "CBR 120+ min closer",
      CBR_time_advantage >= 60   ~ "CBR 60-120 min closer",
      CBR_time_advantage >= 30   ~ "CBR 30-60 min closer",
      CBR_time_advantage >= -30  ~ "Within 30 min",
      CBR_time_advantage >= -60  ~ "Competitor 30-60 min closer",
      CBR_time_advantage >= -120 ~ "Competitor 60-120 min closer",
      TRUE                       ~ "Competitor 120+ min closer"
    ),

    airport_access = factor(
      airport_access,
      levels = c(
        "CBR 120+ min closer",
        "CBR 60-120 min closer",
        "CBR 30-60 min closer",
        "Within 30 min",
        "Competitor 30-60 min closer",
        "Competitor 60-120 min closer",
        "Competitor 120+ min closer"
      )
    )
  )


# =============================================================================
# 15. Population summaries
# =============================================================================

# Population within each CBR drive-time band.
catchment_summary <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(CBR_drive_band) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(CBR_drive_band) %>%
  mutate(
    cumulative_population = cumsum(population_2025)
  )

catchment_sa2 %>% colnames
summary_sa2 <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(CBR_drive_band, closest_airport) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    .groups = "drop"
  )
summary_sa2
#summary_sa2 %>% write_csv(
#  file.path("R Code", "summary_sa2.csv")
#)
# Population by CBR accessibility relative to the closer of SYD / MEL.
airport_access_summary <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(airport_access) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    sa2_count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    population_share = population_2025 / sum(population_2025)
  )
airport_access_summary

# Population by physically closest airport.
closest_airport_summary <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(closest_airport) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    sa2_count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    population_share = population_2025 / sum(population_2025)
  )


# Which competing airport is relevant for each SA2?
competitor_summary <- catchment_sa2 %>%
  st_drop_geometry() %>%
  group_by(competitor_airport) %>%
  summarise(
    population_2025 = sum(`ERP 2025`, na.rm = TRUE),
    sa2_count = n(),
    .groups = "drop"
  )

closest_airport_summary
competitor_summary
1162193

# =============================================================================
# 16. Map locations: airports and major towns
# =============================================================================

airports_plot <- airports %>%
  rename(name = airport) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  st_transform(
    st_crs(catchment_sa2)
  )


towns_plot <- tibble(
  name = c(
    "Yass",
    "Goulburn",
    "Cooma",
    "Batemans Bay",
    "Bowral",
    "Young",
    "Tumut",
    "Wagga Wagga",
    "Bega",
    "Albury"
  ),
  longitude = c(
    148.91,
    149.72,
    149.13,
    150.18,
    150.42,
    148.30,
    148.22,
    147.37,
    149.84,
    146.92
  ),
  latitude = c(
    -34.84,
    -34.75,
    -36.24,
    -35.71,
    -34.48,
    -34.31,
    -35.30,
    -35.12,
    -36.68,
    -36.08
  )
) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  st_transform(
    st_crs(catchment_sa2)
  )


# ggrepel requires ordinary x/y coordinates for label placement.
town_coords <- st_coordinates(towns_plot)

town_labels <- towns_plot %>%
  st_drop_geometry() %>%
  mutate(
    x = town_coords[, "X"],
    y = town_coords[, "Y"]
  )

airport_coords <- st_coordinates(airports_plot)

airport_labels <- airports_plot %>%
  st_drop_geometry() %>%
  mutate(
    x = airport_coords[, "X"],
    y = airport_coords[, "Y"]
  )


# =============================================================================
# 17. CBR drive-time advantage map
# =============================================================================

# The map compares CBR with the closer of SYD and MEL.
#
# Colour interpretation:
# - Purple: the nearest competitor is quicker to reach
# - White: broadly equal surface access
# - Gold: CBR is quicker to reach
#
# Each SA2 receives one colour because the routing calculation uses one
# representative point per SA2.

p_advantage <- ggplot() +

  geom_sf(
    data = catchment_sa2,
    aes(fill = CBR_time_advantage),
    colour = NA
  ) +

  scale_fill_gradient2(
    low = "#C789BB",
    mid = "white",
    high = "#EBBF7B",
    midpoint = 0
  ) +

  # Major towns
  geom_sf(
    data = towns_plot,
    size = 1.4
  ) +

  ggrepel::geom_text_repel(
    data = town_labels,
    aes(
      x = x,
      y = y,
      label = name
    ),
    size = 3,
    segment.color = NA,
    seed = 1
  ) +

  # Airports
  geom_sf(
    data = airports_plot,
    shape = 21,
    fill = "white",
    size = 4,
    stroke = 1.2
  ) +

  ggrepel::geom_text_repel(
    data = airport_labels,
    aes(
      x = x,
      y = y,
      label = name
    ),
    fontface = "bold",
    size = 4,
    segment.color = NA,
    seed = 1
  ) +

  theme_void() +

  theme(
    panel.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = NA
    )
  ) +

  labs(
    title = "Canberra Airport Drive-Time Advantage",
    subtitle = "Compared with the closer of Sydney and Melbourne airports",
    fill = "CBR advantage\n(minutes)"
  )

p_advantage


# Save transparent PNG.
map_output_path <- file.path(
  "R Code",
  "cbr_competitor_drive_advantage.png"
)

ggsave(
  filename = map_output_path,
  plot = p_advantage,
  bg = "transparent",
  width = 10,
  height = 8,
  dpi = 300
)


# =============================================================================
# 18. Closest-airport map
# =============================================================================

# This second map is useful as a QA / explanatory view. It shows which airport
# has the shortest road drive time from each SA2 representative point.

p_closest_airport <- ggplot() +

  geom_sf(
    data = catchment_sa2,
    aes(fill = closest_airport),
    colour = "white",
    linewidth = 0.1
  ) +

  geom_sf(
    data = airports_plot,
    shape = 21,
    fill = "white",
    size = 4,
    stroke = 1.2
  ) +

  ggrepel::geom_text_repel(
    data = airport_labels,
    aes(
      x = x,
      y = y,
      label = name
    ),
    fontface = "bold",
    size = 4,
    segment.color = NA,
    seed = 1
  ) +

  theme_void() +

  labs(
    title = "Closest Major Airport by Road",
    subtitle = "Based on representative SA2 origin points",
    fill = "Closest airport"
  )

p_closest_airport


# =============================================================================
# 19. Review outputs
# =============================================================================

print(catchment_summary)
print(airport_access_summary)
print(closest_airport_summary)
print(competitor_summary)


# =============================================================================
# 20. Interpretation / next step
# =============================================================================
#
# This model measures surface accessibility only.
#
# It does NOT yet estimate actual airport choice. In particular, SYD and MEL
# may attract passengers even when CBR is closer because of:
#
# - wider route networks
# - higher flight frequency
# - lower fares
# - more nonstop destinations
#
# The next modelling step would convert CBR_time_advantage into an airport
# capture probability, potentially varying by destination / route.
#
# =============================================================================

p_advantage <- ggplot() +
  geom_sf(
    data = catchment_sa2,
    aes(fill = CBR_time_advantage),
    colour = NA
  ) +
  scale_fill_gradient2(
    low = "#C789BB",   # SYD / MEL advantage
    mid = "white",     # equal drive time
    high = "#EBBF7B",  # CBR advantage
    midpoint = 0,
    name = "Drive-time advantage\n(minutes)"
  ) +
  theme_void()
p_advantage


###

catchment_sa2 <- catchment_sa2 %>%
  mutate(
    Density = as.numeric(Density)
  )

p_density <- ggplot() +
  geom_sf(
    data = catchment_sa2,
    aes(fill = Density),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    name = "Population density\n(persons / km²)"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  ) +
  labs(
    title = "Population Density in the Canberra Catchment Study Area",
    subtitle = "SA2 population density"
  )

p_density

?scale_fill_viridis_c

p_density <- ggplot() +
  geom_sf(
    data = catchment_sa2,
    aes(fill = Density),
    colour = NA,
    linewidth = 0.1
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    name = "Population density\n(persons / km²)"
  ) +

  # Towns
  geom_sf(
    data = towns_plot,
    size = 1.4
  ) +

  #ggrepel::geom_text_repel(
  #  data = town_labels,
  #  aes(
  #    x = x,
  #    y = y,
  #    label = name
  #  ),
  #  size = 3,
  #  segment.color = NA,
  #  fill = "black",
  #  alpha = 0.5,
  #  color = "white",
  #  seed = 1
  #  
  #) +

  # Airports
  geom_sf(
    data = airports_plot,
    shape = 21,
    fill = "white",
    size = 4,
    stroke = 1.2
  ) +

  #ggrepel::geom_text_repel(
  #  data = airport_labels,
  #  aes(
  #    x = x,
  #    y = y,
  #    label = name
  #  ),
  #  fontface = "bold",
  #  size = 4,
  #  segment.color = NA,
  #  color = "white",
  #  seed = 1
  #) +

  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  ) +
  labs(
    title = "Population Density in the Canberra Catchment Study Area",
    subtitle = "SA2 population density",
    fill = "Population density\n(persons / km²)"
  )

p_density
ggsave(
  filename = "R Code/catchment_population_density.png",
  plot = p_density,
  bg = "transparent",
  width = 10,
  height = 8,
  dpi = 300
)

  write_cbr_catchment_cache(
    catchment_sa2,
    output_path = "C:/Users/MichaelHawley/OneDrive - CSV Limited/06_Altitude/Altitude/app/data/CBR/cbr_catchment_map.rds"
  )
