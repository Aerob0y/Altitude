# New Zealand airport catchment data

The live app reads `nz_airport_catchment.rds`; it never calls a routing API.

Build the cache with `update_nz_catchment()` from
`update/update_modules/update_nz_catchment.r`. Supply:

- Stats NZ SA2 2025 clipped polygons;
- Stats NZ 2023-base estimated resident population at 30 June 2025 by SA2;
- an `ORS_API_KEY` stored in `.Renviron`.

Example (replace the column names with those in the downloaded files):

```r
source("update/update_utilities/update_libraries.r")
source("update/update_modules/update_nz_catchment.r")

update_nz_catchment(
  sa2 = "path/to/statistical-area-2-2025-clipped.shp",
  population = "path/to/sa2-population-2025.csv",
  sa2_code_column = "SA22025_V1_00",
  sa2_name_column = "SA22025_V1_00_NAME",
  population_code_column = "SA2 code",
  population_column = "Estimated resident population"
)
```

Only populated SA2s within 400 km straight-line distance of an airport are sent
to OpenRouteService as eligible catchment origins. Those origins are also
routed to every flagged major competitor so the drive-time advantage remains a
valid comparison when the alternative airport is more than 400 km away.
Completed routing batches are retained in
`routing_batches/`, so the update resumes after an interruption. Delete those
batches only when the SA2 routing points, airport coordinates or routing method
change.

The app requires `leaflet`. Install `leaflet.extras2` as well to show the map's
PNG download control.
