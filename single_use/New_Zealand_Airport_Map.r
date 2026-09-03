# Load necessary libraries
library(sf)
library(readr)
library(dplyr)
# Load SA2 shapefile and population data
sa2_path <- "C:/Users/MichaelHawley/OneDrive - CSV Limited/03_Data/Mapping/NZ/2018/statsnz-statistical-area-2-2018-generalised-SHP/statistical-area-2-2018-generalised.shp"
sa2 <- st_read(sa2_path)
population_path <- "C:/Users/MichaelHawley/OneDrive - CSV Limited/03_Data/Mapping/NZ/2025/Statistical area 2 population projections, by age and sex, 2018(base)-2048 (update) - 2023.csv"
population <- read_csv(population_path)

# Inspect the data
glimpse(sa2)
glimpse(population)
names(sa2)
names(population)
head(sa2)

# Build the cache
source("update/update_utilities/update_libraries.r")
source("update/update_modules/update_nz_catchment.r")

update_nz_catchment(
  sa2 = sa2_path,
  population = population_path,

  sa2_code_column = "SA22018_V1",
  sa2_name_column = "SA22018__1",

  population_code_column = "AREA_POPPR_SUB_007",
  population_column = "OBS_VALUE",

  area_column = "LAND_AREA_",
  radius_km = 400,
  route_airports = "DUD"
)
