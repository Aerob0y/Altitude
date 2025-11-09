# read from source(s), clean, save
src <- readr::read_csv("raw/rbnz_source.csv") # or your existing import
clean <- transform_rbnz(src)                   # lives in R/utils_data.R
readr::write_rds(clean, "data/rbnz.rds")
