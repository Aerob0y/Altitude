library(jsonlite)
library(dplyr)

hb1_guide <- guide_rbnz %>%
  filter(Data == "hb1") %>%
  transmute(
    id = ID,
    name = Name,
    dim = Dim,
    tick = Tick,
    prefix = Prefix,
    style = Style
  )

hb1_guide

write_json(hb1_guide, "C:/Users/MichaelHawley/OneDrive - CSV Limited/Published/Standalone/hb1/hb1_guide.json", pretty = TRUE, auto_unbox = TRUE)

