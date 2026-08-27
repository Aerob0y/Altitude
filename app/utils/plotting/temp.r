assign_series_colours <- function(series) {
  if (!"Palette"   %in% names(series)) series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"

  s <- series
  s <- s %>% group_by(Palette, ColourKey)
  s <- s %>%
    mutate(
      .idx = row_number(),
      colour = case_when(
        Palette == "manual" ~ unname(cc[ColourKey]),

        Palette == "qual" ~
          pal_qual_main[(.idx - 1) %% length(pal_qual_main) + 1],

        Palette == "navy" ~
          palettes$navy[(.idx - 1) %% length(palettes$navy) + 1],

        Palette == "teal" ~
          palettes$teal[(.idx - 1) %% length(palettes$teal) + 1],

        Palette == "ruby" ~
          palettes$ruby[(.idx - 1) %% length(palettes$ruby) + 1],

        Palette == "gold" ~
          palettes$gold[(.idx - 1) %% length(palettes$gold) + 1],

        TRUE ~ palettes$grey["base"]
      )
    ) %>%
    ungroup() %>%
    select(-.idx)
  s
}


assign_series_colours <- function(series) {
  if (!"Palette"   %in% names(series)) series$Palette <- "qual"
  if (!"ColourKey" %in% names(series)) series$ColourKey <- "teal_base"

  s <- series
  s <- s %>% group_by(Palette, ColourKey)
  s <- s %>%
    mutate(
      .idx = row_number(),
      colour = case_when(


        Palette == "navy" ~
          palettes$navy[(.idx - 1) %% length(palettes$navy) + 1],

        Palette == "teal" ~
          palettes$teal[(.idx - 1) %% length(palettes$teal) + 1],

        Palette == "ruby" ~
          palettes$ruby[(.idx - 1) %% length(palettes$ruby) + 1],

        Palette == "gold" ~
          palettes$gold[(.idx - 1) %% length(palettes$gold) + 1]
      )
    ) %>%
    ungroup() %>%
    select(-.idx)
  s
}


