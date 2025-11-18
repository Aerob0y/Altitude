
update_fuel_data("data/Fuel/fuel_data.csv", 6)
update_adp_data()
update_bond_data()
update_tentwo("data/FRED/", "https://fred.stlouisfed.org/graph/fredgraph.xls?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=T10Y2Y&scale=left&cosd=2020-11-14&coed=2025-11-14&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-11-15&revision_date=xxxx&nd=1976-06-01")
rbnz_fetch_xlsx(keys = c(
  "hb1-daily", 
  "hb2-daily",
  "hc35",
  "hm1",
  "hm2",
  "hm3",
  "hm4",
  "hm5",
  "hm6",
  "hm7",
  "hm8",
  "hm9",
  "hm10",
  "hm14",
  "hs32"
  )
)