
update_fuel_data("data/Fuel/fuel_data.csv", 6)
update_adp_data()
update_bond_data()
download_latest_ect()
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
