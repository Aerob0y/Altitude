

library(rvest)
library(httr)
# Function -----------------------------------------------------------------


update_bond_data <- function() {
  download_if_updated_etag("https://www.tenancy.govt.nz/assets/Uploads/Tenancy/Rental-bond-data/Detailed-Monthly-TLA-Tenancy-v2.csv", "data/Bond/Bond.csv")
}
