library(rsconnect)
library(tidyverse)
source("update/update_rbnz.r")
source("update/update_fuel.r")
source("update/update_adp.r")
source("update/update.r")

rsconnect::setAccountInfo(name='csv-consulting',
        token='9664EF3AD6995975E62D4AAC6F23C13E',
        secret='ki9IJJ6LNOywnWkuRBZ9gst41UR1PL+MUOGAPo35')
rsconnect::deployApp()
