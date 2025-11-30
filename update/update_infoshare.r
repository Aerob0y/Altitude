

library(rvest)
library(httr)

url <- "https://infoshare.stats.govt.nz/DownloadFile.aspx?format=csv&seriesid=CPIQ.S1"
temp <- tempfile(fileext = ".csv")
download.file(url, temp, mode = "wb")

data <- read.csv(temp)
head(data)

https://portal.apis.stats.govt.nz/profile


install.packages("rsdmx")
library(rsdmx) 

#https://www.stats.govt.nz/tools/aotearoa-data-explorer/ade-api-user-guide/using-the-ade-api-in-r/
#https://explore.data.stats.govt.nz/?tm=Population%20projections&pg=0&snb=33&isAvailabilityDisabled=false

dataurl <- "https://api.data.stats.govt.nz/rest/data/STATSNZ,POPPR_SUB_001,1.0/001+002+003+076+011+012+013+015+016+017+018+019+020+021+022+023+024+025+026+027+028+029+030+031+032+033+034+035+036+037+038+039+040+041+042+043+044+045+046+047+048+049+050+051+052+053+054+055+056+057+058+059+060+062+063+064+065+066+067+068+069+070+071+072+073+074+075..SEX3.TOTALALLAGES.MEDIUM"
metadataUrl  <- "https://api.data.stats.govt.nz/rest/dataflow/STATSNZ/POPPR_SUB_001/1.0?references=all"
	
key <- "75b4d41ee87f4524a4c13090f907b4e4" 
sdmx_data <- readSDMX(dataurl, verbose = TRUE, headers = c(`Ocp-Apim-Subscription-Key` = key))
sdmx_metadata <- readSDMX(metadataUrl, verbose = TRUE, headers = c(`Ocp-Apim-Subscription-Key` = key)) 
sdmx_data <- rsdmx:::setDSD(sdmx_data, sdmx_metadata)
	
(data <- as.data.frame(sdmx_data, labels = TRUE)) 
