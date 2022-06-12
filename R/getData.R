# library(RCurl)
# library(jsonlite)
# library(httr)
#
#
# paste0(
#     "https://raw.githubusercontent.com/",
#     "metapsy-project/metapsyDocs/master/",
#     "static/uploads/data-index.csv?",
#     "token=GHSAT0AAAAAABTTJNPD234AZFUZFCNVN6NCYVGFE3Q"
# ) ->
#
#
#
#
# plot(1:100)
#
# data
#
# download <- getURL("https://raw.githubusercontent.com/metapsy-project/data-template/master/data.csv")
# version
# data <- read.csv(text = download, sep = ";")
#
# concept_Dois <- c("data.template" = "10.5281/zenodo.6617815")
#
# zenodoUrl <- paste0(
#     "https://zenodo.org/api/deposit/depositions?",
#     "access_token=Bounk4ySHPIYxrFMWN49jyenJZ1Uy6t",
#     "Bhico7tuZ3iW6cp1hJ3m9FIY6HcvX&all_versions=1"
# )
#
# httr::GET(zenodoUrl) -> d
#
# data <- fromJSON(rawToChar(d$content))
#
# data$conceptdoi
# data$title
#
