#' List all available databases
#'
#' This function lists all publicly available Metapsy databases. It prints all
#' database [shorthands](https://docs.metapsy.org/databases/#shorthand), which can
#' be used to import data using [getData()].
#'
#' @usage listData()
#'
#'
#' @return Returns a \code{data.frame} of all available databases by shorthand,
#' along with a URL for the database documentation entry.
#'
#' @examples
#' \dontrun{
#' # List all available databases
#' listData()
#'
#' # Get latest version of the 'depression-psyctr' database
#' d <- getData("depression-psyctr")
#'
#' # Get version 22.2.0 of the 'depression-psyctr' database
#' d <- getData("depression-psyctr", "22.2.0")
#'
#' # Show variable description
#' d$variableDescription()
#'
#' # Open online documentation
#' d$openDocumentation()
#'
#' # Analyze using metapsyTools
#' library(metapsyTools)
#' runMetaAnalysis(d)
#' }
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com}
#'
#' @seealso \code{\link{getData}}
#'
#'
#' @importFrom crayon green
#' @importFrom RCurl getURL
#' @importFrom utils read.csv
#'
#' @export listData

listData = function(){

  dataIndex = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    "metapsyData/master/pkgdown/assets/data/data-index.csv"
  ) %>% RCurl::getURL() %>%
    read.csv(text = ., sep = ";") %>%
    {rownames(.) = .$shorthand; .[,-1]} %>%
    {.$url = paste0("docs.metapsy.org/databases/", .$url);
    .$shorthand = rownames(.);
    rownames(.) = NULL;
    .[c("shorthand", "url")]}

  message("- ", crayon::green("[OK] "),
          "Retrieving available databases...")
  return(dataIndex)

}
