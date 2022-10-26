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
#' # Get version 22.2.0 of the 'depression-inpatients' database
#' d <- getData("depression-inpatients", "22.2.0")
#'
#' # Show variable description
#' d$variableDescription()
#'
#' # Open online documentation
#' d$openDocumentation()
#'
#' # Analyze using metapsyTools
#' library(metapsyTools)
#' runMetaAnalysis(d, which.run = "combined")
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
  class(dataIndex) = c("listData", "data.frame")
  return(dataIndex)

}



#' Print `listData` objects
#'
#' Prints the available databases returned by [listData()].
#'
#' @param x An object of class `listData`.
#' @param ... Additional arguments.
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com},
#' Paula Kuper \email{paula.r.kuper@@gmail.com}, Pim Cuijpers \email{p.cuijpers@@vu.nl}
#'
#' @importFrom crayon green blue bold cyan
#' @export
#' @method print listData

print.listData = function(x, ...){
  data = x[-which(x[,1] == "template"),]
  message(crayon::blue(
    crayon::bold("\n", "Available databases", "\n",
                 "-------------------")))
  apply(data, 1, function(y) {
    paste0(
      crayon::bold(
        crayon::green(paste(y[1]))),
      "\n",
      crayon::cyan(paste0("\u2192 ", y[2], "\n")))
    }) -> res
  message(res)
  message(
    "To retrieve the data, use the database shorthand in ",
    crayon::blue("getData()"), "; \n \u2192 e.g. ",
    crayon::blue('getData("depression-inpatients")'), ".")
}


