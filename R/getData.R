#' Import Metapsy databases into the R environment
#'
#' This function allows to download all publicly available versions of Metapsy
#' databases into your R environment.
#'
#' @usage getData(shorthand, version = NULL)
#'
#' @param shorthand \code{character}. The shorthand assigned to the database that
#' should be downloaded. Shorthands are listed under "Metadata" in the
#' [database documentation entry](https://docs.metapsy.org/databases). Alternatively,
#' all available databases and their shortcodes can be accessed by running [listData()].
#' @param version \code{character}. The version number to be downloaded. Default is
#' \code{NULL}, which downloads the latest version.
#'
#' @return Returns the selected database as a \code{data.frame} object. Metadata is added
#' via attributes to the object:
#' * \code{attr(x,"databaseDoi")} returns the database/"concept" DOI.
#' * \code{attr(x,"versionDoi")} returns the database version DOI.
#' * \code{attr(x,"publicationDate")} returns the publication date of
#' the database (version).
#' * \code{attr(x,"lastSearch")} returns the last search update.
#' * \code{attr(x,"documentation")} returns the URL to the documentation entry for
#' the database.
#'
#'
#' @examples
#' \dontrun{
#' # List all available databases
#' listData()
#'
#' # Get latest version of the 'depression-psyctr' database
#' d <- getData("depression-psyctr")
#'
#' # Get version 22.2 of the 'depression-psyctr' database
#' d <- getData("depression-psyctr")
#'
#' # Show variable description
#' variableDescription(d)
#'
#' # Open online documentation
#' openDocs(d)}
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com}
#'
#' @seealso \code{\link{listData}}, \code{\link{variableDescription}},
#' \code{\link{openDocs}}
#'
#' @details After the databases has been downloaded and saved as an object,
#' the \code{\link{variableDescription}} function can be used to print a
#' variable description. The  \code{\link{openDocs}} function opens the
#' online documentation entry for the database.
#'
#' @importFrom crayon green magenta
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom readr parse_number locale
#' @importFrom utils read.csv
#'
#' @export getData


getData = function(shorthand,
                   version = NULL){

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Version Check                                             #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  if (!is.null(version)){
    message("- ", crayon::green("[OK] "), "Downloading '",
            shorthand, "' database (version ", version, ") ...")
  } else {
    message("- ", crayon::green("[OK] "), "Downloading '",
            shorthand, "' database...")
  }


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Get available shorthands                                  #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  dataIndex = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    "metapsyData/master/pkgdown/assets/data/data-index.csv"
  ) %>% RCurl::getURL() %>%
    read.csv(text = ., sep = ";") %>%
    {rownames(.) = .$shorthand; .[,-1]}


  if (!shorthand %in% rownames(dataIndex)){
    stop("Shorthand '", shorthand, "' not found. ",
         "All available datasets and their",
         " respective shorthands are documented",
         " at docs.metapsy.org/databases ",
         "(see 'Metadata' section).")
  }


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Retrieve Metadata for specified shorthand                 #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  metadata = paste0(
    "https://zenodo.org/api/deposit/depositions?",
    "access_token=Bounk4ySHPIYxrFMWN49jyenJZ1Uy6t",
    "Bhico7tuZ3iW6cp1hJ3m9FIY6HcvX&all_versions=1"
  ) %>% httr::GET() %>%
    {jsonlite::fromJSON(rawToChar(.[["content"]]))} %>%
    {.[.$conceptdoi == dataIndex[shorthand, "doi"],]}



  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Download Data                                             #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  if (is.null(version)) {
    version = metadata$metadata$version[1]
    message("- ", crayon::green("[OK] "), "Retrieving latest version (",
            version, ")...")
  }

  data = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    dataIndex[shorthand, "repo"], "/", version, "/data.csv"
  ) %>% RCurl::getURL() %>%
    read.csv(text = ., sep = ";") %>%
    {.[,1] = NULL;.}

  # Parse numerics
  apply(data, 2, function(x){
    suppressWarnings({
      readr::parse_number(x, locale = readr::locale(decimal_mark = ","))
    }) -> col
    ifelse(is.null(attr(col, "problems")), return(col), return(x))
  }, simplify = FALSE) %>%
    as.data.frame() -> dataClean

  # study and .id are never parsed
  within(dataClean, {
    study = data$study
    .id = data$.id
  }) -> dataClean


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Set Attributes                                            #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  variableDescription = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    dataIndex[shorthand, "repo"], "/", version,
    "/metadata/variable_description.json"
  ) %>% RCurl::getURL() %>%
    jsonlite::fromJSON()

  lastSearch = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    dataIndex[shorthand, "repo"], "/", version,
    "/metadata/last_search.txt"
  ) %>% RCurl::getURL()

  attr(dataClean, "databaseDoi") = dataIndex[shorthand, "doi"]
  attr(dataClean, "versionDoi") = metadata$metadata %>%
    {.[.$version == version, "doi"]}
  attr(dataClean, "publicationDate") = metadata$metadata %>%
    {.[.$version == version, "publication_date"]}
  attr(dataClean, "variableDescription") = variableDescription
  attr(dataClean, "lastSearch") = lastSearch
  attr(dataClean, "documentation") = paste0("https://docs.metapsy.org/databases/",
                                            dataIndex[shorthand, "url"], "/")


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Return                                                    #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  message("- ", crayon::green("[OK] "), "Download successful!")
  message("- ", crayon::green("[OK] "), "Use ", crayon::magenta("openDocs()"),
          " to open the database documentation.")
  message("- ", crayon::green("[OK] "), "Use ",
          crayon::magenta("variableDescription()"),
          " to retrieve a variable description.")

  return(dataClean)

}







