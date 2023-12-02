#' Import Metapsy databases into the R environment
#'
#' This function allows to download all publicly available versions of Metapsy
#' databases into your R environment.
#'
#' @usage getData(shorthand,
#'         version = NULL,
#'         include.metadata = TRUE)
#'
#' @param shorthand \code{character}. The shorthand assigned to the database that
#' should be downloaded. Shorthands are listed under "Metadata" in the
#' [database documentation entry](https://docs.metapsy.org/databases). Alternatively,
#' all available databases and their shortcodes can be accessed by running [listData()].
#' @param version \code{character}. The version number to be downloaded. Default is
#' \code{NULL}, which downloads the latest version.
#' @param include.metadata \code{logical}. If set to \code{TRUE}, the function will
#' return an [R6::R6Class()] object that includes metadata associated with the database
#' version. See "Value".
#'
#' @return If \code{include.metadata} is set to \code{TRUE}, the `getData` function
#' will return both the requested dataset itself, as well as the metadata associated
#' within in it. Metadata items included by default are:
#'
#' - `database.doi`: The digital object identifier of the database ("Concept DOI"
#' in the [Zenodo API](https://zenodo.org/). This DOI will always resolve to the
#' latest database version).
#' - `documentation.url`: URL of the database documentation entry on the
#' [Metapsy Documentation page](https://docs.metapsy.org/databases/).
#' - `github.repo.url`: URL of the specific Github repository state at which the
#' database was released.
#' - `last.search`: Date of the last search.
#' - `last.updated`: Date of the last database updated (release).
#' - `license`: License of the database.
#' - `repository.download.url`: Link to download the entire database from Zenodo.
#' - `title`: Title of the database (including its version).
#' - `variable.description`: Description of the variables included in the dataset.
#' - `version`: Version of the database.
#' - `version.doi`: DOI associated with the specific version of the database. In contrast to
#' the `database.doi`, this identifier will also link to this specific database version.
#'
#' The returned [R6::R6Class()] object also contains a few helpful functions,
#' which can be called directly from the object:
#'
#' - `downloadZip()`: This will download the database (including metadata) as a
#' .zip file from its Zenodo repository.
#' - `openDocumentation()`: This opens the database documentation entry on
#' the [Metapsy Documentation page](https://docs.metapsy.org/databases/).
#' - `openGitRepo()`: This opens the Github repository of the database at the state of
#' its release.
#' - `returnMetadata()`: Returns the entire metadata stored in the `metapsyDatabase`
#' R6 object as a `list`.
#' - `variableDescription()`: This prints a variable description of the database
#' in the R Console.
#'
#' If `include.metadata` is `FALSE`, the function will return the dataset
#' as a simple `data.frame`.
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
#' @seealso \code{\link{listData}}
#'
#'
#' @importFrom crayon green magenta
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom readr parse_number locale
#' @importFrom utils read.csv browseURL
#' @importFrom R6 R6Class
#'
#' @export getData


getData = function(shorthand,
                   version = NULL,
                   include.metadata = TRUE){


  if (missing(shorthand)){
    stop("No database shorthand provided.")
  }

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
    "Bhico7tuZ3iW6cp1hJ3m9FIY6HcvX&all_versions=1&size=10000"
  ) %>% httr::GET() %>%
    {jsonlite::fromJSON(rawToChar(.[["content"]]))} %>%
    {.[.$conceptdoi == dataIndex[shorthand, "doi"],]} %>%
    {.[!is.na(.$conceptdoi),]}


  # Print retrieved version
  if (is.null(version)) {
    version = metadata$metadata$version[1]
    message("- ", crayon::green("[OK] "), "Retrieving latest version (",
            version, ")...")
  } else {
    if (!version %in% metadata$metadata$version){
      stop("The specified database version was not found.")
    }
    message("- ", crayon::green("[OK] "), "Retrieving version ",
            version, "...")
  }

  if (isTRUE(include.metadata)){
    # Create R6 container
    metapsyDatabase =
      R6::R6Class(
        "metapsyDatabase",
      public =
        list(data = NULL,
             title = NULL, version = NULL, last.updated = NULL,
             last.search = NULL, database.doi = NULL,
             version.doi = NULL, repository.download.url = NULL,
             github.repo.url = NULL, documentation.url = NULL,
             license = NULL, variable.description = NULL,
             initialize =
               function(data = NA,
                        title = NA, version = NA, last.updated = NA,
                        last.search = NA, database.doi = NA,
                        version.doi = NA, repository.download.url = NA,
                        github.repo.url = NA, documentation.url = NA,
                        license = NA, variable.description = NA){
                 self$data = data;
                 self$title = title[1]; self$version = version[1];
                 self$last.updated = last.updated[1];
                 self$last.search = last.search;
                 self$database.doi = database.doi[1];
                 self$version.doi = version.doi[1];
                 self$repository.download.url = repository.download.url;
                 self$github.repo.url = github.repo.url;
                 self$documentation.url = documentation.url;
                 self$license = license[1];
                 self$variable.description = variable.description
               },
             downloadZip = function(){
               utils::browseURL(self$repository.download.url)
             },
             openDocumentation = function(){
               utils::browseURL(self$documentation.url)
             },
             openGitRepo = function(){
               utils::browseURL(self$github.repo.url)
             },
             variableDescription = function(){
               df = as.data.frame(
                 t(as.data.frame(self$variable.description)))
               df$variable = rownames(df)
               df$description = df[,1]
               rownames(df) = NULL
               df$V1 = NULL
               apply(df, 1, function(y){
                 message("- ", crayon::green(y["variable"]), ": ",
                         y["description"])
              }) -> null},
             returnMetadata = function(){
               with(as.list(self), {
                 list(database.doi = database.doi,
                      documentation.url = documentation.url,
                      github.repo.url = github.repo.url,
                      last.search = last.search,
                      last.updated = last.updated,
                      license = license,
                      repository.download.url = repository.download.url,
                      title = title,
                      variable.description = variable.description,
                      version = version,
                      version.doi = version.doi)
               })
               }))


    # Collect metadata
    metadata %>%
      {.[.$metadata$version == version,]} %>%
      with(.,{
        metapsyDatabase$new(
          NA, title, metadata$version, modified %>% as.Date(),
          paste0("https://raw.githubusercontent.com/metapsy-project/",
                 dataIndex[shorthand, "repo"], "/", version,
                 "/metadata/last_search.txt") %>% RCurl::getURL() %>% as.Date(),
          conceptdoi, doi, files[[1]]$links$download,
          metadata$related_identifiers[[1]]$identifier,
          paste0("https://docs.metapsy.org/databases/",
                 dataIndex[shorthand, "url"], "/"),
          metadata$license,
          paste0("https://raw.githubusercontent.com/metapsy-project/",
                 dataIndex[shorthand, "repo"], "/", version,
                 "/metadata/variable_description.json") %>%
            RCurl::getURL() %>%
            jsonlite::fromJSON())
      }) -> metapsyDatabaseObject
  }


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Download Data                                             #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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

  if (isTRUE(include.metadata)){
    metapsyDatabaseObject$data = dataClean
    return.obj = metapsyDatabaseObject
  } else {
    return.obj = dataClean
  }


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Return                                                    #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  message("- ", crayon::green("[OK] "), "Download successful!")
  return(return.obj)

}


#' Print `metapsyDatabase` objects
#'
#' Prints the dataset (`data`) if objects returned by [getData()] are of class
#' `metapsyDatabase` (i.e. if `include.metadata=TRUE`).
#'
#' @param x A database of class `metapsyDatabase` imported using [getData()].
#' @param first Number of first rows to be printed.
#' @param last Number of last rows to be printed.
#' @param ... Additional arguments.
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com},
#' Paula Kuper \email{paula.r.kuper@@gmail.com}, Pim Cuijpers \email{p.cuijpers@@vu.nl}
#'
#' @importFrom crayon green
#' @export
#' @method print metapsyDatabase

print.metapsyDatabase = function(x, first=NULL, last=NULL, ...){
  if (all(is.null(first),is.null(last))) {
    print(x$data)
  } else {
    if (!is.null(first)) {
      print(x$data[1:first,])
    }
    if (!is.null(last)) {
      print(x$data[(nrow(x$data)-last+1):nrow(x$data),])
    }
  }
}



