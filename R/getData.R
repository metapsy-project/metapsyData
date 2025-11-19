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
#' @importFrom utils read.csv browseURL URLencode
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

  target.doi = dataIndex[shorthand, "doi"]
  all.records = list()
  page = 1
  size = 99
  
  repeat {
    query = paste0("conceptdoi:\"", target.doi, "\"")
    url = paste0(
      "https://zenodo.org/api/records?",
      "q=", utils::URLencode(query, reserved = TRUE),
      "&size=", size,
      "&page=", page,
      "&all_versions=true"
    )
    
    response = httr::GET(url)
    page.data = jsonlite::fromJSON(rawToChar(response[["content"]]))
    
    if (is.null(page.data$hits) || length(page.data$hits$hits) == 0) {
      break
    }
    
    records = page.data$hits$hits
    all.records = c(all.records, records)
    
    if (length(records) < size) {
      break
    }
    page = page + 1
  }
  
  if (length(all.records) == 0) {
    stop("No metadata found for shorthand '", shorthand, "' with DOI '", target.doi, "'.")
  }
  
  n.records = length(all.records$conceptdoi)
  
  metadata = data.frame(
    conceptdoi = all.records$conceptdoi,
    doi = all.records$doi,
    modified = all.records$updated,
    created = all.records$created,
    id = all.records$id,
    conceptrecid = all.records$conceptrecid,
    doi_url = all.records$doi_url,
    title = all.records$title,
    stringsAsFactors = FALSE
  )
  
  metadata$metadata = lapply(1:n.records, function(i) {
    meta.df = all.records$metadata
    list(
      title = if (i <= nrow(meta.df) && !is.null(meta.df$title[i])) meta.df$title[i] else NA_character_,
      doi = if (i <= nrow(meta.df) && !is.null(meta.df$doi[i])) meta.df$doi[i] else NA_character_,
      publication_date = if (i <= nrow(meta.df) && !is.null(meta.df$publication_date[i])) meta.df$publication_date[i] else NA_character_,
      description = if (i <= nrow(meta.df) && !is.null(meta.df$description[i])) meta.df$description[i] else NA_character_,
      access_right = if (i <= nrow(meta.df) && !is.null(meta.df$access_right[i])) meta.df$access_right[i] else NA_character_,
      creators = if (i <= nrow(meta.df) && !is.null(meta.df$creators[i])) meta.df$creators[i] else NA,
      related_identifiers = if (i <= nrow(meta.df) && !is.null(meta.df$related_identifiers[i])) {
        if (is.data.frame(meta.df$related_identifiers[i])) {
          as.list(meta.df$related_identifiers[i, , drop = FALSE])
        } else {
          meta.df$related_identifiers[[i]]
        }
      } else {
        list(list(identifier = NA_character_))
      },
      version = if (i <= nrow(meta.df) && !is.null(meta.df$version[i])) meta.df$version[i] else NA_character_,
      license = if (i <= nrow(meta.df) && "id" %in% colnames(meta.df) && !is.null(meta.df$id[i])) meta.df$id[i] else NA_character_,
      upload_type = if (i <= nrow(meta.df) && "resource_type.type" %in% colnames(meta.df) && !is.null(meta.df$`resource_type.type`[i])) meta.df$`resource_type.type`[i] else NA_character_
    )
  })
  
  metadata$files = lapply(1:n.records, function(i) {
    if (!is.null(all.records$files) && i <= length(all.records$files) && !is.null(all.records$files[[i]])) {
      file.df = all.records$files[[i]]
      if (is.data.frame(file.df) && nrow(file.df) > 0) {
        lapply(1:nrow(file.df), function(j) {
          list(
            links = list(
              download = if (!is.null(file.df$self[j])) file.df$self[j] else NA_character_,
              self = if (!is.null(file.df$self[j])) file.df$self[j] else NA_character_
            ),
            key = if (!is.null(file.df$key[j])) file.df$key[j] else NA_character_,
            size = if (!is.null(file.df$size[j])) file.df$size[j] else NA_integer_,
            checksum = if (!is.null(file.df$checksum[j])) file.df$checksum[j] else NA_character_
          )
        })
      } else {
        list()
      }
    } else {
      list()
    }
  })
  
  if (!is.null(all.records$links)) {
    metadata$links = lapply(1:n.records, function(i) {
      if (is.data.frame(all.records$links) && i <= nrow(all.records$links)) {
        as.list(all.records$links[i, , drop = FALSE])
      } else {
        list()
      }
    })
  } else {
    metadata$links = lapply(1:n.records, function(i) list())
  }
  
  if (!is.null(all.records$state)) {
    metadata$state = all.records$state
  }
  if (!is.null(all.records$submitted)) {
    metadata$submitted = all.records$submitted
  }
  
  metadata = metadata[metadata$conceptdoi == target.doi & !is.na(metadata$conceptdoi), ]


  versions = sapply(metadata$metadata, function(x) x$version)
  
  if (is.null(version)) {
    version.nums = lapply(versions, function(v) {
      as.numeric(strsplit(v, "\\.")[[1]])
    })
    max.len = max(sapply(version.nums, length))
    version.nums = lapply(version.nums, function(vn) {
      c(vn, rep(0, max.len - length(vn)))
    })
    max.version.idx = which.max(sapply(version.nums, function(vn) {
      sum(vn * 10^((length(vn):1) - 1))
    }))
    version = versions[max.version.idx]
    message("- ", crayon::green("[OK] "), "Retrieving latest version (",
            version, ")...")
  } else {
    if (!version %in% versions){
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


    version.match = sapply(metadata$metadata, function(x) x$version) == version
    metadata.filtered = metadata[version.match, ]
    
    if (nrow(metadata.filtered) == 0) {
      stop("Version '", version, "' not found in metadata.")
    }
    
    row = metadata.filtered[1, ]
    row.metadata = row$metadata[[1]]
    row.files = row$files[[1]]
    
    download.url = if (length(row.files) > 0 && !is.null(row.files[[1]]$links$download)) {
      row.files[[1]]$links$download
    } else if (length(row.files) > 0 && !is.null(row.files[[1]]$links$self)) {
      row.files[[1]]$links$self
    } else {
      NA_character_
    }
    
    related.id = if (!is.null(row.metadata$related_identifiers) && length(row.metadata$related_identifiers) > 0) {
      if (is.list(row.metadata$related_identifiers[[1]]) && !is.null(row.metadata$related_identifiers[[1]]$identifier)) {
        row.metadata$related_identifiers[[1]]$identifier
      } else if (is.character(row.metadata$related_identifiers[[1]])) {
        row.metadata$related_identifiers[[1]]
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }
    
    metapsyDatabase$new(
      NA, 
      row$title, 
      row.metadata$version, 
      row$modified %>% as.Date(),
      paste0("https://raw.githubusercontent.com/metapsy-project/",
             dataIndex[shorthand, "repo"], "/", version,
             "/metadata/last_search.txt") %>% RCurl::getURL() %>% as.Date(),
      row$conceptdoi, 
      row$doi, 
      download.url,
      related.id,
      paste0("https://docs.metapsy.org/databases/",
             dataIndex[shorthand, "url"], "/"),
      row.metadata$license,
      paste0("https://raw.githubusercontent.com/metapsy-project/",
             dataIndex[shorthand, "repo"], "/", version,
             "/metadata/variable_description.json") %>%
        RCurl::getURL() %>%
        jsonlite::fromJSON()
    ) -> metapsyDatabaseObject
  }


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #                                                             #
  #   Download Data                                             #
  #                                                             #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  data = paste0(
    "https://raw.githubusercontent.com/metapsy-project/",
    dataIndex[shorthand, "repo"], "/", version, "/data.csv"
  ) %>% RCurl::getURL(.encoding = "UTF-8") %>%
    read.csv(text = ., sep = ";") %>%
    {.[,"X"] = NULL;.}

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

  # Define other variables to never be parsed
  if ("multi_arm1" %in% colnames(dataClean)){
    dataClean$multi_arm1 = data$multi_arm1 }
  if ("multi_arm2" %in% colnames(dataClean)){
    dataClean$multi_arm2 = data$multi_arm2 }
  if ("dose_arm1" %in% colnames(dataClean)){
    dataClean$dose_arm1 = data$dose_arm1 }
  if ("dose_arm2" %in% colnames(dataClean)){
    dataClean$dose_arm2 = data$dose_arm2 }
  if ("study_time_point" %in% colnames(dataClean)){
    dataClean$study_time_point = data$study_time_point }
  if ("doi" %in% colnames(dataClean)){
    dataClean$doi = data$doi }
  if ("registration_number" %in% colnames(dataClean)){
    dataClean$registration_number = data$registration_number }

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



