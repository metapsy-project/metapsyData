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
#' @details
#'
#' The function can work with or without a Zenodo API access token. While a token
#' is not required, setting one provides several benefits:
#'
#' \itemize{
#'   \item **Higher rate limits**: Authenticated requests can fetch up to 100 records
#'   per page, compared to 25 records per page for unauthenticated requests. This
#'   significantly speeds up data retrieval for databases with many versions.
#'   \item **Reduced rate limiting**: Authenticated requests are less likely to hit
#'   Zenodo's rate limits, especially when fetching multiple pages of results.
#'   \item **Automatic pagination**: The function automatically handles pagination to
#'   retrieve all versions, but this is faster with a token due to larger page sizes.
#' }
#'
#' If no token is set, the function will optionally prompt you to set one. You can
#' also set or update your token at any time using [updateZenodoAccessToken()].
#'
#' To create a Zenodo API access token:
#' \enumerate{
#'   \item Log into your Zenodo account.
#'   \item Visit [this page](https://zenodo.org/account/settings/applications/tokens/new/).
#'   \item Create a new personal access token.
#'   \item Copy the token and use [updateZenodoAccessToken()] to save it.
#' }
#'
#' The token will be saved to your `.Renviron` file for permanent storage across
#' R sessions. The function automatically validates tokens and will clear invalid
#' or expired tokens.
#'
#' @examples
#' \dontrun{
#' # List all available databases
#' listData()
#'
#' # Get latest version of the 'depression-psyctr' database
#' d <- getData("depression-psyctr")
#'
#' # Get version 24.0.2 of the 'depression-psyctr' database
#' d <- getData("depression-psyctr", "24.0.2")
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
#' @author Mathias Harrer \email{m.harrer@@vu.nl}
#'
#' @seealso \code{\link{listData}}, \code{\link{updateZenodoAccessToken}}
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

  # Helper function to save token to .Renviron file
  save_token_to_renviron = function(token) {
    renviron_path = file.path(Sys.getenv("HOME"), ".Renviron")

    # Read existing .Renviron file if it exists
    if (file.exists(renviron_path)) {
      renviron_content = readLines(renviron_path, warn = FALSE)
    } else {
      renviron_content = character(0)
    }

    # Check if ZENODO_ACCESS_TOKEN already exists
    token_line = paste0("ZENODO_ACCESS_TOKEN=", token)
    token_pattern = "^ZENODO_ACCESS_TOKEN="

    # Find existing token line
    token_index = grep(token_pattern, renviron_content)

    if (length(token_index) > 0) {
      # Update existing token
      renviron_content[token_index] = token_line
      message("- ", crayon::green("[OK] "), "Updated existing token in .Renviron file.")
    } else {
      # Append new token
      renviron_content = c(renviron_content, "", token_line)
      message("- ", crayon::green("[OK] "), "Added token to .Renviron file.")
    }

    # Write back to file
    tryCatch({
      writeLines(renviron_content, renviron_path)
      message("- ", crayon::green("[OK] "), "Token saved permanently to: ", renviron_path)
      message("  Note: Please restart R/RStudio for the changes to take effect.")
    }, error = function(e) {
      warning("Could not write to .Renviron file: ", e$message,
              "\nToken will only be available for this R session.")
    })
  }

  # Helper function to read token from .Renviron file
  read_token_from_renviron = function() {
    renviron_path = file.path(Sys.getenv("HOME"), ".Renviron")

    if (file.exists(renviron_path)) {
      renviron_content = readLines(renviron_path, warn = FALSE)
      token_line = grep("^ZENODO_ACCESS_TOKEN=", renviron_content, value = TRUE)

      if (length(token_line) > 0) {
        # Extract token value (handle cases with or without quotes)
        token = sub("^ZENODO_ACCESS_TOKEN=", "", token_line[1])
        token = gsub('^["\']|["\']$', "", token)  # Remove quotes if present
        token = trimws(token)
        return(token)
      }
    }
    return("")
  }

  # Helper function to validate Zenodo token
  validate_zenodo_token = function(token) {
    if (token == "") return(FALSE)

    # Make a simple test API call to check if token is valid
    # Try to use a larger page size - if token is valid, this should work
    # If invalid, we'll get an error about page size
    test_url = paste0(
      "https://zenodo.org/api/records?",
      "q=conceptdoi:10.5281/zenodo.1234567890",  # Dummy DOI for validation
      "&size=100&access_token=", token
    )

    test_response = tryCatch({
      httr::GET(test_url, httr::timeout(5))
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(test_response)) return(FALSE)

    status = httr::status_code(test_response)
    response_text = rawToChar(test_response[["content"]])

    # If we get 200, token is valid
    # If we get 400 with page size error, token is invalid
    # If we get 401 (unauthorized), token is invalid
    if (status == 200) return(TRUE)
    if (status == 401) return(FALSE)
    if (status == 400 && grepl("Page size cannot be greater than 25", response_text)) {
      return(FALSE)  # Token doesn't allow larger page size, so it's invalid
    }

    # For other status codes (like 404), assume token is valid (auth worked)
    return(TRUE)
  }

  # Get Zenodo access token from environment variable (if available)
  zenodo_token = Sys.getenv("ZENODO_ACCESS_TOKEN", unset = "")

  # If not in environment, try reading from .Renviron file
  if (zenodo_token == "") {
    zenodo_token = read_token_from_renviron()
    if (zenodo_token != "") {
      # Load it into current session
      Sys.setenv(ZENODO_ACCESS_TOKEN = zenodo_token)
      message("- ", crayon::green("[OK] "), "Token loaded from .Renviron file.")
    }
  }

  # Validate token if present
  if (zenodo_token != "") {
    message("- ", crayon::magenta("[INFO] "), "Validating Zenodo API access token...")
    if (!validate_zenodo_token(zenodo_token)) {
      message("- ", crayon::magenta("[WARNING] "), "Token appears to be invalid or expired. Clearing token.")
      zenodo_token = ""
      Sys.unsetenv("ZENODO_ACCESS_TOKEN")
      # Also remove from .Renviron if possible
      tryCatch({
        renviron_path = file.path(Sys.getenv("HOME"), ".Renviron")
        if (file.exists(renviron_path)) {
          renviron_content = readLines(renviron_path, warn = FALSE)
          renviron_content = renviron_content[!grepl("^ZENODO_ACCESS_TOKEN=", renviron_content)]
          writeLines(renviron_content, renviron_path)
        }
      }, error = function(e) {
        # Silently fail - user can manually remove it
      })
    } else {
      message("- ", crayon::green("[OK] "), "Token is valid.")
    }
  }

  # Optional token setup if not available
  if (zenodo_token == "") {
    message("\n", crayon::magenta("[INFO] "),
            "No Zenodo API Access Token is set.")

    # Ask if user wants to set a token to avoid rate limits
    user_response = readline(
      paste0("\nWould you like to set a Zenodo API Access Token to avoid rate limits? (Y/n): ")
    )

    if (tolower(trimws(user_response)) %in% c("y", "yes", "")) {
      zenodo_token_url = "https://zenodo.org/account/settings/applications/tokens/new/"

      # Ask if user wants to be directed to the URL
      user_response_url = readline(
        paste0("\nWould you like to be directed to the URL where you can create ",
               "a Zenodo API token? (Y/n): ")
      )

      if (tolower(trimws(user_response_url)) %in% c("y", "yes", "")) {
        message("- ", crayon::green("[OK] "), "Opening Zenodo token creation page...")
        utils::browseURL(zenodo_token_url)
      }

      # Ask for the API key
      message("\nPlease paste your Zenodo API Access Token below.")
      message("(You can find it at: ", zenodo_token_url, ")")
      message("(You can paste it with or without quotes)")
      zenodo_token = readline("Zenodo API Access Token: ")
      zenodo_token = trimws(zenodo_token)
      # Remove quotes if user accidentally included them
      zenodo_token = gsub('^["\']|["\']$', "", zenodo_token)

      if (zenodo_token == "") {
        message("- ", crayon::magenta("[INFO] "), "No token provided. Continuing without token (rate limits may apply).")
        zenodo_token = ""
      } else {
        # Save the token for the current session
        Sys.setenv(ZENODO_ACCESS_TOKEN = zenodo_token)
        message("- ", crayon::green("[OK] "), "Token saved for this R session.")

        # Save to .Renviron file for permanence
        save_token_to_renviron(zenodo_token)
      }
    } else {
      message("- ", crayon::magenta("[INFO] "), "Continuing without token. Rate limits may apply.")
    }
  }

  # Use the public records API instead of deposit API
  # The deposit API only returns deposits owned by the authenticated user
  # The records API can access public records by conceptdoi
  conceptdoi = dataIndex[shorthand, "doi"]

  # Determine page size based on authentication
  # Unauthenticated requests are limited to 25, authenticated can use up to 100
  # This will be adjusted in fetch_page if token is invalid
  page_size = if (zenodo_token != "") 100 else 25

  # Function to fetch a single page of results
  fetch_page = function(page = 1, use_token = TRUE) {
    # Build API URL - use records API with conceptdoi query
    # This works for public records without requiring ownership
    current_page_size = if (use_token && zenodo_token != "") 100 else 25
    api_url = paste0(
      "https://zenodo.org/api/records?",
      "q=conceptdoi:", conceptdoi,
      "&all_versions=1&size=", current_page_size,
      "&page=", page
    )

    # Add token if available and requested
    if (use_token && zenodo_token != "") {
      api_url = paste0(api_url, "&access_token=", zenodo_token)
    }

    # Make API request
    response = httr::GET(api_url)
    status = httr::status_code(response)
    response_text = rawToChar(response[["content"]])

    # Check if request was successful
    if (status != 200) {
      # If we get a 400 error about page size with token, token might be invalid
      if (status == 400 && use_token && zenodo_token != "" &&
          grepl("Page size cannot be greater than 25", response_text)) {
        message("- ", crayon::magenta("[WARNING] "),
                "Token appears invalid (page size error). Retrying without token...")
        # Retry without token
        return(fetch_page(page, use_token = FALSE))
      }
      stop(
        "Failed to retrieve metadata from Zenodo API. ",
        "HTTP Status: ", status, "\n",
        "Response: ", response_text
      )
    }

    # Parse response - records API returns different structure
    response_data = jsonlite::fromJSON(response_text)

    return(response_data)
  }

  # Fetch first page
  if (zenodo_token != "") {
    message("- ", crayon::green("[OK] "), "Using Zenodo API access token.")
  }

  response_data = fetch_page(1)

  # Determine actual page size being used (might be 25 if token was invalid)
  # Check by looking at the number of records returned
  if ("hits" %in% names(response_data) && "hits" %in% names(response_data$hits)) {
    actual_page_size = length(response_data$hits$hits)
    # If we got 25 records and have a token, token might have been invalidated
    if (actual_page_size == 25 && zenodo_token != "" && page_size == 100) {
      message("- ", crayon::magenta("[INFO] "),
              "Using smaller page size (25). Token may not be valid for this request.")
      page_size = 25
      zenodo_token = ""  # Don't use token for subsequent requests
    }
  } else {
    actual_page_size = length(response_data)
  }

  # Check pagination info
  total_records = if ("hits" %in% names(response_data) && "total" %in% names(response_data$hits)) {
    response_data$hits$total
  } else if ("hits" %in% names(response_data) && "hits" %in% names(response_data$hits)) {
    length(response_data$hits$hits)
  } else {
    length(response_data)
  }

  # Records API returns a list with 'hits' containing the records
  if ("hits" %in% names(response_data) && "hits" %in% names(response_data$hits)) {
    all_records = response_data$hits$hits
  } else {
    # Fallback: try direct access if structure is different
    all_records = response_data
  }

  # Fetch additional pages if needed
  if (total_records > page_size) {
    total_pages = ceiling(total_records / page_size)
    if (total_pages > 1) {
      message("- ", crayon::green("[OK] "), "Found ", total_records, " versions. Fetching all pages (", total_pages, " pages)...")
      for (page in 2:total_pages) {
        # Use token only if it's still valid
        page_data = fetch_page(page, use_token = (zenodo_token != ""))
        page_records = if ("hits" %in% names(page_data) && "hits" %in% names(page_data$hits)) {
          page_data$hits$hits
        } else {
          page_data
        }
        # Append records to the list
        if (is.list(all_records) && is.list(page_records)) {
          all_records = append(all_records, page_records)
        } else {
          all_records = c(all_records, page_records)
        }
      }
      message("- ", crayon::green("[OK] "), "Retrieved all ", length(all_records), " versions.")
    }
  }

  records = all_records

  # Convert records API structure to match deposit API structure for compatibility
  # Records API has: records$metadata$version, records$metadata$related_identifiers, etc.
  # We need to normalize this to work with existing code
  if (length(records) > 0 && is.data.frame(records)) {
    # If it's already a data frame, check structure
    if ("conceptdoi" %in% colnames(records)) {
      metadata = records[records$conceptdoi == conceptdoi, ]
    } else {
      metadata = records
    }
  } else if (length(records) > 0) {
    # Convert list of records to data frame-like structure
    # Extract key fields from records API structure
    metadata_list = lapply(records, function(rec) {
      # Helper to safely get nested values
      get_val = function(x, default = NA) {
        if (is.null(x)) default else x
      }

      list(
        conceptdoi = get_val(rec$conceptdoi, get_val(rec$metadata$conceptdoi)),
        doi = get_val(rec$doi, get_val(rec$metadata$doi)),
        modified = get_val(rec$updated, get_val(rec$modified)),
        metadata = list(
          version = get_val(rec$metadata$version),
          title = get_val(rec$metadata$title),
          license = get_val(rec$metadata$license$id, get_val(rec$metadata$license)),
          related_identifiers = get_val(rec$metadata$related_identifiers, list(list(identifier = NA)))
        ),
        files = list(list(links = list(
          download = if (!is.null(rec$files) && length(rec$files) > 0) {
            get_val(rec$files[[1]]$links$download)
          } else {
            NA
          }
        )))
      )
    })

    # Convert to data frame (simplified structure)
    metadata = do.call(rbind, lapply(metadata_list, function(x) {
      data.frame(
        conceptdoi = x$conceptdoi,
        doi = x$doi,
        modified = x$modified,
        version = x$metadata$version,
        title = x$metadata$title,
        license = x$metadata$license,
        stringsAsFactors = FALSE
      )
    }))

    # Add nested structures as lists
    metadata$metadata = lapply(metadata_list, function(x) x$metadata)
    metadata$files = lapply(metadata_list, function(x) x$files)

    # Filter by conceptdoi
    metadata = metadata[metadata$conceptdoi == conceptdoi, ]
  } else {
    metadata = data.frame()
  }

  # Check if metadata was found
  if (nrow(metadata) == 0) {
    stop("No metadata found for shorthand '", shorthand,
         "'. Please check that the shorthand is correct.")
  }

  # Helper function to extract version from metadata (handles both structures)
  get_version = function(meta_row) {
    if (is.list(meta_row$metadata)) {
      meta_row$metadata$version
    } else if ("version" %in% names(meta_row)) {
      meta_row$version
    } else {
      NA
    }
  }

  # Print retrieved version
  if (is.null(version)) {
    # Get version from metadata, handling potential NA values
    if (nrow(metadata) > 0) {
      versions = sapply(1:nrow(metadata), function(i) get_version(metadata[i, ]))
      versions = versions[!is.na(versions)]
      
      # Sort versions to get the highest (latest) version
      # Convert version strings to numeric by removing dots and comparing
      if (length(versions) > 0) {
        # Helper function to convert version string to numeric for comparison
        # Split by dots, pad each part to fixed width, then combine
        # e.g., "24.0.2" -> "002400000002", "24.0.0" -> "002400000000"
        version_to_numeric = function(v) {
          v_parts = strsplit(as.character(v), "\\.")[[1]]
          # Pad each part to 4 digits to handle versions like "24.0.10" vs "24.0.2"
          v_padded = paste(sprintf("%04d", suppressWarnings(as.numeric(v_parts))), collapse = "")
          as.numeric(v_padded)
        }
        
        # Calculate numeric values for all versions
        version_nums = sapply(versions, version_to_numeric)
        # Sort by numeric value in descending order (highest first)
        version_order = order(version_nums, decreasing = TRUE)
        version = versions[version_order[1]]
      } else {
        version = NA
      }
    } else {
      version = NA
    }

    if (is.na(version) || is.null(version)) {
      stop("Could not determine database version. The metadata may be incomplete.")
    }
    message("- ", crayon::green("[OK] "), "Retrieving latest version (",
            version, ")...")
  } else {
    if (nrow(metadata) > 0) {
      versions = sapply(1:nrow(metadata), function(i) get_version(metadata[i, ]))
      all_versions = versions[!is.na(versions)]
    } else {
      all_versions = character(0)
    }

    if (!version %in% all_versions){
      stop("The specified database version '", version, "' was not found. ",
           "Available versions: ", paste(all_versions, collapse = ", "))
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


    # Helper function to safely parse dates
    safe_as_date = function(x) {
      # Check for NULL, NA, or empty string more safely
      if (is.null(x)) {
        return(as.Date(NA))
      }
      if (length(x) == 0 || (is.character(x) && trimws(x) == "")) {
        return(as.Date(NA))
      }
      # Check for NA values (handles both logical and other types)
      if (any(is.na(x))) {
        return(as.Date(NA))
      }

      # Convert to character if not already
      if (!is.character(x)) {
        x = as.character(x)
      }

      # Try parsing with different formats
      # First, try standard ISO format (YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS)
      result = tryCatch({
        # Remove timezone info if present (e.g., "Z" or "+00:00")
        x_clean = gsub("T.*$", "", gsub("\\+.*$", "", gsub("Z$", "", x)))
        x_clean = trimws(x_clean)
        if (x_clean == "") {
          return(as.Date(NA))
        }
        as.Date(x_clean)
      }, error = function(e) {
        # Try with format specification
        tryCatch({
          as.Date(x, format = "%Y-%m-%d")
        }, error = function(e2) {
          # Try other common formats
          tryCatch({
            as.Date(x, format = "%Y/%m/%d")
          }, error = function(e3) {
            # If all else fails, return NA
            warning("Could not parse date: ", x, ". Returning NA.")
            as.Date(NA)
          })
        })
      })
      return(result)
    }

    # Collect metadata
    metadata %>%
      {.[.$metadata$version == version,]} %>%
      with(.,{
        # Parse last_search date from GitHub
        last_search_url = paste0("https://raw.githubusercontent.com/metapsy-project/",
                                 dataIndex[shorthand, "repo"], "/", version,
                                 "/metadata/last_search.txt")
        last_search_raw = tryCatch({
          RCurl::getURL(last_search_url) %>% trimws()
        }, error = function(e) {
          warning("Could not retrieve last_search date from: ", last_search_url)
          return("")
        })
        last_search_date = safe_as_date(last_search_raw)

        metapsyDatabase$new(
          NA, title, metadata$version, safe_as_date(modified),
          last_search_date,
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

#' Update Zenodo API Access Token
#'
#' This function allows you to update or set your Zenodo API access token.
#' The token will be saved to your `.Renviron` file for permanent storage
#' and will also be set for the current R session.
#'
#' @usage updateZenodoAccessToken(token = NULL)
#'
#' @param token \code{character}. The Zenodo API access token. If \code{NULL}
#' (default), the function will interactively prompt you to enter the token.
#' You can create a token [here](https://zenodo.org/account/settings/applications/tokens/new/).
#'
#' @details
#' The function performs the following actions:
#' \itemize{
#'   \item If \code{token} is \code{NULL}, prompts you to enter the token
#'   \item Strips any quotes from the token if accidentally included
#'   \item Saves the token to your `.Renviron` file (located at `~/.Renviron`)
#'   \item Updates the token in your current R session
#'   \item If a token already exists in `.Renviron`, it will be updated
#' }
#'
#' After updating the token, you may need to restart R/RStudio for the changes
#' to take full effect, though the token will be available in the current
#' session immediately.
#'
#' @return Invisibly returns \code{TRUE} if successful, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' # Update token interactively
#' updateZenodoAccessToken()
#'
#' # Update token directly
#' updateZenodoAccessToken("your_token_here")
#' }
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com}
#'
#' @seealso \code{\link{getData}}
#'
#' @importFrom crayon green magenta
#' @importFrom utils browseURL
#'
#' @export updateZenodoAccessToken
updateZenodoAccessToken = function(token = NULL) {

  zenodo_token_url = "https://zenodo.org/account/settings/applications/tokens/new/"

  # If token not provided, ask for it
  if (is.null(token)) {
    message("\n", crayon::magenta("[INFO] "),
            "Updating Zenodo API Access Token.")

    # Ask if user wants to be directed to the URL
    user_response = readline(
      paste0("\nWould you like to be directed to the URL where you can create ",
             "a Zenodo API token? (Y/n): ")
    )

    if (tolower(trimws(user_response)) %in% c("y", "yes", "")) {
      message("- ", crayon::green("[OK] "), "Opening Zenodo token creation page...")
      utils::browseURL(zenodo_token_url)
    }

    # Ask for the API key
    message("\nPlease paste your Zenodo API Access Token below.")
    message("(You can find it at: ", zenodo_token_url, ")")
    message("(You can paste it with or without quotes)")
    token = readline("Zenodo API Access Token: ")
  }

  # Clean the token
  token = trimws(token)
  # Remove quotes if user accidentally included them
  token = gsub('^["\']|["\']$', "", token)

  if (token == "") {
    stop("No token provided. Please provide a valid Zenodo API Access Token.")
  }

  # Helper function to save token to .Renviron file
  save_token_to_renviron = function(token) {
    renviron_path = file.path(Sys.getenv("HOME"), ".Renviron")

    # Read existing .Renviron file if it exists
    if (file.exists(renviron_path)) {
      renviron_content = readLines(renviron_path, warn = FALSE)
    } else {
      renviron_content = character(0)
    }

    # Check if ZENODO_ACCESS_TOKEN already exists
    token_line = paste0("ZENODO_ACCESS_TOKEN=", token)
    token_pattern = "^ZENODO_ACCESS_TOKEN="

    # Find existing token line
    token_index = grep(token_pattern, renviron_content)

    if (length(token_index) > 0) {
      # Update existing token
      renviron_content[token_index] = token_line
      message("- ", crayon::green("[OK] "), "Updated existing token in .Renviron file.")
    } else {
      # Append new token
      renviron_content = c(renviron_content, "", token_line)
      message("- ", crayon::green("[OK] "), "Added token to .Renviron file.")
    }

    # Write back to file
    tryCatch({
      writeLines(renviron_content, renviron_path)
      message("- ", crayon::green("[OK] "), "Token saved permanently to: ", renviron_path)
      message("  Note: Please restart R/RStudio for the changes to take effect.")
      return(TRUE)
    }, error = function(e) {
      warning("Could not write to .Renviron file: ", e$message,
              "\nToken will only be available for this R session.")
      return(FALSE)
    })
  }

  # Save the token for the current session
  Sys.setenv(ZENODO_ACCESS_TOKEN = token)
  message("- ", crayon::green("[OK] "), "Token saved for this R session.")

  # Save to .Renviron file for permanence
  success = save_token_to_renviron(token)

  message("\n", crayon::green("[OK] "), "Zenodo API Access Token updated successfully!")

  return(invisible(success))
}

