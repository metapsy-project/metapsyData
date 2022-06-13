#' Open database documentation
#'
#' This function opens the online documentation of an imported database.
#'
#' @usage openDocs(data)
#'
#' @param data A database \code{data.frame} imported using [getData()].
#'
#' @return Opens the database documentation entry on
#' [docs.metapsy.org](https://docs.metapsy.org/databases/).
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
#' openDocs(d)
#' }
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com}
#'
#' @seealso \code{\link{listData}}, \code{\link{getData}},
#' \code{\link{variableDescription}},
#'
#' @importFrom crayon green
#' @importFrom utils browseURL
#'
#' @export openDocs

openDocs = function(data){
  url = attr(data, "documentation")
  if (is.null(url)){
    stop("Could not retrieve documentation entry URL. ",
         "Have you created the data frame using getData()?")}
  message("- ", crayon::green("[OK] "), "Opening ", url)
  utils::browseURL(url)
}
