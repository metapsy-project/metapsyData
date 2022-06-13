#' Show variable description for Metapsy database
#'
#' This function prints the variable description included in the metadata
#' of an imported Metapsy database.
#'
#' @usage variableDescription(data)
#'
#' @param data A database \code{data.frame} imported using [getData()].
#'
#' @return Returns a \code{data.frame} with two columns:
#' * \code{variable}. The variable name.
#' * \code{description}. The variable description,
#' including factor level explanations.
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
#' \code{\link{openDocs}}
#'
#' @importFrom crayon green
#'
#' @export variableDescription

variableDescription = function(data){
  varDesc = attr(data, "variableDescription")
  if (is.null(varDesc)){
    stop("Could not retrieve variable description. ",
         "Have you created the data frame using getData()?")}
  df = as.data.frame(t(as.data.frame(varDesc)))
  df$variable = rownames(df)
  df$description = df[,1]
  rownames(df) = NULL
  df$V1 = NULL
  class(df) = c("variableDescription", "data.frame")
  return(df)
}

#' Print variable description
#'
#' Prints the variable description included in the metadata of an imported
#' Metapsy database.
#'
#' @param x A database \code{data.frame} imported using [getData()].
#' @param ... Additional arguments.
#'
#' @author Mathias Harrer \email{mathias.h.harrer@@gmail.com},
#' Paula Kuper \email{paula.r.kuper@@gmail.com}, Pim Cuijpers \email{p.cuijpers@@vu.nl}
#'
#' @importFrom crayon green
#' @export
#' @method print variableDescription

print.variableDescription = function(x, ...){
  apply(x, 1, function(y){
    message("- ", crayon::green(y["variable"]), ": ",
            y["description"])
  }) -> null
}

