.onAttach <- function(libname, pkgname) {
  packageStartupMessage(crayon::green("\u2713"),
                        crayon::cyan(" Loading "),
                        crayon::cyan$bold("{metapsyData}"),
                        crayon::cyan(" 0.1.0 [BETA]. \n \u2192 For help, go to "),
                        crayon::green("metapsy.org/r-package"))
}