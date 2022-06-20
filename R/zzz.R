.onAttach <- function(libname, pkgname) {
  packageStartupMessage(crayon::green("\u2713"),
                        crayon::cyan(" Loading "),
                        crayon::cyan$bold("{metapsyData}"),
                        crayon::cyan(" 0.2.1 [BETA]. \n \u2192 For help, go to "),
                        crayon::green("data.metapsy.org"),
                        crayon::cyan(".\n \u2192 To access data, make",
                                     "sure your computer is connected",
                                     "to the Internet."),
                        crayon::cyan(".\n \u2192 Use the",
                                     crayon::cyan$bold("{metapsyTools}"),
                                     "package to run meta-analyses."))
}
