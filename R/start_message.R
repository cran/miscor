#' @importFrom utils packageDescription
#'
.onAttach <- function(libname, pkgname){


  desc <- packageDescription("miscor")
  d1 <- desc$Version
  nk <- paste0(rep(" ", 20 - nchar(d1)))

  packageStartupMessage("|---------------------------------------------------------|\n",
                          paste0("| ", desc$Package, " ", d1," (",desc$Date,")"), nk, "                |\n" ,
                        "| Miscellaneous Functions for the Correlation Coefficient |\n" ,
                        "|---------------------------------------------------------|" )

}

version <- function(pkg = "miscor") {

  lib <- dirname(system.file(package = pkg))
  desc <- packageDescription(pkg)

  return(paste(desc$Package, desc$Version, desc$Date,lib))

}
