#' Print size
#'
#' This function prints the \code{size} object
#'
#' @param x           \code{size} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{size.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @method print size
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#  # Two-sided test
#' # H0: rho = 0.3, H1: rho != 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' n <- size.cor(delta = 0.2, rho = 0.3, alpha = 0.05, beta = 0.2,
#'               output = FALSE)
#'
#' print(n)
print.size <- function(x, ...) {

  #-----------------------------------------------------------------------------------
  # Main function

  cat("\nSample size determination for testing the product-moment correlation coefficient\n\n",

      " optimal sample size: n =", ceiling(x$res$n), "\n\n")

  ###

  if (x$spec$alternative == "two.sided") {

    cat("  H0: rho =", x$spec$rho, " versus  H1: rho !=",  x$spec$rho, "\n")

  }

  if (x$spec$alternative == "less") {

    cat("  H0: rho >=", x$spec$rho, " versus  H1: rho <",  x$spec$rho, "\n")

  }

  if (x$spec$alternative == "greater") {

    cat("  H0: rho <=", x$spec$rho, " versus  H1: rho >",  x$spec$rho, "\n")

  }

  ###

  cat("  alpha:", x$spec$alpha, " beta:", x$spec$beta, " delta:", x$spec$delta, "\n\n")

}
