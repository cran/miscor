#' Print cor.rhotest
#'
#' This function prints the \code{test.cor} object
#'
#' @param x           \code{test.cor} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Kubinger, K. D., Rasch, D., & Simeckova, M. (2007). Testing a correlation coefficient's significance:
#' Using H0: 0 \eqn{< \rho \le \lambda} is preferable to H0: \eqn{\rho = }0. \emph{Psychology Science, 49}, 74-87.
#'
#' @method print test.cor
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Two-sided test
#' # H0: rho == 0, H1: rho != 0
#' # r = 0.23, n = 60
#'
#' obj <- test.cor(r = 0.23, n = 120, output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Two-sided test
#' # H0: rho == 0.4, H1: rho != 0.4
#' # r = 0.55, n = 120
#'
#' obj <- test.cor(r = 0.55, n = 120, rho0 = 0.4,
#'                 output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # One-sided test
#' # H0: rho <= 0.4, H1: rho > 0.4
#'
#' # Generate random data
#' dat <- sim.cor(100, rho = 0.4)
#'
#' obj <- test.cor(dat$x, dat$y, rho0 = 0.4, output = FALSE)
#' print(obj)
print.test.cor <- function(x, ...) {

  #-----------------------------------------------------------------------------------

  cat("\nStatistical test for the product-moment correlation coefficient\n\n")

  ###

  if (x$spec$alternative == "two.sided") {

    cat("  H0: rho ==", x$spec$rho0, " versus  H1: rho !=", x$spec$rho0, "\n")

  }

  if (x$spec$alternative == "greater") {

      cat("  H0: rho <=", x$spec$rho0, " versus  H1: rho >", x$spec$rho0, "\n")

  }

  if (x$spec$alternative == "less") {

    cat("  H0: rho >=", x$spec$rho0, " versus  H1: rho <", x$spec$rho0, "\n")

  }

  ###

  if (x$spec$rho0 == 0) {

    cat(paste0("\n  t = ", formatC(x$res$t, digits = x$spec$digits, format = "f"), ", df = ", x$res$df,
               ", p-value = ", formatC(x$res$pval, digits = 4, format = "f")))

  } else {

    cat(paste0("\n  z = ", formatC(x$res$z, digits = x$spec$digits, format = "f"),
               ", p-value = ", formatC(x$res$pval, digits = 4, format = "f")))

  }

  ###

  cat(ifelse(x$res$lower < 0, "\n\n  Sample estimate r:     ", "\n\n  Sample estimate r:    "),
      formatC(x$res$r, digits = x$spec$digits, format = "f"), "\n",
      paste0(ifelse(x$spec$alternative == "two.sided", "  Two-sided ", "  One-sided "), x$spec$conf.level * 100, "% CI: [",
             formatC(x$res$lower, digits = x$spec$digits, format = "f"), ", ",
             formatC(x$res$upper, digits = x$spec$digits, format = "f"), "]\n\n"))

}
