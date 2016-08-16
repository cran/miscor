#' Print comptest.cor
#'
#' This function prints the \code{comptest.cor} object
#'
#' @param x           \code{comptest.cor} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{comptest.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlation. \emph{Psychological Methods, 12}, 399-413.
#'
#' @method print comptest.cor
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Dependent samples
#'
#' # Generate random data
#' x <- c(3, 2, 2, 3, 7, 8, 5, 9)
#' y <- c(2, 4, 1, 5, 7, 3, 6, 7)
#' z <- c(1, 4, 3, 3, 1, 4, 2, 5)
#'
#' #............................................
#' # H0: rho.xy == rho.xz,  H1: rho.xy != rho.xz
#'
#' obj <- comptest.cor(x, y, z, output = FALSE)
#' print(obj)
#'
#' #...........................................
#' # H0: rho.xy <= rho.xz,  H1: rho.xy > rho.xz
#' # r.xy = 0.44, r.xz = 0.21. r.yz = 0.20, n = 120
#'
#' obj <- comptest.cor(r.xy = 0.44, r.xz = 0.21, r.yz = 0.20, n = 120,
#'                     alternative = "greater", output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Independent samples
#'
#' # Generate random data
#' dat <- data.frame(group = rep(1:2, each = 200),
#'                   rbind(sim.cor(200, rho = 0.3),
#'                         sim.cor(200, rho = 0.5)))
#'
#' #.......................................
#' # H0: rho.1 == rho.2, H1: rho.1 != rho.2
#'
#' obj <- comptest.cor(x = dat$x, y = dat$y, group = dat$group,
#'                     output = FALSE)
#' print(obj)
#'
#' #........................................
#' # H0: rho.1 >= rho.2, H1: rho.1 ! < rho.2
#' # Group 1: r = 0.32, n = 108
#' # Group 2: r = 0.56, n = 113
#'
#' obj <- comptest.cor(r.1 = 0.32, n.1 = 108, r.2 = 0.56, n.2 = 113,
#'                     alternative = "less", output = FALSE)
#' print(obj)
print.comptest.cor <- function(x, ...) {

  #-----------------------------------------------------------------------------------
  # Main function

  # Dependent samples
  if (x$spec$type == "dependent") {

    cat("\nComparison of the product-moment correlation coefficient from dependent samples\n\n")

    if (x$spec$alternative == "two.sided") {

      cat("  H0: rho.xy == rho.xz  versus  H1: rho.xy != rho.xz\n")

    }

    if (x$spec$alternative == "greater") {

      cat("  H0: rho.xy <= rho.xz  versus  H1: rho.xy > rho.xz\n")

    }

    if (x$spec$alternative == "less") {

      cat("  H0: rho.xy >= rho.xz  versus  H1: rho.xy < rho.xz\n")

    }

    cat(paste0("\n  z = ", formatC(x$res$z, digits = x$spec$digits, format = "f"),
               ", p-value = ", formatC(x$res$pval, digits = x$spec$digits, format = "f")), "\n\n")

    cat(paste0("  Sample estimate r.xy: ", formatC(x$res$r.xy, digits = x$spec$digits, format = "f"), "\n",
               "                  r.xz: ", formatC(x$res$r.xz, digits = x$spec$digits, format = "f"), "\n\n"))

    cat(paste0("  Two-sided ", x$spec$conf.level * 100, "% CI for r.xy - r.xz: [",
               formatC(x$res$lower, digits = x$spec$digits, format = "f"), ", ", formatC(x$res$upper, digits = x$spec$digits, format = "f"), "]\n\n"))

  # Independent samples
  } else {

    cat("\nComparison of product-moment correlation coefficients from independent samples\n\n")

    if (x$spec$alternative == "two.sided") {

      cat("  H0: rho.1 == rho.2  versus  H1: rho.1 != rho.2\n")

    }

    if (x$spec$alternative == "greater") {

      cat("  H0: rho.1 <= rho.2  versus  H1: rho.1 > rho.2\n")

    }

    if (x$spec$alternative == "less") {

      cat("  H0: rho.1 >= rho.2  versus  H1: rho.1 < rho.2\n")

    }

    cat(paste0("\n  z = ", formatC(x$res$z, digits = x$spec$digits, format = "f"),
               ", p-value = ", formatC(x$res$pval, digits = 4, format = "f")), "\n\n")

    cat(paste0("  Sample estimate r.1: ", formatC(x$res$r.1, digits = x$spec$digits, format = "f"), "\n",
               "                  r.2: ", formatC(x$res$r.2, digits = x$spec$digits, format = "f"), "\n\n"))

    cat(paste0("  Two-sided ", x$spec$conf.level * 100, "% CI for r.1 - r.2: [",
               formatC(x$res$lower, digits = x$spec$digits, format = "f"), ", ", formatC(x$res$upper, digits = x$spec$digits, format = "f"), "]\n\n"))

  }

}
