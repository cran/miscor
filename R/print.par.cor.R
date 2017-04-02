#' Print par.cor
#'
#' This function prints the \code{par.cor} object
#'
#' @param x           \code{par.cor} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{par.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @method print par.cor
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(4, 6, 8, 8, 9, 4),
#'                   y = c(3, 7, 9, 8, 9, 3),
#'                   z = c(1, 3, 4, 4, 5, 2))
#'
#' #--------------------------------------
#' # Partial correlation
#'
#' obj <- par.cor(dat$x, dat$y, p.xy = dat$z, output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Semipartial correlation
#' # remove z from x
#'
#' obj <- par.cor(dat$x, dat$y, p.x = dat$z, output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Semipartial correlation
#' # remove z from y
#'
#' obj <- par.cor(dat$x, dat$y, p.y = dat$y, output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Partial correlation: Two-sided test
#' # H0: rho.p == 0, H1: rho.p != 0
#'
#' obj <- par.cor(dat$x, dat$y, p.xy = dat$z, sig = TRUE,
#'                output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # Partial correlation: One-sided test
#' # H0: rho.p <= 0.2, H1: rho.p > 0.2
#'
#' obj <- par.cor(dat$x, dat$y, p.xy = dat$z, sig = TRUE,
#'                rho0 = 0.4, alternative = "less", output = FALSE)
#' print(obj)
print.par.cor <- function(x, ...) {

  #-----------------------------------------------------------------------------------
  # Main function

  if (x$spec$sig == TRUE) {

    if (!is.null(x$dat$p.xy)) { cat("\nStatistical test for the partial correlation coefficient\n\n") } else {
                                cat("\nStatistical test for the semipartial correlation coefficient\n\n") }

    ###

    if (x$spec$alternative == "two.sided") {

      cat("  H0: rho.p ==", x$spec$rho0, " versus  H1: rho.p !=", x$spec$rho0, "\n")

    }

    if (x$spec$alternative == "greater") {

      cat("  H0: rho.p <=", x$spec$rho0, " versus  H1: rho.p >", x$spec$rho0, "\n")

    }

    if (x$spec$alternative == "less") {

      cat("  H0: rho.p >=", x$spec$rho0, " versus  H1: rho.p <", x$spec$rho0, "\n")

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

    cat(ifelse(x$res$lower < 0, "\n\n  Sample estimate r.p:     ", "\n\n  Sample estimate r.p:    "),
        formatC(x$res$r.p, digits = x$spec$digits, format = "f"), "\n",
        paste0(ifelse(x$spec$alternative == "two.sided", "    Two-sided ", "    One-sided "), x$spec$conf.level * 100, "% CI: [",
               formatC(x$res$lower, digits = x$spec$digits, format = "f"), ", ",
               formatC(x$res$upper, digits = x$spec$digits, format = "f"), "]\n\n"))

  } else {

    print(x$res$r.p)

  }

}
