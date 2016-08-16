#' Print cor.conf
#'
#' This function prints the \code{cor.conf} object
#'
#' @param x           \code{cor.conf} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{conf.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Kubinger, K. D., Rasch, D., & Simeckova, M. (2007). Testing a correlation coefficient's significance:
#' Using H0: 0 \eqn{< \rho \le \lambda} is preferable to H0: \eqn{\rho = }0. \emph{Psychology Science, 49}, 74-87.
#'
#' @method print conf.cor
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Two-sided 95% Confidence Interval
#' # r = 0.55, n = 120
#'
#' obj <- conf.cor(r = 0.55, n = 120, output = FALSE)
#' print(obj)
#'
#' #--------------------------------------
#' # One-sided 99% Confidence Interval
#'
#' # Generate random data
#' dat <- sim.cor(100, rho = 0.4)
#'
#' obj <- conf.cor(dat$x, dat$y, conf.level = 0.99, alternative = "less",
#'                output = FALSE)
#' print(obj)
print.conf.cor <- function(x, ...) {

  #-----------------------------------------------------------------------------------
  # Main function
  cat("\nProduct-moment correlation coefficient\n\n",

      ifelse(x$res$lower < 0, " Sample estimate r:     ", " Sample estimate r:    "),
      formatC(x$res$r, digits = x$spec$digits, format = "f"), "\n",
      paste0(ifelse(x$spec$alternative == "two.sided", "  Two-sided ", "  One-sided "), x$spec$conf.level * 100,
             ifelse(nchar(x$spec$conf * 100) == 2, "% CI: [", "% CI: ["),
             formatC(x$res$lower, digits = x$spec$digits, format = "f"), ", ", formatC(x$res$upper, digits = x$spec$digits, format = "f"), "]\n\n"))

  #-----------------------------------------------------------------------------------

}
