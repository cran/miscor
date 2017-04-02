#' Product-moment correlation coefficient with confidence interval
#'
#' This function computes the product-moment correlation coefficient with two-sided or one-sided
#' confidence interval using Fisher's z tranformation.
#'
#' @param x           a numeric vector.
#' @param y           a numeric vector.
#' @param r           alternative specification, product-moment correlation coefficient.
#' @param n           alternative specification, number of observations.
#' @param alternative a character string describing the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  confidence level of the interval.
#' @param digits      integer indicating the number of decimal places to be displayed.
#' @param output      logical; if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{test.cor}}, \code{\link{seqtest.cor}}, \code{\link{comptest.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Kubinger, K. D., Rasch, D., & Simeckova, M. (2007). Testing a correlation coefficient's significance:
#' Using H0: 0 \eqn{< \rho \le \lambda} is preferable to H0: \eqn{\rho = }0. \emph{Psychology Science, 49}, 74-87.
#'
#' @return
#' Returns an object of class \code{conf.cor} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{dat}       \tab data.frame with x and y (if available) \cr
#'   \code{spec}      \tab specification of function arguments \cr
#'   \code{res}       \tab list with results, i.e., r (correlation coefficient),
#'                         n, lower (lower limit of CI), upper (upper limit of CI) \cr
#' }
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Two-sided 95% Confidence Interval
#' # r = 0.55, n = 120
#'
#' conf.cor(r = 0.55, n = 120)
#'
#' #--------------------------------------
#' # One-sided 99% Confidence Interval
#'
#' # Generate random data
#' dat <- sim.cor(100, rho = 0.4)
#'
#' conf.cor(dat$x, dat$y, conf.level = 0.99, alternative = "less")
conf.cor <- function(x = NULL, y = NULL, r = NULL, n = NULL, alternative = c("two.sided", "less", "greater"),
                     conf.level = 0.95, digits = 3, output = TRUE) {

  #-----------------------------------------------------------------------------------
  # Check input

  if ((!is.null(x) | !is.null(y)) & (!is.null(r) | !is.null(n))) {

    stop("Specifiy function parameters using arguments (x, y) or (r, n)")

  }

  ###

  if (!is.null(r)) {

    if (r > 1 | r < -1) {

      stop("Specified correlation coefficient out of bounds")

    }

  }

  #-----------------------------------------------------------------------------------
  # Main function

  if (!is.null(x) & !is.null(y)) {

    r <- cor(x, y, "complete.obs")
    n <- nrow(na.omit(cbind(x, y)))

  }

  ###

  if (n < 4) {

    stop("Number of observations smaller than n = 4")

  }

  ###

  z.r <- 0.5 * log((1 + r) / (1 - r))

  ###

  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  # Two-sided
  if (alternative == "two.sided") {

    z.q <- qnorm(1 - (1 - conf.level) / 2)

    se <- sqrt(n - 3)

    lo.z <- z.r - z.q / se
    up.z <- z.r + z.q / se

    lower <- (exp(2 * lo.z) - 1) / (exp(2 * lo.z) + 1)
    upper <- (exp(2 * up.z) - 1) / (exp(2 * up.z) + 1)

  # One-sided
  } else {

    z.q <- qnorm(1 - (1 - conf.level))

    se <- sqrt(n - 3)

    lo.z <- z.r - z.q / se
    up.z <- z.r + z.q / se

    if (alternative == "less") {

      lower <- (exp(2 * lo.z) - 1) / (exp(2 * lo.z) + 1)
      upper <- 1.000

    } else {

      lower <- -1.000
      upper <- (exp(2 * up.z) - 1) / (exp(2 * up.z) + 1)

    }

  }

  #-----------------------------------------------------------------------------------
  # Return object

  object <- list(call = match.call(),
                 dat = data.frame(x, y),
                 spec = list(alternative = alternative, conf.level = conf.level, digits = digits),
                 res = list(r = r, n = n, lower = lower, upper = upper))

  class(object) <- "conf.cor"

  #-----------------------------------------------------------------------------------
  # Output

  if (output == TRUE) { print(object) }

  return(invisible(object))

}
