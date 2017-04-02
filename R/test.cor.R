#' Test for the product-moment correlation coefficient for H0: \eqn{\rho = \rho}0
#'
#' This function tests the product-moment correlation coefficient for H0: \eqn{\rho = \rho}0,
#' so that any value for \eqn{\rho}0 can be specified.
#'
#' Computation is based on  Fisher's z transformation \eqn{\textbf{z} = 0.5 /cdot ln(\frac{1 + r}{1 - r})}. The
#' difference between the full formula (i.e., \code{reduced = FALSE}) and the reduced formula (i.e., \code{reduced = TRUE})
#' is that the full formula includes the term \eqn{\frac{\rho}{n - 1}} in the formula of the exectation \emph{E}, i.e.,
#'
#' \deqn{\emph{E}(\textbf{z}) = 0.5 /cdot ln(\frac{1 + \rho}{1 - \rho}) + \frac{\rho}{n - 1}}
#'
#' whereas the reduced formula does not include this term, i.e.,
#'
#' \deqn{\emph{E}(\textbf{z}) = 0.5 /cdot ln(\frac{1 + \rho}{1 - \rho})}
#'
#' It is recommended to always use the full formula, especially in small samples.
#'
#' @param x           a numeric vector.
#' @param y           a numeric vector.
#' @param r           alternative specification, product-moment correlation coefficient.
#' @param n           alternative specification, number of observations.
#' @param rho0        a number indicating \eqn{\rho}0, the value under the null hypothesis.
#' @param alternative a character string describing the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param reduced     logical: if \code{TRUE}, compuatation is based on the reduced formula.
#' @param conf.level  confidence level of the interval.
#' @param digits      integer indicating the number of decimal places to be displayed.
#' @param output      logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{size.cor}}, \code{\link{comptest.cor}}, \code{\link{seqtest.cor}}
#'
#' @references
#' Cramer, H. (1946). \emph{Mathematical methods of statistics}. Princeton: Princeton Press.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Kubinger, K. D., Rasch, D., & Simeckova, M. (2007). Testing a correlation coefficient's significance:
#' Using H0: 0 \eqn{< \rho \le \lambda} is preferable to H0: \eqn{\rho = }0. \emph{Psychology Science, 49}, 74-87.
#'
#' @return
#' Returns an object of class \code{test.cor} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{dat}       \tab data.frame with x and y (if available) \cr
#'   \code{spec}      \tab specification of function arguments \cr
#'   \code{res}       \tab list with results, i.e., t or z (test statistic), df (degree of feedom),
#'                         pval (significance value), r (correlation coefficient),
#'                         n (sample size), lower (lower limit of CI), upper (upper limit of CI) \cr
#' }
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Two-sided test
#' # H0: rho == 0, H1: rho != 0
#' # r = 0.23, n = 60
#'
#' test.cor(r = 0.23, n = 120)
#'
#' #--------------------------------------
#' # Two-sided test
#' # H0: rho == 0.4, H1: rho != 0.4
#' # r = 0.55, n = 120
#'
#' test.cor(r = 0.55, n = 120, rho0 = 0.4)
#'
#' #--------------------------------------
#' # One-sided test
#' # H0: rho <= 0.4, H1: rho > 0.4
#'
#' # Generate random data
#' dat <- sim.cor(100, rho = 0.4)
#'
#' test.cor(dat$x, dat$y, rho0 = 0.4)
test.cor <- function(x = NULL, y = NULL, r = NULL, n = NULL, rho0 = 0, alternative = c("two.sided", "less", "greater"),
                     reduced = FALSE, conf.level = 0.95, digits = 3, output = TRUE) {

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

  ###

  if (rho0 >= 1 | rho0 <= -1) {

    stop("Specified value under the null hypothesis rho0 out of bounds")

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

  #...............
  # t-Test
  if (rho0 == 0) {

    statistic <- r * sqrt(n - 2) / sqrt(1 - r^2)

    df <- n - 2

  #...............
  # z-Test
  } else {

    z.r <- 0.5 * log((1 + r) / (1 - r))
    z.rho0 <- 0.5 * log((1 + rho0) / (1 - rho0)) + if (reduced == FALSE) { rho0 / (2 * (n - 1)) } else { 0 }

    statistic <- (z.r - z.rho0) * sqrt(n - 3)

  }

  ###

  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  if (alternative == "two.sided") {

    pval <- if (rho0 == 0) { pt(abs(statistic), df = df, lower.tail = FALSE)*2 } else { pnorm(abs(statistic), lower.tail = FALSE)*2 }

    res.conf <- conf.cor(r = r, n = n, alternative = "two.sided", conf.level = conf.level, output = FALSE)$res

  }

  if (alternative == "greater") {

    pval <- if (rho0 == 0) { ifelse(statistic < 0, 1, pt(statistic, df = df, lower.tail = FALSE)) } else { ifelse(statistic < 0, 1, pnorm(statistic, lower.tail = FALSE)) }

    res.conf <- conf.cor(r = r, n = n, alternative = "less", conf.level = conf.level, output = FALSE)$res

  }

  if (alternative == "less") {

    pval <- if (rho0 == 0) { ifelse(statistic > 0, 1, pt(statistic, df = df)) } else { ifelse(statistic > 0, 1, pnorm(statistic)) }

    res.conf <- conf.cor(r = r, n = n, alternative = "greater", conf.level = conf.level, output = FALSE)$res

  }

  #-----------------------------------------------------------------------------------
  # Return object

  if (rho0 == 0) {

    object <- list(call = match.call(),
                   dat = data.frame(x, y),
                   spec = list(rho0 = rho0, alternative = alternative, reduced = reduced,
                               conf.level = conf.level, digits = digits),
                   res = list(t = statistic, df = df, pval = pval, r = r, n = n,
                              lower = res.conf$lower, upper = res.conf$upper))

  } else {

    object <- list(call = match.call(),
                   dat = data.frame(x, y),
                   spec = list(rho0 = rho0, alternative = alternative, reduced = reduced,
                               conf.level = conf.level, digits = digits),
                   res = list(z = statistic, pval = pval, r = r, n = n,
                              lower = res.conf$lower, upper = res.conf$upper))

  }

  class(object) <- "test.cor"

  #-----------------------------------------------------------------------------------
  # Output

  if (output == TRUE) { print(object) }

  return(invisible(object))

}
