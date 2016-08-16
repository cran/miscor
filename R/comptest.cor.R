#' Comparision of product-moment correlation coefficients
#'
#' This function statistically compares product-moment correlation coefficients in
#' independent and dependent samples.
#'
#' In dependent samples, the function tests the two-sided null hypothesis H0: \eqn{\rho .xy = \rho .xz} or
#' the one-sided null hypothesis H0: \eqn{\rho .xy >= \rho .xz} or \eqn{\rho .xy <= \rho .xz}. Function parameters
#' are specified using either (\code{x}, \code{y}, \code{z}) or (\code{r.xy}, \code{r.xz}, \code{r.yz}, \code{n}).
#' In independent samples, the function tests the two-sided null hypothesis H0: \eqn{\rho.1 = \rho.2} or
#' the one-sided null hypothesis H0: \eqn{\rho.1 >= \rho.2} or \eqn{\rho.1 <= \rho.2}. Function parameters
#' are specified using either (\code{x}, \code{y}, \code{group}) or (\code{r.1}, \code{r.2}, \code{n.1}, \code{n.2}).
#'
#' @param x           a numeric vector.
#' @param y           a numeric vector.
#' @param z           a numeric vector.
#' @param group       a numeric vector indiating the group membership.
#' @param r.xy        alternative specification, product-moment correlation coefficient between \code{x} and \code{y}.
#' @param r.xz        alternative specification, product-moment correlation coefficient between \code{x} and \code{z}.
#' @param r.yz        alternative specification, product-moment correlation coefficient between \code{y} and \code{z}.
#' @param n           alternative specification, number of observations.
#' @param r.1         alternative specification, product-moment correlation coefficient in group 1.
#' @param r.2         alternative specification, product-moment correlation coefficient in group 2.
#' @param n.1         alternative specification, number of observations in group 1.
#' @param n.2         alternative specification, number of observations in group 2.
#' @param alternative a character string describing the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param conf.level  confidence level of the interval.
#' @param digits      integer indicating the number of decimal places to be displayed.
#' @param output      logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{test.cor}}, \code{\link{seqtest.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlation. \emph{Psychological Methods, 12}, 399-413.
#'
#' @return
#' Returns an object of class \code{comptest.cor} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{dat}       \tab data.frame with x, y and z (if available) \cr
#'   \code{spec}      \tab specification of function arguments \cr
#'   \code{res}       \tab list with results depending on the analysis (independent of dependent samples),
#'                         i.e., z (test statistic), pval (significance value),
#'                         r.xy, r.xz, r.yz, r.1, r.2, diff (difference), n, n.1, n.1,
#'                         lower (lower limit of CI), upper (upper limit of CI) \cr
#' }
#'
#' @export
#'
#' @examples
#' # Dependent samples: Generate random data
#' x <- c(3, 2, 2, 3, 7, 8, 5, 9)
#' y <- c(2, 4, 1, 5, 7, 3, 6, 7)
#' z <- c(1, 4, 3, 3, 1, 4, 2, 5)
#'
#' #--------------------------------------
#' # Dependent samples
#' # H0: rho.xy == rho.xz,  H1: rho.xy != rho.xz
#'
#' comptest.cor(x, y, z)
#'
#' #--------------------------------------
#' # Dependent samples
#' # H0: rho.xy <= rho.xz,  H1: rho.xy > rho.xz
#' # r.xy = 0.44, r.xz = 0.21. r.yz = 0.20, n = 120
#'
#' comptest.cor(r.xy = 0.44, r.xz = 0.21, r.yz = 0.20, n = 120,
#'              alternative = "greater")
#'
#' ###
#'
#' # Independent samples: Generate random data
#' dat <- data.frame(group = rep(1:2, each = 200),
#'                   rbind(sim.cor(200, rho = 0.3),
#'                         sim.cor(200, rho = 0.5)))
#'
#' #--------------------------------------
#' # Independent samples
#' # H0: rho.1 == rho.2, H1: rho.1 != rho.2
#'
#' comptest.cor(x = dat$x, y = dat$y, group = dat$group)
#'
#' #--------------------------------------
#' # Independent samples
#' # H0: rho.1 >= rho.2, H1: rho.1 ! < rho.2
#' # Group 1: r = 0.32, n = 108
#' # Group 2: r = 0.56, n = 113
#'
#' comptest.cor(r.1 = 0.32, n.1 = 108, r.2 = 0.56, n.2 = 113,
#'              alternative = "less")
comptest.cor <- function(x = NULL, y = NULL, z = NULL, group = NULL, r.xy = NULL, r.xz = NULL, r.yz = NULL, n = NULL,
                         r.1 = NULL, r.2 = NULL, n.1 = NULL, n.2 = NULL, alternative = c("two.sided", "less", "greater"),
                         conf.level = 0.95, digits = 3, output = TRUE) {

  #-----------------------------------------------------------------------------------
  # Input check

  if (((!is.null(x) | !is.null(y) | !is.null(z) | !is.null(group)) &
       (!is.null(r.xy) | !is.null(r.xz) | !is.null(r.yz) | !is.null(n) | !is.null(r.1) | !is.null(r.2) | !is.null(n.1) | !is.null(n.2))) |
      (!is.null(r.xy) | !is.null(r.xz) | !is.null(r.yz) | !is.null(r.yz)) & (!is.null(r.2) | !is.null(n.1) | !is.null(n.2))) {

    stop("Specifiy function parameters using arguments (x, y, z), (x, y, group), (r.xy, r.xz, r.yz, n) or (r.1, r.2, n.1, n.2)")

  }

  ###

  if (!is.null(r.xy) | !is.null(r.xz) | !is.null(r.yz) | !is.null(r.1) | !is.null(r.2)) {

    if (any(c(r.xy, r.xz, r.yz, r.1, r.2) > 1 | c(r.xy, r.xz, r.yz, r.1, r.2) < -1)) {

      stop("Specified correlation coefficient(s) out of bounds")

    }

  }

  ###

  if (!is.null(z) & !is.null(group)) {

    stop("Specify either z (dependent sample) or group (independent sample)")

  }

  ###

  type <- ifelse(is.null(group) & is.null(r.1) & is.null(r.2), "dependent", "independent")

  #-----------------------------------------------------------------------------------
  # Main function

  # Dependent samples
  if (type == "dependent") {

    if (is.null(r.xy) & is.null(r.xz) & is.null(r.yz)) {

      dat <- data.frame(na.omit(cbind(x, y, z)))

      r.xy <- cor(dat$x, dat$y)
      r.xz <- cor(dat$x, dat$z)
      r.yz <- cor(dat$y, dat$z)

      n <- nrow(dat)

    }

    ###

    if (n < 4) {

      stop("Number of observations smaller than n = 4")

    }

    ###

    z.r.xy <- 0.5 * log((1 + r.xy) / (1 - r.xy))
    z.r.xz <- 0.5 * log((1 + r.xz) / (1 - r.xz))

    r.1 <- (r.xy + r.xz) / 2

    cov.r <- 1 / ((1 - r.1^2)^2)  * (r.yz * (1 - 2 * r.1^2) - 0.5 * r.1^2 * (1 - 2 * r.1^2 - r.yz^2))

    se <- sqrt((2 - 2*cov.r) / (n - 3))

    z.diff <- (z.r.xy - z.r.xz) / se

    ###

    r.xy.conf <- conf.cor(r = r.xy, n = n, conf.level = conf.level, output = FALSE)$res
    r.xz.conf <- conf.cor(r = r.xz, n = n, conf.level = conf.level, output = FALSE)$res

    cor.r <- ((r.yz - 0.5 * r.xy * r.xz) * (1 - r.xy^2 - r.xz^2 - r.yz^2) + r.yz^3) /
             ((1 - r.xy^2) * (1 - r.xz^2))

    lower <- r.xy - r.xz - sqrt((r.xy - r.xy.conf$lower)^2 + (r.xz.conf$upper - r.xz)^2 -
                                2 * cor.r * (r.xy - r.xy.conf$lower) * (r.xz.conf$upper - r.xz))

    upper <- r.xy - r.xz + sqrt((r.xy.conf$upper - r.xy)^2 + (r.xz - r.xz.conf$lower)^2 -
                                2* cor.r * (r.xy.conf$upper - r.xy) * (r.xz - r.xz.conf$lower))

  # Independent samples
  } else {

    if (is.null(r.1) & is.null(r.2) & is.null(n.1) & is.null(n.2)) {

      dat <- apply(cbind(x, y), 2, function(x) split(x, group))

      r.1 <- cor(dat$x[[1]], dat$y[[1]], "complete.obs")
      n.1 <- nrow(na.omit(cbind(dat$x[[1]], dat$y[[1]])))

      r.2 <- cor(dat$x[[2]], dat$y[[2]], "complete.obs")
      n.2 <- nrow(na.omit(cbind(dat$x[[2]], dat$y[[2]])))

    }

    ###

    if (any(c(n.1, n.2) < 4)) {

      stop("Number of observations smaller than n = 4")

    }

    ###

    z.r.1 <- 0.5 * log((1 + r.1) / (1 - r.1))
    z.r.2 <- 0.5 * log((1 + r.2) / (1 - r.2))

    se <- sqrt(1 / (n.1 - 3) + 1 / (n.2 - 3))

    z.diff <- (z.r.1 - z.r.2) / se

    ###

    r.1.conf <- conf.cor(r = r.1, n = n.1, output = FALSE)$res
    r.2.conf <- conf.cor(r = r.2, n = n.2, output = FALSE)$res

    lower <- r.1 - r.2 - sqrt((r.1 - r.1.conf$lower)^2 + (r.2.conf$upper - r.2)^2)
    upper <- r.1 - r.2 + sqrt((r.1.conf$upper - r.1)^2 + (r.2 - r.2.conf$lower)^2)

  }

  ###

  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  if (alternative == "two.sided") {

    pval <- pnorm(abs(z.diff), lower.tail = FALSE) * 2

  }

  if (alternative == "greater") {

    pval <- ifelse(z.diff < 0, 1, pnorm(z.diff, lower.tail = FALSE))

  }

  if (alternative == "less") {

    pval <- ifelse(z.diff > 0, 1, pnorm(z.diff, lower.tail = TRUE))

  }

  #-----------------------------------------------------------------------------------
  # Return object

  if (type == "dependent") {

    object <- list(call = match.call(),
                   dat = data.frame(x, y, z),
                   spec = list(type = "dependent", alternative = alternative, conf.level = conf.level, digits = digits),
                   res = list(z = z.diff, pval = pval, r.xy = r.xy, r.xz = r.xz, r.yz = r.yz, diff = r.xy - r.xz, n = n,
                              lower = lower, upper = upper))

  } else {

    object <- list(call = match.call(),
                   dat = data.frame(x, y, group),
                   spec = list(type = "independent", alternative = alternative, conf.level = conf.level, digits = digits),
                   res = list(z = z.diff, pval = pval, r.1 = r.1, n.1 = n.1, r.2 = r.2, n.2 = n.2, diff = r.1 - r.2,
                              lower = lower, upper = upper))

  }

  class(object) <- "comptest.cor"

  #-----------------------------------------------------------------------------------
  # Output

  if (output == TRUE) { print(object) }

  return(invisible(object))

}
