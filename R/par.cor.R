#' Partial and semipartial correlation
#'
#' This function computes the partial or semipartial correlation coefficient between two vaiables.
#' In addition, this function can test the partial or semipartial correlation coefficient for
#' H0: \eqn{\rho .p = \rho}0, so that any value for \eqn{\rho}0 can be specified.
#'
#' Partial correlation is the correlation of \code{x} and \code{y} while statistically controlling
#' for third variables specified in the argument \code{p.xy}. These variables are residualized from
#' \code{x} and \code{y} using (multiple) regression models.
#' Semipartial correlation is the correlation of \code{x} and \code{y} while statistically controlling
#' for third variables only for \code{x} (specified in the argument \code{p.x}) or \code{y} (specified
#' in the argument \code{p.y}). These variables are residualized from \code{x} or \code{y} using a
#' (multiple) regression model.
#'
#' @param x           a numeric vector.
#' @param y           a numeric vector.
#' @param p.xy        a numeric vector or data.frame, varialbe(s) residualized from x and y.
#' @param p.x         a numeric vector or data.frame, varialbe(s) residualized only from x.
#' @param p.y         a numeric vector or data.frame, varialbe(s) residualized only from y.
#' @param sig         logical: if \code{TRUE}, statistical significance test is conducted.
#' @param rho        a number indicating \eqn{\rho}0, the value under the null hypothesis.
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
#' \code{\link{test.cor}}, \code{\link{conf.cor}}, \code{\link{comptest.cor}}, \code{\link{seqtest.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{par.cor} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{dat}       \tab list with data for x.resid (x residualized), y.resid (y residualized),
#'                          x, y, p.xy, p.y, and p.x \cr
#'   \code{spec}      \tab specification of function argument method \cr
#'   \code{res}       \tab list with results, i.e., t or z (test statistic), df (degree of freedom)
#'                         pval (significance value), r.p (partial or semipartial correlation coefficient),
#'                         n (sample size), lower (lower limit of CI), upper (upper limit of CI) \cr
#' }
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
#' par.cor(dat$x, dat$y, p.xy = dat$z)
#'
#' #--------------------------------------
#' # Semipartial correlation
#' # remove z from x
#'
#' par.cor(dat$x, dat$y, p.x = dat$z)
#'
#' #--------------------------------------
#' # Semipartial correlation
#' # remove z from y
#'
#' par.cor(dat$x, dat$y, p.y = dat$y)
#'
#' #--------------------------------------
#' # Partial correlation: Two-sided test
#' # H0: rho.p == 0, H1: rho.p != 0
#'
#' par.cor(dat$x, dat$y, p.xy = dat$z, sig = TRUE)
#'
#' #--------------------------------------
#' # Partial correlation: One-sided test
#' # H0: rho.p <= 0.2, H1: rho.p > 0.2
#'
#' par.cor(dat$x, dat$y, p.xy = dat$z,
#'         sig = TRUE, rho = 0.4, alternative = "less")
par.cor <- function(x = NULL, y = NULL, p.xy = NULL, p.x = NULL, p.y = NULL,
                    sig = FALSE, rho = 0, alternative = c("two.sided", "less", "greater"),
                    reduced = FALSE, conf.level = 0.95, digits = 3, output = TRUE) {

  #-----------------------------------------------------------------------------------
  # Check input

  if (is.null(p.xy) & is.null(p.x) & is.null(p.y)) {

    stop("Specify argument p.xy, p.x, or p.y")

  }

  ###

  if (rho >= 1 | rho <= -1) {

    stop("Specified value under the null hypothesis rho out of bounds")

  }

  #-----------------------------------------------------------------------------------
  # Main function

  #...........
  # x-variable

  if (!is.null(p.xy) | !is.null(p.x)) {

    # Only one variable selected
    if (is.vector(p.xy)) { dat.p.xy <- data.frame(z1.x = p.xy) } else { dat.p.xy <- p.xy }
    if (is.vector(p.x)) { dat.p.x <- data.frame(z2.x = p.x) } else { dat.p.x <- p.x }

    pred.x <- union(names(dat.p.xy), names(dat.p.x))

    # p.xy specified
    if (!is.null(dat.p.xy) & is.null(dat.p.x)) {

      dat.x <- data.frame(crit.x = x, dat.p.xy, stringsAsFactors = FALSE)

    }

    # p.x specified
    if (is.null(dat.p.xy) & !is.null(dat.p.x)) {

      dat.x <- data.frame(crit.x = x, dat.p.x, stringsAsFactors = FALSE)

    }

    # p.xy and p.x specified
    if (!is.null(dat.p.xy) & !is.null(dat.p.x)) {

      dat.x <- data.frame(crit.x = x, dat.p.x, dat.p.x, stringsAsFactors = FALSE)

    }

    x.resid <- resid(eval(parse(text = paste0("lm(crit.x ~ ", paste(pred.x, collapse = " + "), ", data = dat.x)"))))

  } else {

    x.resid <- x

  }


  #...........
  # y-variable

  if (!is.null(p.xy) | !is.null(p.y)) {

    # Only one variable selected
    if (is.vector(p.xy)) { dat.p.xy <- data.frame(z1.y = p.xy) } else { dat.p.xy <- p.xy }
    if (is.vector(p.y)) { dat.p.y <- data.frame(z2.y = p.y) } else { dat.p.y <- p.y }

    pred.y <- union(names(dat.p.xy), names(dat.p.y))

    # p.xy specified
    if (!is.null(dat.p.xy) & is.null(dat.p.y)) {

      dat.y <- data.frame(crit.y = y, dat.p.xy, stringsAsFactors = FALSE)

    }

    # p.y specified
    if (is.null(dat.p.xy) & !is.null(dat.p.y)) {

      dat.y <- data.frame(crit.y = y, dat.p.y, stringsAsFactors = FALSE)

    }

    # p.xy and p.y specified
    if (!is.null(dat.p.xy) & !is.null(dat.p.y)) {

      dat.y <- data.frame(crit.y = y, dat.p.y, dat.p.y, stringsAsFactors = FALSE)

    }

    y.resid <- resid(eval(parse(text = paste0("lm(crit.y ~ ", paste(pred.y, collapse = " + "), ", data = dat.y)"))))

  } else  {

    y.resid <- y

  }

  ###

  # Test for statistical significance
  if (sig == TRUE) {

    obj.test.cor <- test.cor(x = x.resid, y = y.resid, rho = rho, alternative = alternative,
                             reduced = reduced, conf.level = conf.level , digits = digits, output = FALSE)

  } else {

    r.p <- cor(x.resid, y.resid, use = "complete.obs")

  }

  #-----------------------------------------------------------------------------------
  # Return object

  if (sig == TRUE) {

    if (rho == 0) {

      object <- list(call = match.call(),
                     dat = list(x.resid = x.resid, y.resid = y.resid,
                                x = x, y = y, p.xy = p.xy, p.x = p.x, p.y = p.y),
                     spec = list(sig = sig, rho = rho, alternative = obj.test.cor$spec$alternative, reduced = reduced,
                                 conf.level = conf.level, digits = digits),
                     res = list(t = obj.test.cor$res$t, df = obj.test.cor$res$df,
                                pval = obj.test.cor$res$pval, r.p = obj.test.cor$res$r, n = obj.test.cor$res$n,
                                lower = obj.test.cor$res$lower, upper = obj.test.cor$res$upper))

    } else {

      object <- list(call = match.call(),
                     dat = list(x.resid = x.resid, y.resid = y.resid,
                                x = x, y = y, p.xy = p.xy, p.x = p.x, p.y = p.y),
                     spec = list(sig = sig, rho = rho, alternative = obj.test.cor$spec$alternative, reduced = reduced,
                                 conf.level = conf.level, digits = digits),
                     res = list(z = obj.test.cor$res$z,
                                pval = obj.test.cor$res$pval, r.p = obj.test.cor$res$r, n = obj.test.cor$res$n,
                                lower = obj.test.cor$res$lower, upper = obj.test.cor$res$upper))

    }

  } else {

    object <- list(call = match.call(),
                   dat = list(x.resid = x.resid, y.resid = y.resid,
                              x = x, y = y, p.xy = p.xy, p.x = p.x, p.y = p.y),
                   spec = list(sig = sig),
                   res = list(r.p = r.p))

  }

  class(object) <- "par.cor"

  #-----------------------------------------------------------------------------------
  # Output

  if (output == TRUE) { print(object) }

  return(invisible(object))

}
