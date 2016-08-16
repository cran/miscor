#' Simulate bivariate distribution with a specified correlation
#'
#' This function simulates bivariate distribution with correaltion equal to \code{rho}, mean equal to \code{mean},
#' standard deviation equal to \code{sd}, skewness equal to \code{skewness}, and kurtosis equal to \code{kurtosis}
#' by Fleishman polynomials.
#'
#' @param n          number of observations.
#' @param rho        correlation.
#' @param mean       mean vector.
#' @param sd         standard deviation vector.
#' @param skewness   skewness vector.
#' @param kurtosis   kurtosis vector.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{test.cor}}, \code{\link{seqtest.cor}}, \code{\link{comptest.cor}},
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns a data.frame with variables \code{x} and \code{y}.
#'
#' @import PoisNonNor
#'
#' @export
#'
#' @examples
#' #------------------------------------------------
#' # Bivariate  distribution with rho = 0.3, n = 200
#' # x: skewness = 0, kurtosis = 0
#' # y: skewness = 0, kurtosis = 0
#'
#' sim.cor(200, rho = 0.3)
#'
#' #-----------------------------------------------
#' # Bivariate distribution with rho = 0.4, n = 500
#' # x: skewness = 0, kurtosis = 1.5
#' # y: skewness = 2, kurtosis = 7
#'
#' sim.cor(500, rho = 0.4, skewness = c(0, 1.5), kurtosis = c(2, 7))
sim.cor <- function(n, rho, mean = c(0, 0), sd = c(1, 1), skewness = c(0, 0), kurtosis = c(0, 0)) {

  #-----------------------------------------------------------------------------------
  # Check input

  if (rho > 1 | rho < -1) {

    stop("Specified correlation coefficient out of bounds")

  }

  ###

  if (length(mean) != 2) {

    stop("Specify a vector of length = 2 for the argument 'mean'")

  }

  ###

  if (length(sd) != 2) {

    stop("Specify a vector of length = 2 for the argument 'sd'")

  }

  ###

  if (length(skewness) != 2) {

    stop("Specify a vector of length = 2 for the argument 'skewness'")

  }

  ###

  if (length(kurtosis) != 2) {

    stop("Specify a vector of length = 2 for the argument 'kurtosis'")

  }
  #-----------------------------------------------------------------------------------
  # Main function

  # Bivariate normal distribution
  if (all(c(skewness, kurtosis) == 0)) {

    sigma <- matrix(c(sd[1], rho*sqrt(sd[1] * sd[2]), rho*sqrt(sd[1] * sd[2]), sd[2]), ncol = 2)
    tempdat <- internal.rmvnorm(n = n, mean = mean, sigma = sigma)

  # Bivariate non-normal distribution
  } else {

    # correlation matrix
    cmat <- matrix(c(1, rho, rho, 1), ncol = 2)

    # skewness and kurtosis
    rmat <- matrix(c(skewness[1], kurtosis[1], skewness[2], kurtosis[2]), byrow = TRUE, ncol = 2)

    tempdat <- PoisNonNor::RNG_P_NN(cmat = cmat, rmat = rmat, norow = n,
                        mean.vec = mean, variance.vec = sd^2)

  }

  dat <- data.frame(x = tempdat[, 1], y = tempdat[, 2])

  return(dat)

}
