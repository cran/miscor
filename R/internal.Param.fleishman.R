##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: Param.fleishman
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.Param.fleishman <- function(rmat) {

  if (dim(rmat)[2] != 2) {
    stop("column of rmat must be 2 \n")
  }

  if (sum(rmat[, 2] >= (rmat[, 1]^2 - 2)) < dim(rmat)[1]) {
    stop("Specified skewness and kurtosis parameter should be v2>=v1^2-2 \n")
  }

  internal.fleishman.roots <- function (p, r) {

    f <- rep(NA, length(p))
    f[1] <- p[1]^2 + 6 * p[1] * p[3] + 2 * p[2]^2 + 15 * p[3]^2 - 1
    f[2] <- 2 * p[2] * (p[1]^2 + 24 * p[1] * p[3] + 105 * p[3]^2 + 2) - r[1]
    f[3] <- p[1] * p[3] + p[2]^2 * (1 + p[1]^2 + 28 * p[1] * p[3]) +
            p[3]^2 * (12 + 48 * p[1] * p[3] + 141 * p[2]^2 + 225 * p[3]^2) - r[2]/24

    return(f)

  }

  pmat <- matrix(NA, nrow = dim(rmat)[1], ncol = 3)
  for (i in 1:nrow(rmat)) {
    pmat[i, ] <- internal.BBsolve(par = rep(0, 3), fn = internal.fleishman.roots,
                                  r = rmat[i, ])$par
  }
  pmat <- as.matrix(cbind(-pmat[, 2], pmat))
  return(pmat)
}
