##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: bounds.corr.GSC.PP
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.bounds.corr.GSC.PP <- function (lamvec) {

  if (sum(lamvec <= 0) > 0) {
    stop("lambda should be positive \n")
  }

  norow <- 1e+05
  maxmat <- minmat <- diag(NA, length(lamvec))
  errorCount <- 0
  for (i in 2:length(lamvec)) {
    for (j in 1:(i - 1)) {
      Xpoisi <- rpois(norow, lamvec[i])
      Xpoisj <- rpois(norow, lamvec[j])
      max <- cor(Xpoisi[order(Xpoisi)], Xpoisj[order(Xpoisj)])
      min <- cor(Xpoisi[order(Xpoisi, decreasing = TRUE)], Xpoisj[order(Xpoisj)])
      minmat[i, j] <- minmat[j, i] <- min
      maxmat[i, j] <- maxmat[j, i] <- max
    }
  }
  return(list(min = minmat, max = maxmat))
}

##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: bounds.corr.GSC.NN
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.bounds.corr.GSC.NN <- function (pmat) {
  if (dim(pmat)[2] != 4) {
    stop("column of pmat must be 4\n")
  }

  fleishman.uni <- function(p, norow = 1e+05) {
    x <- rnorm(norow)
    X <- as.matrix(cbind(rep(1, norow), x, x^2, x^3))
    Y <- X %*% t(p)
    return(Y)
  }
  maxmat <- minmat <- diag(NA, dim(pmat)[1])
  for (i in 2:dim(pmat)[1]) {
    for (j in 1:(i - 1)) {
      Yi <- fleishman.uni(matrix(pmat[i, ], nrow = 1))
      Yj <- fleishman.uni(matrix(pmat[j, ], nrow = 1))
      max <- cor(Yi[order(Yi)], Yj[order(Yj)])
      min <- cor(Yi[order(Yi, decreasing = TRUE)], Yj[order(Yj)])
      minmat[i, j] <- minmat[j, i] <- min
      maxmat[i, j] <- maxmat[j, i] <- max
    }
  }
  return(list(min = round(minmat, 3), max = round(maxmat, 3)))
}

##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: bounds.corr.GSC.NNP
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.bounds.corr.GSC.NNP <- function (lamvec, pmat) {

  if (sum(lamvec <= 0) > 0) {
    stop("lambda should be positive \n")
  }

  if (dim(pmat)[2] != 4) {
    stop("column of pmat must be 4\n")
  }

  fleishman.uni <- function(p, norow = 1e+05) {
    x <- rnorm(norow)
    X <- as.matrix(cbind(rep(1, norow), x, x^2, x^3))
    Y <- X %*% t(p)
    return(Y)
  }

  norow <- 1e+05
  maxmat <- minmat <- matrix(NA, nrow = length(lamvec), ncol = dim(pmat)[1])
  errorCount <- 0
  for (i in 1:length(lamvec)) {
    for (j in 1:dim(pmat)[1]) {
      Xpoisi <- rpois(norow, lamvec[i])
      Yj <- fleishman.uni(matrix(pmat[j, ], nrow = 1))
      max <- cor(Xpoisi[order(Xpoisi)], Yj[order(Yj)])
      min <- cor(Xpoisi[order(Xpoisi, decreasing = TRUE)], Yj[order(Yj)])
      minmat[i, j] <- min
      maxmat[i, j] <- max
    }
  }
  return(list(min = minmat, max = maxmat))
}
