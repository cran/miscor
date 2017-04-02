##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: intercor.PP
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.intercor.PP <- function (lamvec, cmat) {

  if (length(lamvec) != dim(cmat)[1]) {
    stop("Correlation matrix dimension is not consistent with number of Poisson variables in lamvec!\n")
  }

  if (sum(lamvec <= 0) > 0) {
    stop("lambda should be positive \n")
  }

  norow <- 1e+05
  u <- runif(norow, 0, 1)
  corrected <- diag(1, length(lamvec))
  for (i in 2:length(lamvec)) {
    for (j in 1:(i - 1)) {
      maxcor <- cor(qpois(u, lamvec[i]), qpois(u, lamvec[j]))
      mincor <- cor(qpois(u, lamvec[i]), qpois(1 - u, lamvec[j]))
      a <- -maxcor * mincor/(maxcor + mincor)
      b <- log((maxcor + a)/a, exp(1))
      c <- -a
      corrected[i, j] <- corrected[j, i] <- log((cmat[i, j] + a)/a, exp(1))/b
      corrected[i, j] <- corrected[j, i] <- ifelse((corrected[i, j] > 1 | corrected[i, j] < (-1)), NA, corrected[i, j])
    }
  }
  return(round(corrected, 3))
}

##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: intercor.NN
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.intercor.NN <- function (pmat, cmat) {

  if (dim(pmat)[1] != dim(cmat)[1]) {
    stop("Correlation matrix dimension is not consistent with number of continous variables in pmat!\n")
  }

  if (dim(pmat)[2] != 4) {
    stop("column of pmat must be 4\n")
  }

  cmat_star <- diag(1, dim(cmat)[1])
  k <- 1
  for (i in 2:dim(cmat)[1]) {
    for (j in 1:(i - 1)) {
      z <- rep(NA, 3)
      z[1] <- pmat[i, 2] * pmat[j, 2] + 3 * pmat[i, 2] *
        pmat[j, 4] + 3 * pmat[i, 4] * pmat[j, 2] + 9 *
        pmat[i, 4] * pmat[j, 4]
      z[2] <- 2 * pmat[i, 3] * pmat[j, 3]
      z[3] <- 6 * pmat[i, 4] * pmat[j, 4]
      c_star <- polyroot(c(-cmat[i, j], z[1], z[2], z[3]))
      c_star <- c_star[abs(Im(c_star)) < 1e-10 & abs(Re(c_star)) <= 1]
      cmat_star[i, j] <- cmat_star[j, i] <- Re(c_star)
      k <- k + 1
    }
  }
  return(cmat_star)
}

##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: intercor.NNP
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.intercor.NNP <- function (lamvec, cmat, pmat) {

  if ((length(lamvec) != dim(cmat)[1]) | (dim(pmat)[1] != dim(cmat)[2])) {
    stop("Correlation matrix dimension is not consistent with number of variables!\n")
  }

  if (sum(lamvec <= 0) > 0) {
    stop("lambda should be positive \n")
  }

  if (dim(pmat)[2] != 4) {
    stop("column of pmat must be 4\n")
  }

  n1 <- length(lamvec)
  n2 <- dim(pmat)[1]
  norow <- 1e+05
  cor_NN <- matrix(NA, nrow = n1, ncol = n2)
  for (i in 1:n1) {
    for (j in 1:n2) {
      X <- rnorm(norow, 0, 1)
      Y <- rnorm(norow, 0, 1)
      U <- pnorm(X)
      Xpois <- qpois(U, lamvec[i])
      c <- cor(Xpois[order(Xpois)], Y[order(Y)])/cor(X[order(X)], Y[order(Y)])
      cor_NN[i, j] <- cmat[i, j]/c/(pmat[j, 2] + 3 * pmat[j, 4])
    }
  }
  return(cor_NN)
}

