##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: is.positive.definite
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.is.positive.definite <- function(m, tol, method = c("eigen", "chol")) {

  method <- match.arg(method)
  if (!is.matrix(m))
    m <- as.matrix(m)
  if (method == "eigen") {
    eval <- eigen(m, only.values = TRUE, symmetric = TRUE)$values
    if (is.complex(eval)) {
      warning("Input matrix has complex eigenvalues!")
      return(FALSE)
    }
    if (missing(tol))
      tol <- max(dim(m)) * max(abs(eval)) * .Machine$double.eps
    if (sum(eval > tol) == length(eval))
      return(TRUE)
    else return(FALSE)
  }
  if (method == "chol") {
    val = try(chol(m), silent = TRUE)
    if (class(val) == "try-error")
      return(FALSE)
    else return(TRUE)
  }
}
