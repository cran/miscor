% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.size.R
\name{print.size}
\alias{print.size}
\title{Print size}
\usage{
\method{print}{size}(x, ...)
}
\arguments{
\item{x}{\code{size} object.}

\item{...}{further arguments passed to or from other methods.}
}
\description{
This function prints the \code{size} object
}
\examples{
#--------------------------------------
# H0: rho = 0.3, H1: rho != 0.3
# alpha = 0.05, beta = 0.2, delta = 0.2

n <- size.cor(delta = 0.2, rho = 0.3, alpha = 0.05, beta = 0.2,
              output = FALSE)

print(n)
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
New York: John Wiley & Sons.
}
\seealso{
\code{\link{size.cor}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
