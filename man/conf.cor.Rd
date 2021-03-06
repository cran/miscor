% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf.cor.R
\name{conf.cor}
\alias{conf.cor}
\title{Product-moment correlation coefficient with confidence interval}
\usage{
conf.cor(x = NULL, y = NULL, r = NULL, n = NULL,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, digits = 3, output = TRUE)
}
\arguments{
\item{x}{a numeric vector.}

\item{y}{a numeric vector.}

\item{r}{alternative specification, product-moment correlation coefficient.}

\item{n}{alternative specification, number of observations.}

\item{alternative}{a character string describing the alternative hypothesis,
must be one of \code{"two.sided"} (default), \code{"greater"}
or \code{"less"}.}

\item{conf.level}{confidence level of the interval.}

\item{digits}{integer indicating the number of decimal places to be displayed.}

\item{output}{logical; if \code{TRUE}, output is shown.}
}
\value{
Returns an object of class \code{conf.cor} with following entries:

\tabular{ll}{
  \code{call}      \tab function call \cr
  \code{dat}       \tab data.frame with x and y (if available) \cr
  \code{spec}      \tab specification of function arguments \cr
  \code{res}       \tab list with results, i.e., r (correlation coefficient),
                        n, lower (lower limit of CI), upper (upper limit of CI) \cr
}
}
\description{
This function computes the product-moment correlation coefficient with two-sided or one-sided
confidence interval using Fisher's z tranformation.
}
\examples{
#--------------------------------------
# Two-sided 95\% Confidence Interval
# r = 0.55, n = 120

conf.cor(r = 0.55, n = 120)

#--------------------------------------
# One-sided 99\% Confidence Interval

# Generate random data
dat <- sim.cor(100, rho = 0.4)

conf.cor(dat$x, dat$y, conf.level = 0.99, alternative = "less")
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
New York: John Wiley & Sons.

Kubinger, K. D., Rasch, D., & Simeckova, M. (2007). Testing a correlation coefficient's significance:
Using H0: 0 \eqn{< \rho \le \lambda} is preferable to H0: \eqn{\rho = }0. \emph{Psychology Science, 49}, 74-87.
}
\seealso{
\code{\link{test.cor}}, \code{\link{seqtest.cor}}, \code{\link{comptest.cor}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at},
}
