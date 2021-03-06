% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.seqtest.cor.R
\name{sim.seqtest.cor}
\alias{sim.seqtest.cor}
\title{Simulation of the sequential triangular test for the product-moment correlation coefficient}
\usage{
sim.seqtest.cor(rho.sim, k, rho,
                alternative = c("two.sided", "less", "greater"),
                delta, alpha = 0.05, beta = 0.1, runs = 1000,
                m.x = 0,  sd.x = 1, m.y = 0, sd.y = 1,
                digits = 3, output = TRUE, plot = FALSE)
}
\arguments{
\item{rho.sim}{simulated population correlation coefficient, \eqn{\rho}.}

\item{k}{an integer or a numerical vector indicating the number of observations in each sub-sample.}

\item{rho}{a number indicating the correlation under the null hypothesis, \eqn{\rho}0.}

\item{alternative}{a character string specifying the alternative hypothesis,}

\item{delta}{minimum difference to be detected, \eqn{\delta}.}

\item{alpha}{type-I-risk, \eqn{\alpha}.}

\item{beta}{an integer or a numerical vector indicating the type-II-risk, \eqn{\beta}.}

\item{runs}{numer of simulation runs.}

\item{m.x}{population mean of simulated vector x.}

\item{sd.x}{population standard deviation of simulated vector x.}

\item{m.y}{population mean of simulated vector y.}

\item{sd.y}{population standard deviation of simulated vector y.}

\item{digits}{integer indicating the number of decimal places to be displayed.}

\item{output}{logical: if \code{TRUE}, output is shown.}

\item{plot}{logical: if \code{TRUE}, plot is shown.}
}
\value{
Returns an object of class \code{sim.seqtest.cor} with following entries:

\tabular{ll}{
  \code{call}      \tab function call \cr
  \code{spec}      \tab specification of function arguments \cr
  \code{simres}    \tab list with results (for each k or beta) for each run \cr
  \code{res}       \tab data.frame with results, i.e., k, alpha.nom (nominal alpha),
                        alpha.emp (estimated empirical alpha), beta.nom (nominal beta),
                        beta.emp (empirica beta), p.H0 (proportion decision = H0),
                        p.H1 (proportion decision = H1), AVN (average number of V),
                        ASN (average number of sample pairs) \cr
}
}
\description{
This function performs a statistical simulation for the sequential triangular test
for the product-moment correlation coefficient
}
\details{
In order to determine the optimal k, simulation is conducted under the H0 condition, i.e., \code{rho.sim} = \code{rho}.
Simulation is carried out for a sequence of k values to seek for the optimal k where the empirical alpha is as close
as possible to the nominal alpha.
In order to determine optimal beta (with fixed k), simulation is conudcted under the H1 condition,
i.e., \code{rho.sim} = \code{rho} + \code{delta} or \code{rho.sim} = \code{rho} - \code{delta}.
Simulation is carried out for a sequencen of beta values to seek for the optimal beta where the empirical beta
is as close as possible to the nominal beta.

In order to specify a one-sided test, argument \code{alternative} has to be used (i.e., two-sided tests are conducted by default).
Specifying argument \code{alternative = "less"} conducts the simulation for the null hypothesis, H0: \eqn{\rho} >= \eqn{\rho}.0
with the alternative hypothesis, H1: \eqn{\rho} < \eqn{\rho}.0; specifying argument \code{alternative = "greater"} conducts the simluation
for the null hypothesis, H0: \eqn{\rho} <= \eqn{\rho}.0 with the alternative hypothesis, H1: \eqn{\rho} > \eqn{\rho}.0.
}
\examples{
\dontrun{
#---------------------------------------------
# Determine optimal k and nominal type-II-risk
# H0: rho <= 0.3, H1: rho > 0.3
# alpha = 0.01, beta = 0.05, delta = 0.25

# Step 1: Determine the optimal size of subsamples (k)

sim.seqtest.cor(rho.sim = 0.3, k = seq(4, 16, by = 1), rho = 0.3,
                alternative = "greater",
                delta = 0.25, alpha = 0.05, beta = 0.05,
                runs = 10000, plot = TRUE)

# Step 2: Determine the optimal nominal type-II-risk based on
#         the optimal size of subsamples (k) from step 1

sim.seqtest.cor(rho.sim = 0.55, k = 16, rho = 0.3,
                alternative = "greater",
                delta = 0.25, alpha = 0.05, beta = seq(0.05, 0.15, by = 0.01),
                runs = 10000, plot = TRUE)
}
}
\references{
Schneider, B., Rasch, D., Kubinger, K. D., & Yanagida, T. (2015).
A Sequential triangular test of a correlation coefficient's null-hypothesis: 0 \eqn{< \rho \le \rho}0.
\emph{Statistical Papers, 56}, 689-699.
}
\seealso{
\code{\link{seqtest.cor}}, \code{\link{plot.sim.seqtest.cor}}, \code{\link{print.sim.seqtest.cor}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at},
}
