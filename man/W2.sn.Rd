\name{W2.sn}
\alias{W2.sn}
\title{ Observed Kolmogorov-Smirnov statistic W2 and aproximate prior predictive p-value }
\description{ This function calculates the Kolmogorov-Smirnov statistic in an observed data set and approximate the prior predictive p-value comparing the statistic with the approximated quantiles contained in \code{W2.quantiles}.  }
\usage{ W2.sn(data) }
\arguments{ \item{data}{ a numeric vector with the observed data } }
\details{ First, this function calculates the statistic W2 on the observed data, after this value is compared with the appropiate row of the matrix \code{W2.quantiles} that provides critical quantiles of the prior predictive distribution \code{m(t)} for the EDF stadistic W2 and different sample sizes. }
\value{ \item{x }{ a numeric value with the observed stadistic W2 and the p-value associated. } }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain \email{v.paton@alumnos.urjc.es} }
\seealso{ \code{\link{W2.sn}} }
\examples{
data(ais, package="sn")
attach(ais)
data=bmi
data(prior.lambda)
data(W2.quantiles)
W2.sn(data)
}
\keyword{ distribution }

