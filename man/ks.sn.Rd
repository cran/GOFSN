\name{ks.sn}
\alias{ks.sn}
\title{ Observed Kolmogorov-Smirnov statistic D and aproximate prior predictive p-value }
\description{ This function calculates the Kolmogorov-Smirnov statistic in an observed data set and approximate the prior predictive p-value comparing the statistic with the approximated quantiles contained in \code{D.quantiles}.  }
\usage{ ks.sn(data) }
\arguments{ \item{data}{ a numeric vector with the observed data } }
\details{ First, this function calculates the statistic D on the observed data, after this value is compared with the appropiate row of the matrix \code{D.quantiles} that provides critical quantiles of the prior predictive distribution \code{m(t)} for the EDF stadistic D and different sample sizes. }
\value{ \item{x }{ a numeric value with the observed stadistic D and the p-value associated. } }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain \email{v.paton@alumnos.urjc.es} }
\seealso{ \code{\link{W2.sn}} }
\examples{
data(ais, package="sn")
attach(ais)
data=bmi
data(prior.lambda)
data(D.quantiles)
ks.sn(data)
}
\keyword{ distribution }

